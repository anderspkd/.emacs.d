(defvar old-file-name-handler-alist file-name-handler-alist)
(defvar preferred-font "DejaVu Sans Mono")

(setq gc-cons-threshold 64000000
      auto-save-list-file-prefix nil
      package-enable-at-startup nil
      package--init-file-ensured t
      file-name-handler-alist nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(add-hook 'after-init-hook
	  #'(lambda ()
	      (setq gc-cons-threshold 800000
		    file-name-handler-alist old-file-name-handler-alist)))

(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (package-initialize)
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

(use-package bind-key)
(use-package diminish)
;; wack!
(use-package use-package :commands use-package-autoload-keymap)

(setf (symbol-function 'x-focus-frame) #'ignore)

(eval-and-compile
  (defsubst emacs-dir (&optional file-name)
    (expand-file-name (concat user-emacs-directory file-name)))

  (defsubst personal-file (file-name)
    (emacs-dir (concat "personal/" file-name)))

  ;; load custom directory and folder name variables
  (load-file (personal-file "folders"))

  (setq custom-file (emacs-dir "custom.el"))
  (load custom-file)

  (add-to-list 'load-path (emacs-dir "etc")))

(eval-and-compile

  ;; Remove toolbar, menu etc...
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (electric-pair-mode t)

  (setq initial-scratch-message nil
	ring-bell-function 'ignore
	inhibit-startup-screen t
	scroll-step 1)

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-emacs 'y-or-n-p)

  (setq backup-by-copying t
	backup-directory-alist `(("." . ,asd::folders::emacs-backups))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t))

;;; Functions

(eval-and-compile

  (defvar trim-ws-modes nil
    "Modes in which whitespace should automatically be trimmed")

  (defun trim-ws-in-mode (mode &optional no-trim)
    "Automatically trim ws in MODE. if NO-TRIM is non-nil, then
MODE disable ws trimming."
    (let ((already-present (memq mode trim-ws-modes)))
      (if no-trim
	  (when already-present
	    (let (temp)
	      (dolist (m trim-ws-modes temp)
		(unless (eq m mode)
		  (push m temp)))
	      (setq trim-ws-modes temp)))
	(unless already-present
	  (push mode trim-ws-modes)))))

  (defun conditional-trim-ws ()
    (when (memq major-mode trim-ws-modes)
      (delete-trailing-whitespace)))

  (add-hook 'before-save-hook 'conditional-trim-ws t)

  (defsubst byte-recompile-dotemacs-dir ()
    (interactive)
    (byte-recompile-directory (emacs-dir) 0 t))

  (defsubst recreate-scratch ()
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))

  (defun kill-all-buffers-and-reopen-scratch ()
    (interactive)
    (when (y-or-n-p "Kill all buffers? ")
      (mapc 'kill-buffer (buffer-list))
      (delete-other-windows)
      (recreate-scratch)))

  (defun back-to-indentation-or-beginning ()
    (interactive)
    (when (= (point)
	     (progn (back-to-indentation) (point)))
      (beginning-of-line)))

  (defsubst open-file-or-thing-in-mpv (file-or-thing)
    (when (stringp file-or-thing)
      (start-process "mpv-emacs" nil "mpv" file-or-thing)))

  (defun quick-find-directory (directory)
    (interactive
     (let* ((hist (mapcar 'car asd::folders::quick-dirs))
	    (choice (completing-read "directory: " hist)))
       (list (cdr (assoc choice asd::folders::quick-dirs)))))
    (dired directory))

  (defun open-todos ()
    (interactive)
    (org-agenda nil "c"))

  (defun add-number-to-number-at-point (x)
  (interactive "n")
  (let ((y (thing-at-point 'number)))
    (unless y
      (error "no number at point"))
    (save-excursion
      (let ((bounds
	     (when (thing-at-point-looking-at "-?[0-9]+\\.?[0-9]*" 500)
	       (cons (match-beginning 0) (match-end 0)))))
	(kill-region (car bounds) (cdr bounds))
	(insert (number-to-string (+ y x))))))))

;;; Keys and movement

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(bind-key "C-x K" 'kill-all-buffers-and-reopen-scratch)
(bind-key "C-a" 'back-to-indentation-or-beginning)
(bind-key "C-c q" 'quick-find-directory)
(bind-key "C-c i" 'imenu-list)
(bind-key "C-c :" 'avy-goto-line)
(bind-key "C-c ." 'avy-goto-char-2)

(use-package hydra :ensure t)

(use-package transpose-frame
  :bind ("C-c f" . hydra-flop-frame/body)
  :config
  (defhydra hydra-flop-frame (:hint nil)
    "
Capitalization is the inverse; e.g., flip is vertical, flop is horizontal.
(_s_)wap, (_f_)lip, (_F_)flop, (_r_)otate, (_R_)otate, (_q_)uit."
    ("s" transpose-frame)
    ("f" flip-frame)
    ("F" flop-frame)
    ("r" rotate-frame-clockwise)
    ("R" rotate-frame-anticlockwise)
    ("q" nil)))

(defhydra hydra-resize-windows (global-map "C-c r")
  "Resize buffer"
  ("h" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window 3 t))))
  ("l" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window -3 t))))
  ("j" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window -3))))
  ("k" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window 3))))
  ("q" nil))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (lambda () (interactive) (text-scale-adjust 0))))

  (defhydra hydra-outline (:hint nil)
  "
Outline mode shortcuts:
_F_/_B_ (_f_/_b_): Forward/backward same level (heading)
_h_/_s_: hide/show entry
_q_:quit
"
  ("F" outline-forward-same-level)
  ("B" outline-backward-same-level)
  ("f" outline-next-heading)
  ("b" outline-previous-heading)
  ("h" outline-hide-entry)
  ("s" outline-show-entry)
  ("q" nil))

;;; Other packages

(use-package recentf
  :defer 5
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (setq recentf-auto-cleanup 'never)
  :config
  (mapc #'(lambda (path) (add-to-list 'recentf-exclude path))
	(list ".*\\.synctex\\.gz\\'"
	      ".*\\.aux\\'"
	      (expand-file-name "~/.elfeed/index")
	      custom-file
	      (emacs-dir "elpa/.*")
	      (emacs-dir "recentf")))
  (recentf-mode t))

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(use-package nlinum
  :defer t)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-reload-all))

(use-package tex
  :mode ("\\.tex\\'" . tex-mode)
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
  	    (lambda () (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook (lambda () (setq fill-column 80)))
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  :config
  (setq TeX-source-correlate-method-active 'synctex
	TeX-electric-sub-and-superscript t
	TeX-auto-save t
	TeX-parse-self t
	TeX-source-correlate-start-server t
	ispell-list-command "--list")
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t
	reftex-ref-style-default-list '("Default" "Hyperref")))

(defun insert-header-guard (&optional prefix)
  "Insert a C/C++ style header guard with a name derived from the
filename. If `prefix' is supplied, prompt for a string which is
prepended to the guard."
  (interactive
   (list (when current-prefix-arg (read-string "prefix: "))))
  (let ((ext (file-name-extension (buffer-file-name))))
    (let ((is-cpp (string= ext "hpp"))
	  (is-c (string= ext "h")))
      (unless (or is-cpp is-c)
	(error "Not in a c or c++ header file ... "))
      (let* ((filename (file-name-base (buffer-file-name)))
	     guard)
	(when (stringp filename)
	  (if prefix
	      (setq guard (format "_%s_%s_%s" (upcase prefix) (upcase filename) (upcase ext)))
	    (setq guard (format "_%s_%s" (upcase filename) (upcase ext))))
	  (insert (format "#ifndef %s\n" guard))
	  (insert (format "#define %s\n\n" guard))
	  (forward-line)
	  (save-excursion
	    (goto-char (point-max))
	    (insert (format "\n\n#endif %s" (if is-cpp (format "// %s" guard)
					      (format "/* %s */" guard))))))))))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
	 ("\\.h\\'" . c-mode))
  :bind (:map c-mode-map
	      ([ret] . newline-and-indent)
	      ("C-c h" . insert-header-guard)
	      ("C-c C-c" . compile))
  :init
  (trim-ws-in-mode 'c-mode)
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux")))
  ;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  )

(use-package c++-mode
  :ensure nil
  :mode "\\.cpp\\'"
  :bind (:map c++-mode-map
	      ([ret] . newline-and-indent)
	      ("C-c h" . insert-header-guard))
  :init
  (trim-ws-in-mode 'c++-mode)
  (defconst cpp-no-ns-indent
    '("linux" (c-offsets-alist . ((innamespace . [0])
				  (inlambda . 0)))))
  (c-add-style "cpp-no-ns-indent" cpp-no-ns-indent)
  (add-hook 'c++-mode-hook (lambda ()
			     (c-set-style "cpp-no-ns-indent")
			     (setq c-basic-offset 4))))

(use-package python
  :mode (("\\.py\\'" . python-mode)
	 ("\\.mpc\\'". python-mode))
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
	      ("C-c b t" . hs-toggle-hiding)
	      ("C-." . dabbrev-expand))
  :init
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'nlinum-mode)
  (trim-ws-in-mode 'python-mode)
  :config
  ;; (elpy-enable)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
	python-indent-offset 4))

(use-package emacs-lisp-mode
  :ensure nil  ; already present
  :mode "\\.el\\'"
  :init
  (trim-ws-in-mode 'emacs-lisp-mode))

(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (trim-ws-in-mode 'lisp-mode)
  :config
  (require 'slime-autoloads)
  (add-to-list 'slime-contribs 'slime-fancy))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.php\\'" . web-mode)
	 ("\\.css\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook 'auto-revert-mode))

;; C-x C-q is very handy, but I keep forgetting that it exists lol.
(use-package dired
  :ensure nil  ; dired is already installed by default
  :defer t
  :bind (:map dired-mode-map
	      ([backspace] . dired-up-directory)
	      ("b" . browse-url-of-dired-file)
	      ("\"" . do-shell-and-copy-to-kill-ring)
	      ("P" . emms-play-dired)
	      ("W" . dired-play-in-mpv))
  :init
  (add-hook 'dired-mode-hook 'toggle-truncate-lines)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :config
  (use-package dired-x :ensure nil)  ; so is dired-x
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-auto-revert-buffer t
	;; no "." and "..", long-list, human readable, classify, dirs first
	dired-listing-switches "-AlhF --group-directories-first")

  (defun dired-play-in-mpv ()
    (interactive)
    (let ((file (dired-get-filename)))
      (when file
	(open-file-or-thing-in-mpv file))))

  ;; https://stackoverflow.com/a/29816147
  (defun do-shell-and-copy-to-kill-ring (command &optional arg file-list)
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list
	(dired-read-shell-command "! on %s: " current-prefix-arg files)
	current-prefix-arg
	files)))
    (dired-do-shell-command command arg file-list)
    (with-current-buffer "*Shell Command Output*"
      (copy-region-as-kill (point-min) (point-max)))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c C-l" . org-insert-link))
  :init
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
  :config
  (defun org-add-timeslot ()
    (interactive)
    (let ((ts-string (with-temp-buffer (org-time-stamp nil) (buffer-string))))
      (org-set-property "WHEN" ts-string)))

  (bind-key "C-c w" #'org-add-timeslot org-mode-map)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline asd::folders::agenda-file "Tasks")
	   "* %?\n :PROPERTIES:\n :ADDED: %U\n :ANNOTATION: %a\n :END:\n %i")))
  (setq org-agenda-files (list asd::folders::agenda-file)
	org-log-reschedule t
	org-log-done t)
  (setq org-agenda-custom-commands
	'(("c" "todo+agenda" ((agenda "") (alltodo ""))))))

(use-package org-ref
  :defer 1
  :bind (("C-c p n" . org-ref-open-bibtex-notes)
	 ("C-c p b" . goto-org-ref-bib-file)
	 ("C-c p r" . helm-bibtex))
  :init
  (defvar org-ref::bib-files asd::folders::org-ref-bib-files)
  (defvar org-ref::notes-file asd::folders::org-ref-notes-files)
  (defvar org-ref::lib-path asd::folders::org-ref-lib-path)
  (defun goto-org-ref-bib-file (path)
    (interactive (list
		  (if (< 1 (length org-ref::bib-files))
		      (completing-read "bib file: " org-ref::bib-files)
		    (car org-ref::bib-files))))
    (find-file path))
  :config
  (message "loaded org-ref")
  (setq org-ref-bibliography-notes org-ref::notes-file
	org-ref-default-bibliography org-ref::bib-files
	org-ref-pdf-directory org-ref::lib-path)
  (setq bibtex-completion-bibliography org-ref::bib-files
	bibtex-completion-library-path org-ref::lib-path
	bibtex-completion-notes-path org-ref::notes-file))

(use-package elfeed
  :bind (("C-x w" . elfeed)
	 :map elfeed-search-mode-map
	 ("x" . elfeed-play-in-mpv)
	 :map elfeed-show-mode-map
	 ("w" . visual-line-mode)
	 ("c" . elfeed-save-url-to-clipboard))
  :config
  (setq shr-width 80)
  (set-face-attribute 'variable-pitch nil :family preferred-font)
  (set-face-attribute 'message-header-subject nil :family preferred-font)

  (defun elfeed-save-url-to-clipboard ()
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (message "copied to clipboard: %s" link)
      (kill-new link)))

  (defun elfeed-play-in-mpv ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (elfeed-search-untag-all 'unread)
      (open-file-or-thing-in-mpv (elfeed-entry-link entry))))

  (setq elfeed-curl-program-name "curl"
	elfeed-use-curl t
	elfeed-search-title-max-width 100)
  (setq-default elfeed-search-filter "@1-week-ago +unread -advisory ")

  ;; load settings
  (use-package elfeed-settings :ensure nil)

  (bind-key "S" 'asd-feeds-toggle-tag elfeed-search-mode-map)
  (bind-key "f" 'asd-feeds-mark-favorite elfeed-search-mode-map)
  (bind-key "F" 'asd-feeds-show-favorites elfeed-search-mode-map)

  (reload-feeds (personal-file "feeds")))

(use-package epa-file
  :ensure nil
  :config
  (epa-file-enable))

(use-package mu4e
  :ensure nil  ; installed with system package manager
  :init
  (add-hook 'message-send-hook
	    (lambda ()
	      (unless (yes-or-no-p "Send message?")
		(signal 'quit nil))))
  :config

  (use-package smtpmail :ensure nil)

  ;; defines `user-full-name' and `private-mail-*' variables used below.
  (load (personal-file "email-private"))
  (load (personal-file "email-work"))

  (add-to-list 'mu4e-view-actions
	       '("ViewInBrowser" . mu4e-action-view-in-browser))

  (add-to-list 'mu4e-bookmarks '("flag:flagged" "Flagged Messages" ?f))

  (add-hook 'mu4e-compose-mode-hook (lambda () (setq fill-column 70)))
  (setq mu4e-user-mail-address-list `(,private-mail-address ,work-mail-address))
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/mail")
  (setq mu4e-contexts
	`(,(make-mu4e-context
	    :name "Private"
	    :enter-func (lambda () (mu4e-message "Entering private context"))
	    :leave-func (lambda () (mu4e-message "Leaving private context"))
	    :match-func (lambda (msg)
			  (when msg
			    (string-match private-mail-ctx-rx
					  (mu4e-message-field msg :maildir))))
	    :vars `((user-full-name         . ,user-full-name)
		    (user-mail-address      . ,private-mail-address)
		    (mu4e-drafts-folder     . ,private-mail-drafts-folder)
		    (mu4e-sent-folder       . ,private-mail-sent-folder)
		    (mu4e-trash-folder      . ,private-mail-trash-folder)
		    (mu4e-compose-signature . ,private-mail-sig)

		    ;; SMTP settings
		    (message-send-mail-function   . smtpmail-send-it)
		    (smtpmail-stream-type         . ssl)
		    (smtpmail-default-smtp-server . ,private-mail-smtp-server)
		    (smtpmail-smtp-server         . ,private-mail-smtp-server)
		    (smtpmail-smtp-service        . ,private-mail-smtp-port)))
	  ,(make-mu4e-context
	    :name "Work"
	    :enter-func (lambda () (mu4e-message "Entering work context"))
	    :leave-func (lambda () (mu4e-message "Leaving work context"))
	    :match-func (lambda (msg)
			  (when msg
			    (string-match private-mail-ctx-rx
					  (mu4e-message-field msg :maildir))))
	    :vars `((user-full-name         . ,user-full-name)
		    (user-mail-address      . ,work-mail-address)
		    (mu4e-drafts-folder     . ,work-mail-drafts-folder)
		    (mu4e-sent-folder       . ,work-mail-sent-folder)
		    (mu4e-trash-folder      . ,work-mail-trash-folder)
		    (mu4e-compose-signature . ,work-mail-sig)

		    ;; SMTP settings
		    (message-send-mail-function   . smtpmail-send-it)
		    (smtpmail-stream-type         . starttls)
		    (smtpmail-default-smtp-server . ,work-mail-smtp-server)
		    (smtpmail-smtp-server         . ,work-mail-smtp-server)
		    (smtpmail-smtp-service        . ,work-mail-smtp-port))))))

(use-package emms
  :ensure nil
  :bind (("C-c e m" . hydra-emms-media-keys/body)
	 ("C-c e u" . emms-play-url))
  :config
  (defsubst emms-player-mpv--mute-unmute ()
    (interactive)
    (call-process-shell-command (emms-player-mpv--format-command "mute")))

  (defsubst emms-goto-music-directory ()
    (interactive)
    (when emms-source-file-default-directory
      (find-file emms-source-file-default-directory)))

  (defhydra hydra-emms-media-keys (:color amaranth :hint nil)
    "
currently playing: %s(emms-track-description (emms-playlist-current-selected-track))

^playback control^  ^^^volume control^^^    ^other^
-----------------------------------------------------
_n_: next           _k_, _+_: louder      _e_: emms list
_p_: previous       _j_, _-_: silenter    _q_: quit
_r_: random         _m_: mute ^^          _d_: music dir
_s_: stop
_SPC_: %s(if emms-player-paused-p \"play \" \"pause\")
"
    ("n" emms-next)
    ("p" emms-previous)
    ("r" emms-random)
    ("s" emms-stop)
    ("SPC" emms-pause)
    ("j" emms-volume-lower)
    ("-" emms-volume-lower)
    ("k" emms-volume-raise)
    ("+" emms-volume-raise)
    ("m" emms-player-mpv--mute-unmute)
    ("d" emms-goto-music-directory :color blue)
    ("q" nil :color blue)
    ("e" emms :color blue))

  (emms-all)
  (emms-history-load)
  (setq emms-player-list '(emms-player-mpv)
	emms-source-file-default-directory (expand-file-name "~/audio/music"))
  (add-to-list 'emms-player-mpv-parameters "--no-video")
  (add-to-list 'emms-info-functions 'emms-info-cueinfo))

(eval-and-compile
  (require 'iso-transl)
  (set-face-attribute 'default nil :family preferred-font :height 90)

  ;; fun with bitmaps :-)
  (define-fringe-bitmap 'right-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b01111100
     #b01111100
     #b00001100
     #b00001100
     #b00000000])

  (define-fringe-bitmap 'left-curly-arrow
    [#b00000000
     #b00110000
     #b00110000
     #b00111110
     #b00111110
     #b00000000
     #b00000000
     #b00000000])

  (add-hook 'prog-mode-hook (lambda () (setq fill-column 80)))

  (load-theme 'mostlyblue)
  ;; (load-theme 'mostlydark)
  )

(unless (string= (nth 1 command-line-args) "--no-server")
  (server-start))
