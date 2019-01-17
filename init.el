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
	backup-directory-alist '(("." . "~/.emacs_backups"))
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
      (start-process "mpv-emacs" nil "mpv" file-or-thing))))

;;; Keys and movement

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(bind-key "C-x K" 'kill-all-buffers-and-reopen-scratch)
(bind-key "C-a" 'back-to-indentation-or-beginning)

(bind-key "C-c i" 'imenu-list)

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

(use-package ace-window
  :bind ("C-x o" . ace-window))

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

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-annot-activate-created-annotations nil)
  (setq pdf-view-resize-factor 1.1)

  (bind-key "k" (lambda (interactive)) pdf-view-mode-map)

  (bind-key "h" 'pdf-annot-add-highlight-markup-annotation pdf-view-mode-map)
  (bind-key "t" 'pdf-annot-add-text-annotation pdf-view-mode-map)
  (bind-key "D" 'pdf-annot-delete pdf-view-mode-map)

  ;; ugly hack to change the default highlight color to LightCyan2 from yellow.
  (setq pdf-annot-default-annotation-properties
	`((t (label . ,user-full-name))
	  (text (icon . "Comment") (color . "#ff0000"))
	  (highlight (color . "LightCyan2"))
	  (squiggly (color . "orange"))
	  (strike-out (color . "red"))
	  (underline (color . "blue"))))

  ;; default movements are painfully slow
  (let ((fwd (lambda (n) (interactive "p")
	       (image-forward-hscroll (if (= n 1) 5 n))))
	(bkw (lambda (n) (interactive "p")
	       (image-backward-hscroll (if (= n 1) 5 n))))
	(dwn (lambda (n) (interactive "p")
	       (pdf-view-next-line-or-next-page (if (= n 1) 5 n))))
	(up  (lambda (n) (interactive "p")
	       (pdf-view-previous-line-or-previous-page (if (= n 1) 5 n)))))

    (bind-key "C-f"     fwd pdf-view-mode-map)
    (bind-key "<right>" fwd pdf-view-mode-map)

    (bind-key "C-b"     bkw pdf-view-mode-map)
    (bind-key "<left>"  bkw pdf-view-mode-map)

    (bind-key "C-n"     dwn pdf-view-mode-map)
    (bind-key "<down>"  dwn pdf-view-mode-map)

    (bind-key "C-p"     up  pdf-view-mode-map)
    (bind-key "<up>"    up  pdf-view-mode-map)))

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
	    (lambda () (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))
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
    (unless (or (string= ext "hpp")
		(string= ext "h"))
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
	  (insert (format "\n\n#endif /* %s */" guard)))))))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
	 ("\\.h\\'" . c-mode))
  :bind (:map c-mode-map
	      ([ret] . newline-and-indent)
	      ("C-c h" . insert-header-guard)
	      ("C-c C-c" . compile))
  :init
  (trim-ws-in-mode 'c-mode)
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux"))))

(use-package c++-mode
  :ensure nil
  :mode "\\.cpp\\'"
  :bind (:map c++-mode-map
	      ([ret] . newline-and-indent))
  :init
  (trim-ws-in-mode 'c++-mode)
  (defconst cpp-no-ns-indent
    '("linux" (c-offsets-alist . ((innamespace . [0])))))
  (c-add-style "cpp-no-ns-indent" cpp-no-ns-indent)
  (add-hook 'c++-mode-hook (lambda ()
			     (c-set-style "cpp-no-ns-indent")
			     (setq c-basic-offset 4)))
  :config
  (bind-key "C-c h" 'insert-header-guard c-mode-map))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map ("C-c b t" . hs-toggle-hiding))
  :init
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'nlinum-mode)
  (trim-ws-in-mode 'python-mode)
  :config
  (setq python-indent-offset 4))

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

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (trim-ws-in-mode 'go-mode)
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; (setenv "GOPATH" (expand-file-name "~/code/go"))
  :config
  (require 'go-guru)
  (bind-key "C-c C-k" 'godoc go-mode-map)
  (bind-key "M-," 'pop-tag-mark go-mode-map)
  (bind-key "M-." 'godef-jump go-mode-map))

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
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
	   "* %?\n :PROPERTIES:\n :ADDED: %U\n :ANNOTATION: %a\n :END:\n %i")))
  (setq org-agenda-files '("~/org/tasks.org")
	org-log-reschedule t
	org-log-done t))

(use-package org-ref
  :defer 1
  :bind (("C-c p n" . org-ref-open-bibtex-notes)
	 ("C-c p b" . goto-org-ref-bib-file)
	 ("C-c p r" . helm-bibtex))
  :init
  (defvar org-ref::bib-files '(;; "~/docs/lib/crypto.bib" ;; this guy takes forever to load
			       "~/docs/lib/bibs/refs.bib"))
  (defvar org-ref::notes-file "~/docs/lib/notes.org")
  (defvar org-ref::lib-path "~/docs/lib/pdfs")
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
	 ("w" . visual-line-mode))
  :config
  (setq shr-width 80)
  (set-face-attribute 'variable-pitch nil :family preferred-font)
  (set-face-attribute 'message-header-subject nil :family preferred-font)

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

  (add-to-list 'mu4e-view-actions
	       '("ViewInBrowser" . mu4e-action-view-in-browser))

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
		    (smtpmail-smtp-service        . ,private-mail-smtp-port))))))

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
  ;; (use-package leuven-theme
  ;;   :init (setq leuven-scale-outline-headlines nil)
  ;;   :config (load-theme 'leuven t))
  )
