(defvar old-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold 64000000
      auto-save-list-file-prefix nil
      package-enable-at-startup nil
      package--init-file-ensured t
      file-name-handler-alist nil
      package-user-dir "~/.emacs.d/elpa/"
      package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

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

  (setq custom-file (emacs-dir "custom.el"))
  (load custom-file))

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
	version-control t)

  (setq browse-url-chromium-arguments '("-incognito")
	browse-url-generic-program "chromium"
	browse-url-browser-function 'browse-url-chromium))

;;; Functions

(eval-and-compile

  (defsubst remove-ws-hook ()
    (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)) t))

  (defsubst byte-recompile-dotemacs-dir ()
    (interactive)
    (byte-recompile-directory (emacs-dir) 0 t))

  (defsubst open-file-or-thing-in-mpv (file-or-thing)
    (when (stringp file-or-thing)
      (start-process "mpv-emacs" nil "mpv" file-or-thing))))

;;; Keys and movement

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package hydra)

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
  (setq pdf-annot-activate-created-annotations t)

  ;; default movements are painfully slow
  (let ((fwd (lambda (n) (interactive "p") (image-forward-hscroll (if (= n 1) 5 n))))
	(bkw (lambda (n) (interactive "p") (image-backward-hscroll (if (= n 1) 5 n))))
	(dwn (lambda (n) (interactive "p") (pdf-view-next-line-or-next-page (if (= n 1) 5 n))))
	(up  (lambda (n) (interactive "p") (pdf-view-previous-line-or-previous-page (if (= n 1) 5 n)))))
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
  (add-hook 'LaTeX-mode-hook (lambda () (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  :config
  (setq TeX-source-correlate-method-active 'synctex)
  (setq fill-column 90)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-source-correlate-start-server t
	ispell-list-command "--list")
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-style-default-list '("Default" "Hyperref")))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
	 ("\\.h\\'" . c-mode))
  :bind ([ret] . newline-and-indent)
  :init
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux"))))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map ("C-c b t" . hs-toggle-hiding))
  :init
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'nlinum-mode)
  (add-hook 'python-mode-hook 'remove-ws-hook)
  :config
  (setq python-indent-offset 4))

(use-package emacs-lisp-mode
  :ensure nil  ; already present
  :mode "\\.el\\'"
  :init
  (add-hook 'emacs-lisp-mode-hook 'remove-ws-hook))

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

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c C-l" . org-insert-link))
  :init
  (add-hook 'org-mode-hook 'yas-minor-mode)
  :config
  (defun org-add-timeslot ()
    (interactive)
    (let ((ts-string (with-temp-buffer (org-time-stamp nil) (buffer-string))))
      (org-set-property "WHEN" ts-string)))

  (bind-key "C-c w" #'org-add-timeslot org-mode-map)

  (setq org-agenda-files (directory-files "~/Documents/org/agendafiles" t "^[^.#].+\\.org\\'" t)
	org-agenda-custom-commands '(("c" "Simple Agenda view"
				      ((agenda "")
				       (alltodo ""))))
	org-log-reschedule t
	org-log-done t
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
			    (sequence "|" "WAITING(w)" "|")
			    (sequence "|" "CANCELED(c@)"))
	org-todo-keyword-faces '(("WAITING" . "blue")
				 ("CANCELED" . (:foreground "grey" :weight "bold")))))

(use-package dired
  :ensure nil  ; dired is already installed by default
  :defer t
  :bind (:map dired-mode-map
	      ([backspace] . dired-up-directory)
	      ("b" . browse-url-of-dired-file)
	      ("\"" . do-shell-and-copy-to-kill-ring)
	      ("W" . dired-play-in-mpv))
  :init
  (add-hook 'dired-mode-hook 'toggle-truncate-lines)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :config
  (use-package dired-x :ensure nil)  ; so is dired-x
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

;;; Theme settings

(eval-and-compile
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 90)

  (add-hook 'prog-mode-hook
	    (lambda ()
	      (hl-line-mode)
	      (set-face-attribute hl-line-face nil :underline nil)
	      (set-face-background 'hl-line "#ddffff")))

  (use-package leuven-theme
    :init (setq leuven-scale-outline-headlines nil)
    :config (load-theme 'leuven t))

  (use-package smart-mode-line
    :config
    (setq sml/theme 'dark)
    (sml/setup)
    (mapc #'(lambda (pattern) (add-to-list 'sml/replacer-regexp-list pattern))
	  '(("^~/Code/" ":CODE:")
	    ("^~/.config/" ":CONF:")
	    ("^~/.emacs.d" ":EMACS:")
	    ("^~/Documents/" ":DOC:")
	    ("^~/Documents/org/" ":ORG:")
	    ("^~/Documents/org/agendafiles/" ":AGENDA:")
	    ("^~/Documents/uni/" ":UNI:")))))
