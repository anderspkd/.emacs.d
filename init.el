(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)

;; general configuration
(defun apkd-emacs-dir (&optional filename)
  (expand-file-name (concat user-emacs-directory filename)))

(setq custom-file (apkd-emacs-dir "custom.el"))
(load custom-file)

(load (apkd-emacs-dir "settings.el"))

;; look and feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq indent-tabs-mode nil)
(setq fill-column 80)

(line-number-mode t)
(column-number-mode t)
(electric-pair-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message nil
      ring-bell-function 'ignore
      inhibit-startup-screen t
      scroll-step 1)

(setq backup-by-copying t
      backup-directory-alist `(("." . ,(apkd-emacs-dir "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq split-height-threshold 120
      split-width-threshold 160)

(set-face-attribute 'default nil :height 90 :font "DejaVu Sans Mono")

(setq solarized-distinct-fringe-background t
      solarized-use-variable-pitch nil
      solarized-emphasize-indicators nil
      solarized-scale-org-headlines nil
      solarized-scale-markdown-headlines t
      solarized-height-minus-1 1.0
      solarized-height-plus-1 1.0
      solarized-height-plus-2 1.0
      solarized-height-plus-3 1.0
      solarized-height-plus-4 1.0)

(load-theme 'solarized-light t)

(setq frame-background-mode nil)

;; https://emacs.stackexchange.com/a/37648
(defun replace-or-delete-pair (open)
  "Replace pair at point by OPEN and its corresponding closing character.
The closing character is lookup in the syntax table or asked to
the user if not found."
  (interactive
   (list
    (read-char
     (format "Replacing pair %c%c by (or hit RET to delete pair):"
	     (char-after)
	     (save-excursion
	       (forward-sexp 1)
	       (char-before))))))
  (if (memq open '(?\n ?\r))
      (delete-pair)
    (let ((close (cdr (aref (syntax-table) open))))
      (when (not close)
	(setq close
	      (read-char
	       (format "Don't know how to close character %s (#%d) ; please provide a closing character: "
		       (single-key-description open 'no-angles)
		       open))))
      (replace-pair open close))))

(defun replace-pair (open close)
  "Replace pair at point by respective chars OPEN and CLOSE.
If CLOSE is nil, lookup the syntax table. If that fails, signal
an error."
  (let ((close (or close
		   (cdr-safe (aref (syntax-table) open))
		   (error "No matching closing char for character %s (#%d)"
			  (single-key-description open t)
			  open)))
	(parens-require-spaces))
    (insert-pair 1 open close))
  (delete-pair)
  (backward-char 1))

(bind-key "C-c e" 'replace-or-delete-pair)

(use-package ace-window
  :ensure t
  :config
  (bind-key "C-x o" 'ace-window))

(use-package yasnippet
  :ensure t)

(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-selectable-prompt t))

(ivy-mode t)

(defun apkd-do-shell-and-copy (command &optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      (dired-read-shell-command "! on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (dired-do-shell-command command arg file-list)
  (with-current-buffer "*Shell Command Output*"
    (copy-region-as-kill (point-min) (point-max))))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
              ([backspace] . dired-up-directory)
              ("b" . browse-url-of-dired-file)
              ("\"" . apkd-do-shell-and-copy))
  :hook ((dired-mode . toggle-truncate-lines)
         (dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :config
  (require 'dired-x)

  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|__pycache__")
        dired-auto-revert-buffer t
        dired-listing-switches "-AlhF --group-directories-first"))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c C-l" . org-store-link)
         ("C-c l" . org-insert-link))
  :hook ((org-mode . yas-minor-mode))
  :config

  (require 'solarized-palettes)
  (let ((green (cdr (assoc 'green solarized-light-color-palette-alist)))
	(blue (cdr (assoc 'blue solarized-light-color-palette-alist)))
	(yellow (cdr (assoc 'yellow solarized-light-color-palette-alist)))
	(red (cdr (assoc 'red solarized-light-color-palette-alist))))
    (setq org-todo-keyword-faces
	  `(("DONE" . (:foreground ,green :weight bold))
	    ("TODO" . (:foreground ,blue :weight bold))
	    ("REFINE" . (:foreground ,yellow :weight bold))
	    ("DOING" . (:foreground ,red :weight bold)))))

  (setq org-agenda-files apkd-folders-org-agenda-files
        org-log-reschedule t
	org-adapt-indentation nil
        org-log-done t))

(use-package projectile
  :ensure t
  :defer nil
  :bind (("C-c p" . projectile-commander))
  :config
  (require 'subr-x)
  (projectile-mode 1))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package auctex
  :mode ("\\.tex\\'" . tex-mode)
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
	    (lambda () (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  (setq TeX-source-correlate-method-active 'synctex
	TeX-electric-sub-and-superscript t
	TeX-auto-save t
	TeX-parse-self t
	TeX-source-correlate-start-server t
	ispell-list-command "--list")
  :config
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t
	reftex-ref-style-default-list '("Default" "Hyperref")))

(defun insert-header-guard (prefix)
  (interactive
   (list (read-string "prefix: ")))
  (let ((ext (file-name-extension (buffer-file-name)))
	((filename (file-name-base (buffer-file-name))))
	guard)
    (when (stringp filename)
      (if (string-empty-p prefix)
	  (setq guard (format "%s_%s" (upcase filename) (upcase ext)))
	(setq guard (format "%s_%s_%s" (upcase prefix) (upcase filename) (upcase ext))))
      (insert (format "#ifndef %s\n" guard))
      (insert (format "#define %s\n\n" guard))
      (forward-line)
      (save-excursion
	(goto-char (point-max))
	(insert (format "\n\n#endif %s" (format "// %s" guard)))))))

(use-package company
  :ensure t)

(use-package eglot
  :ensure t
  :init
  (add-hook 'eglot-managed-mode-hook
	    (lambda () (eglot-inlay-hints-mode -1))))

(use-package modern-cpp-font-lock
  :ensure t)

(defconst apkd-cpp-no-namespace-indent
  '("linux" (c-offsets-alist . ((innamespace . [0])
                                (topmost-intro-cont 0 nil)
                                (access-label -1)
				(inlambda . 0)))))
(c-add-style "apkd-cpp-no-namespace-indent" apkd-cpp-no-namespace-indent)

(use-package cmake-mode
  :ensure t)

(use-package c++-mode
  :defer nil
  :mode ("\\.cpp\\'" "\\.h\\'")
  :bind (([ret] . newline-and-indent)
	 ("C-c C-f" . eglot-format)
	 ("C-c C-r" . eglot-rename)
	 ("C-c f n" . flymake-goto-next-error)
	 ("C-c f p" . flymake-goto-prev-error)
	 ("C-c f d" . eglot-find-declaration))
  :hook ((c++-mode . (lambda ()
		       (c-set-style "apkd-cpp-no-namespace-indent")
		       (setq c-basic-offset 2)))
	 (c++-mode . eglot-ensure)
	 (c++-mode . yas-global-mode))
  :init
  (modern-c++-font-lock-global-mode t))

(use-package elpy
  :ensure t
  :init (elpy-enable))
