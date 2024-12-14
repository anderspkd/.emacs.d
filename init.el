(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)

(defun apkd-emacs-dir (&optional file-name)
  (expand-file-name (concat user-emacs-directory file-name)))

(setq custom-file (apkd-emacs-dir "custom.el"))
(load custom-file)

;; settings.el is a file which defiles a plist and a single helper function. It
;; is equivalent to
;;
;;  (setq apkd-settings nil)
;;
;;  (defun apkd-setting (key value)
;;   (setq apkd-settings (plist-put apkd-settings key value)))
;;
;;  (defun apkd-get-setting (key)
;;   (plist-get apkd-settings key))
;;
;; Settings are then defined in the settings.el file as
;;
;;  (apkd-setting :key value)
;;
;; and read using the APKD-GET-SETTING function below.
(load (apkd-emacs-dir "settings.el"))

;; some general look-and-feel
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(electric-pair-mode t)

;; use 'y' or 'n' instead of 'yes' or 'no' for selections
(fset 'yes-or-no-p 'y-or-n-p)

;; always indent with spaces
(setq indent-tabs-mode nil)

;; default to 80 character wide columns when wrapping is performed.
;; customize with C-x f
(setq-default fill-column 80)

;; start emacs in an empty scratch buffer
(setq initial-scratch-message nil
      inhibit-startup-screen t)

;; ding!
(setq ring-bell-function 'ignore)

;; set the scroll step to 1. Makes scrolling behave a bit more sanely
(setq scroll-step 1)

;; place all #filename backup files in .emacs.d/backups. Generates less clutter 
;; on the file system.
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(apkd-emacs-dir "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(set-face-attribute 'default nil :height 90 :font "DejaVu Sans Mono")

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark-high-contrast)
  (enable-theme 'solarized-dark-high-contrast))

;; https://emacs.stackexchange.com/a/37648
(defun apkd-replace-or-delete-pair (open)
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

(bind-key "C-c e" 'apkd-replace-or-delete-pair)

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
  (setq ivy-use-selectable-prompt t)
  :config
  (ivy-mode t))

(defun apkd-do-shell-and-copy (command &optional arg file-list)
  "Executes COMMAND on a file. Useful in dired mode."
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

  (setq org-agenda-files (apkd-get-setting :org-folder)
        org-log-reschedule t
	org-adapt-indentation nil
        org-log-done t))

(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds (apkd-get-setting :feeds))
  (setq elfeed-search-title-max-width 160))

(use-package hydra
  :ensure t)

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

(use-package projectile
  :ensure t
  :defer nil
  :bind (("C-c p" . projectile-commander))
  :config
  (require 'subr-x)
  (projectile-mode 1))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (setq pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  
  (bind-key "k" (lambda (interactive)) pdf-view-mode-map)

  (bind-key "h" 'pdf-annot-add-highlight-markup-annotation pdf-view-mode-map)
  (bind-key "t" 'pdf-annot-add-text-annotation pdf-view-mode-map)
  (bind-key "D" 'pdf-annot-delete pdf-view-mode-map)

  (setq pdf-annot-default-annotation-properties
	`((t (label . ,user-full-name))
	  (text (icon . "Comment") (color . "#ff0000"))
	  (highlight (color . "LightCyan2"))
	  (squiggly (color . "orange"))
	  (strike-out (color . "red"))
	  (underline (color . "blue"))))

  ;; redfine movement keys to make navigating PDFs more pleasant. The
  ;; default movement of 1 is waaaay to slow.
  (let ((fwd (lambda (n) (interactive "p")
	       (image-forward-hscroll (if (= n 1) 5 n))))
	(bkw (lambda (n) (interactive "p")
	       (image-backward-hscroll (if (= n 1) 5 n))))
	(dwn (lambda (n) (interactive "p")
	       (pdf-view-next-line-or-next-page (if (= n 1) 5 n))))
	(up  (lambda (n) (interactive "p")
	       (pdf-view-previous-line-or-previous-page (if (= n 1) 5 n)))))

    (bind-key "C-f"      fwd pdf-view-mode-map)
    (bind-key "<right>"  fwd pdf-view-mode-map)

    (bind-key "C-b"      bkw pdf-view-mode-map)
    (bind-key "<left>"   bkw pdf-view-mode-map)

    (bind-key "C-n"      dwn pdf-view-mode-map)
    (bind-key "<down>"   dwn pdf-view-mode-map)
    (bind-key "<return>" dwn pdf-view-mode-map)

    (bind-key "C-p"      up  pdf-view-mode-map)
    (bind-key "<up>"     up  pdf-view-mode-map)))

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
  (add-hook 'LaTeX-mode-hook (lambda () (setq fill-column 100)))
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
  :bind (:map c++-mode-map
	      ([ret] . newline-and-indent)
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

(use-package slime
  :ensure t
  :mode ("\\.lisp" . lisp-mode)
  :hook ((lisp-mode . (lambda ()
			(setq inferior-lisp-program "/usr/bin/sbcl"))))
  :config
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj" . clojure-mode))

(use-package cider
  :ensure t
  :after clojure-mode
  :config
  (setq cider-preferred-build-tool 'lein))
