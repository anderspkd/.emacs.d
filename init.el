;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved
;;
;;; Commentary:
;;
;; Emacs configuration file.
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Ensure `use-packge` is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; Some general settings related to visuals

(tool-bar-mode -1)     ; no toolbar,
(menu-bar-mode -1)     ; no menu bar,
(scroll-bar-mode -1)   ; no scroll bar,
(blink-cursor-mode -1) ; no cursor blinking, (fox only, final destination)

;; Enable line and column numbers in the modeline
(line-number-mode t)
(column-number-mode t)

;; No *scratch* comment
(setq initial-scratch-message nil)

(setq ring-bell-function 'ignore ; no bell
      inhibit-splash-screen t    ; shows *scratch* on startup
      scroll-step 1)             ; make scrolling sane

;; Display time as `weekday month day hour:minutes` in the modeline
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil)
(display-time)

;; `y` or `n` instead of `yes` or `no`
(fset 'yes-or-no-p 'y-or-n-p)

;; Place backups somewhere else than everywhere
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs_backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package iso-transl) ; fixes dead keys e.g., tilde

;; Some settings to ensure stuff is opened in the right browser
(setq browse-url-chromium-arguments '("-incognito")
      browse-url-generic-program "chromium"
      browse-url-browser-function 'browse-url-chromium)

;; Settings that are relevant when running in X
(when (eq window-system 'x)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 90)
  (use-package leuven-theme
    :ensure t
    :init (setq leuven-scale-outline-headlines nil))
  (use-package smart-mode-line
    :ensure t
    :init
    (setq sml/no-confirm-load-theme t
  	  sml/theme 'dark)
    :config
    (sml/setup)
    (dolist (pattern '(("^~/Code/" ":CODE:")
  		       ("^~/.config/" ":CONF:")
  		       ("^~/.emacs.d" ":EMACS:")
  		       ("^~/Documents/" ":DOC:")
  		       ("^~/Documents/org/" ":ORG:")
  		       ("^~/Documents/uni/" ":UNI:")))
      (add-to-list 'sml/replacer-regexp-list pattern))))

;;; Settings for keybindings and whatnot

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package hydra
  :ensure t)

(use-package asd-funcs
  ;; `demand t` is neccessary here since some of the functions it
  ;; provides are used elsewhere (e.g., elfeed and dired).
  :demand t
  :bind (("C-x K" . asd/kill-all-buffers)
	 ("C-a" . asd/back-to-indent-or-beg)
	 ("C-x r a" . asd/insert-around-rectangle)))

(use-package transpose-frame
  :ensure t
  :config
  (defhydra hydra-flop-frame (:hint nil)
    "
Capitalization is the inverse; e.g., flip is vertical, flop is horizontal.
(_s_)wap, (_f_)lip, (_F_)flop, (_r_)otate, (_R_)otate, (_q_)uit.
"
    ("s" transpose-frame)
    ("f" flip-frame)
    ("F" flop-frame)
    ("r" rotate-frame-clockwise)
    ("R" rotate-frame-anticlockwise)
    ("q" nil))
(bind-key "C-c f" 'hydra-flop-frame/body))


(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;;; Use `hjkl` to shrink window. Behaviour is a little inconsistent,
;;; depending on which window the cursor is currently in.
(defhydra hydra-resize-windows (:hint nil)
  "
Use shift to increase shrinkage ;-)
_h_:left _j_:down _k_:up _l_:right _q_:quit
"
  ("h" (shrink-window 3 t))
  ("l" (shrink-window -3 t))
  ("j" (shrink-window -3))
  ("k" (shrink-window 3))
  ("H" (shrink-window 9 t))
  ("L" (shrink-window -9 t))
  ("J" (shrink-window -9))
  ("K" (shrink-window 9))
  ("q" nil))

(bind-key "C-c r" 'hydra-resize-windows/body)

;;; More settings

(defsubst asd/remove-ws-hook ()
  "Auto delete trailing whitespace before saving in some modes."
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)) nil t))

(use-package nlinum
  :defer t
  :ensure t)

(use-package pdf-tools
  :demand t
  :ensure t
  :init
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  :config
  (pdf-tools-install))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer t
  :config
  (yas-reload-all))

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package tex
  ;; no `:ensure` for this as it is installed by the distro
  :mode ("\\.tex\\'" . tex-mode)
  :init
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-source-correlate-method-active 'synctex
	ispell-list-command "--list")
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (dolist (fun '((lambda () (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))
		 #'visual-line-mode
		 #'tex-source-correlate-mode
		 (lambda () (TeX-add-symbols '("eqref" TeX-arg-ref (ignore))))
		 #'turn-on-flyspell
		 #'LaTeX-math-mode
		 #'turn-on-reftex))
    (add-hook 'LaTeX-mode-hook fun)))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
	 ("\\.h\\'" . c-mode))
  :bind ([ret] . newline-and-indent)
  :init
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux"))))

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :init
  (use-package flycheck-rust
    :ensure t)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'asd/remove-ws-hook))

;;; TODO: actual make this usable (i.e., add feeds)
(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed)
	 :map elfeed-search-mode-map
	 ("x" . open-search-entry-in-mpv))
  :init
  (setq elfeed-curl-program-name "curl"
	elfeed-use-curl t
	elfeed-search-filter "@2-weeks-ago +unread ")

  (defun open-search-entry-in-mpv ()
    "Opens a search entry in mpv and marks said entry as read."
    (interactive)
    (let* ((entry (elfeed-search-selected :single))
	   (url (elfeed-entry-link entry)))
      (asd/send-to-mpv url)))

  :config
  ;; remove `unread` tag from old entries.
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "2 weeks ago"
				:remove 'unread))
  (defface reddit-entry
    '((t :foreground "#2874a6"))
    "Colors for reddit entries.")

  (defface advisory-entry
    '((t :foreground "#58d68d"))
    "Colors for security advisories.")

  (defface youtube-entry
    '((t :foreground "#e74c3c"))
    "Colors for Youtube entries.")

  (dolist (tag-and-face '((reddit reddit-entry)
			  (advisory advisory-entry)
			  (youtube youtube-entry)))
    (push tag-and-face elfeed-search-face-alist)))


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
	      ("C-c b t" . hs-toggle-hiding))
  :init
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook #'eldoc-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  ;; TODO: find some way to toggle this on/off.
  (add-hook 'python-mode-hook #'asd/remove-ws-hook)
  (add-hook 'python-mode-hook #'nlinum-mode))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
	      ([backspace] . dired-up-directory)
	      ("b" . browse-url-of-dired-file)
	      ("W" . play-in-mpv))
  :init
  (add-hook 'dired-mode-hook (lambda () (toggle-truncate-lines)))
  (defun play-in-mpv ()
    (interactive)
    (let ((file (dired-get-filename)))
      (when file
	(asd/send-to-mpv file))))
  :config
  (setq dired-auto-revert-buffer t
	;; `-v` and `-group-directories-first` are GNU ls specific afaik
	dired-listing-switches "-alhFv --group-directories-first"))

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :init
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'asd/remove-ws-hook))

(use-package tramp
  :ensure t
  :defer t
  :init
  (setq tramp-default-method "ssh"))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.css\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'auto-revert-mode))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c C-l" . org-insert-link))
  :init
  (add-hook 'org-mode-hook 'yas-minor-mode)
  :config
  (unless org-agenda-files
      (setq org-agenda-files '("~/Documents/org/agenda.org")))
  (setq org-log-done t
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
			    (sequence "WAITING(w)" "|")
			    (sequence "|" "CANCELED(c)"))
	org-todo-keyword-faces '(("WAITING" . "yellow")
				 ("CANCELED" . (:foreground "grey" :weight "bold")))))

;; put that junk somewhere out of the way.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
