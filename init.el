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

;; I use this twice, so that justifies abstraction :^)
(defsubst emacsdir (f)
  (expand-file-name (concat user-emacs-directory f)))

(add-to-list 'load-path (emacsdir "lisp"))

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

;; Auto parenthesis stuff
(electric-pair-mode t)

;; No *scratch* comment
(setq initial-scratch-message nil)

(setq ring-bell-function 'ignore ; no bell
      inhibit-startup-screen t
      scroll-step 1)             ; make scrolling sane

;; Display time as `weekday month day hour:minutes` in the modeline
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil)
(display-time)

;; `y` or `n` instead of `yes` or `no`
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

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

;; For the sake of excluding certain files from recentf.
;; Bugs when `:ensure t` is present
(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (dolist (p (list ".*\\.synctex\\.gz\\'"
		   ".*\\.aux\\'"
		   (expand-file-name "~/.elfeed/index")
		   (emacsdir "custom.el")
		   (emacsdir "elpa/.*")
		   (emacsdir "recentf")))
    (add-to-list 'recentf-exclude p))
  (recentf-mode t))

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
      (add-to-list 'sml/replacer-regexp-list pattern)))
  (use-package marisa-mode
    :demand t
    :config
    ;; pick a random image from the specified folder
    (setq mm/image (lambda ()
		     (let ((files (directory-files "~/Pictures/marisas/scaled" t ".*\\.png\\'\\|.*\\.jpe?g\\'\\|.*\\.gif\\'")))
		       (nth (random (length files)) files))))
    (mm/init)))

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

;; use `hjkl' to shrink/expand current window in a variety of ways.
(defhydra hydra-resize-windows (:hint nil)
  "
Shrink/expand window -- use `hjkl'.
_q_:quit
"
  ("h" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window 3 t))))
  ("l" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window -3 t))))
  ("j" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window -3))))
  ("k" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window 3))))
  ("q" nil))
(bind-key "C-c r" 'hydra-resize-windows/body)

;; Zoom functionality, from the github page of Hydra.
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

(defhydra hydra-jump-around (:hint nil)
  "
jump to either `.,!' in text (C-u for reverse direction)
_q_:quit
"
  ("." (lambda (b) (interactive "P") (if b (search-backward ".") (search-forward "."))))
  ("," (lambda (b) (interactive "P") (if b (search-backward ",") (search-forward ","))))
  ("!" (lambda (b) (interactive "P") (if b (search-backward "!") (search-forward "!"))))
  ("q" nil))

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
  (pdf-tools-install)
  (bind-key "C-f" (lambda (n) (interactive "p")
		    (image-forward-hscroll (if (= n 1) 5 n)))
	    pdf-view-mode-map)
  (bind-key "C-b" (lambda (n) (interactive "p")
		    (image-backward-hscroll (if (= n 1) 5 n)))
	    pdf-view-mode-map)
  (bind-key "C-n" (lambda (n) (interactive "p")
		    (pdf-view-next-line-or-next-page (if (= n 1) 5 n)))
	    pdf-view-mode-map)
  (bind-key "C-p" (lambda (n) (interactive "p")
		    (pdf-view-previous-line-or-previous-page (if (= n 1) 5 n)))
	    pdf-view-mode-map))

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
  :mode ("\\.tex\\'" . tex-mode)
  :bind ("C-c j" . hydra-jump-around/body)
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook (lambda () (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'tex-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-source-correlate-method-active 'synctex
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

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :init
  (use-package flycheck-rust
    :ensure t)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'asd/remove-ws-hook))

(use-package elfeed
  :ensure t
  :bind (("C-x w" . elfeed)
	 :map elfeed-search-mode-map
	 ("x" . open-in-mpv)
	 ("f" . favorite-entry)
	 ("F" . display-favorites))
  :init
  (setq elfeed-curl-program-name "curl"
	elfeed-use-curl t
	elfeed-search-title-max-width 100)
  (setq-default elfeed-search-filter "@1-week-ago +unread -advisory ")

  ;; Helper function for `display-favorites' -- Works like `concat',
  ;; except it puts `sep' (if supplied) between each element. E.g.,
  ;;  (concat-ext " -> " "a" "b" "c") => "a -> b -> c".
  (defun concat-ext (sep &rest seq)
    (if (not sep)
	(apply #'concat seq)
      (let ((r "") s)
	(while (setq s (pop seq))
	  (setq r (concat r s (unless (null seq) sep))))
	r)))

  ;; Open an entry in mpv. Useful for youtube entries.
  (defun open-in-mpv ()
    (interactive)
    (let ((entry (elfeed-search-selected :single)))
      (elfeed-search-untag-all 'unread) ;; mark as read
      (asd/send-to-mpv (elfeed-entry-link entry))))

  ;; Favorite an entry (by giving it a `faved' tag). If already
  ;; favored, remove `faved' tag.
  (defun favorite-entry ()
    (interactive)
    (let ((entry (elfeed-search-selected :single))
	  (fav-tag 'faved))
      (if (member fav-tag (elfeed-entry-tags entry))
	  (elfeed-untag-1 entry fav-tag)
	(elfeed-tag-1 entry fav-tag))
      (elfeed-search-update-entry entry)))

  ;; Display all entries with the `faved' tag. I.e., display favorited
  ;; entries.
  (defun display-favorites ()
    (interactive)
    (let ((search-filter (split-string elfeed-search-filter)))
      (if (member "+faved" search-filter)
	  (setq elfeed-search-filter
		(apply #'concat-ext
		       `(" " .,(remove-if #'(lambda (x) (string= x "+faved"))
					  search-filter))))
	(setq elfeed-search-filter
	      (apply #'concat-ext `(" " ,@search-filter "+faved"))))
      (elfeed-search-update--force)))

  :config
  ;; Needed to ensure proper display of e.g., Japense text in
  ;; elfeed-show.
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans Mono")
  (set-face-attribute 'message-header-subject nil :family "DejaVu Sans Mono")

  (require 'asd-feeds)
  (load-rss-feeds))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
	      ("C-c b t" . hs-toggle-hiding))
  :init
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
  (add-hook 'python-mode-hook #'eldoc-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook #'asd/remove-ws-hook) ; maybe someway to turn this on/off
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
  (require 'dired-x)
  (dired-omit-mode 1)
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
	 ("\\.php\\'" . web-mode)
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
  (add-hook 'org-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'org-mode-hook #'asd/remove-ws-hook)
  :config
  ;; (unless org-agenda-files
  (setq org-agenda-files '("~/Documents/org/agenda.org"))
  (setq org-log-reschedule t
	org-log-done t
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
			    (sequence "|" "WAITING(w)" "|")
			    (sequence "|" "CANCELED(c)"))
	org-todo-keyword-faces '(("WAITING" . "blue")
				 ("CANCELED" . (:foreground "grey" :weight "bold")))))

;; put that junk somewhere out of the way.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
