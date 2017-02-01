(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;;;;;
;;;; Themes, fonts and other stuff related to looks and default behaviour
;;;;;;
(when (eq window-system 'x)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 90)
  (setq leuven-scale-outline-headlines nil)
  (load-theme 'leuven t))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(line-number-mode t)
(column-number-mode t)
(setq ring-bell-function 'ignore
      inhibit-splash-screen t
      initial-scratch-message nil
      scroll-step 1)
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil)
(display-time)
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete selection on typing. Behaviour can be feel wonky if one is
;; used to electric-pair like modes, when used in modes without such
;; modes.
(delete-selection-mode 1)

;; Number windows so we can switch between them with M-<n>
(window-numbering-mode 1)

;; Set frame title to <user>@<machine>:<buffer name>
(setq frame-title-format (concat (user-login-name) "@" (system-name) ":%b"))

(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t))

;; especially for 'flop-frame
(use-package transpose-frame)

(use-package smartparens-config)

(use-package iso-transl) ;; fix dead keys

;; Place backups in a specific directory
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs_backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
	sml/theme 'light)
  :config
  (sml/setup)
  (mapc (lambda (x) (add-to-list 'sml/replacer-regexp-list x))
  	'(("^~/Code/" ":CODE:")
	  ("^~/.config/" ":CONF:")
	  ("^~/.emacs.d/" ":EMACS:")
	  ("^~/Documents/" ":DOC:")
	  ("^~/Documents/org/" ":ORG:")
	  ("^~/Documents/uni" ":UNI:"))))

(use-package ansi-color
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;;;;;
;;;; Misc functions
;;;;;;
(defun recreate-scratch ()
  "Recreate scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun kill-all-buffers ()
  "Kill all buffers and open *scratch* -- basically resets the session."
  (interactive)
  (when (y-or-n-p "Kill all buffers?")
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)
    (recreate-scratch)))

(defun reload-dotemacs ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun byte-recompile-everything ()
  "Force recompile everything in .emacs.d"
  (interactive)
  (byte-recompile-directory "~/.emacs.d/" 0 t))

(defun base64-string-length (beg end)
  "Decode region as base64 (stripping quotes) and output the
length in the message buffer"
  (interactive "r")
  (if (and beg end)
      (let* ((selection (buffer-substring-no-properties beg end))
	     (b64-decoded (base64-decode-string (replace-regexp-in-string "'" "" selection))))
	(message "decoded length: %d bytes" (length b64-decoded)))))

(defun unixtime->readable (beg end)
  "Intepret selection as a unix timestamp and output a (more)
human readable representation in the minibuffer."
  (interactive "r")
  (when (and beg end)
    (let ((selected (buffer-substring-no-properties beg end)))
      (message (format-time-string "%F %T%z" (string-to-int selected))))))

;;;;;;
;;;; Keybinds (non mode specific)
;;;;;;
(put 'upcase-region 'disabled nil)

(bind-key "C-x K" #'kill-all-buffers)

;;;;;;
;;;; Hooks and advices for non-specific modes
;;;;;;

;; yeah yeah, this is mode-specific, fight me.
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

;; Delete trailing whitespaces before saving a file.
;; TODO: should be applied on a per-mode basis
(advice-add 'save-buffer :before
	    #'(lambda (&rest args)
		(delete-trailing-whitespace)))
;;;;;;
;;;; Package configs
;;;;;;
(use-package yasnippet
  :config
  (yas-reload-all))

(use-package web-mode
  :mode ("\\.html\\'"
	 "\\.php\\'"))

(use-package tex
  :mode ("\\.tex\\'" . tex-mode)
  :init
  (setq TeX-auto-save t
	TeX-parse-self t
	ispell-list-command "--list")
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (TeX-add-symbols '("eqref" TeX-arg-ref (ignore)))))
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  :config
  (sp-local-pair 'latex-mode "``" "''" :trigger "\"" :actions :rem))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
	 ("\\.h\\'" . c-mode))
  :bind ([ret] . newline-and-indent)
  :init
  (add-hook 'c-mode-hook (lambda () (c-set-style "linux")))
  (add-hook 'c-mode-hook 'smartparens-mode)
  (add-hook 'c-mode-hook 'yas-minor-mode))

(use-package slime
  :mode ("\\.lisp\\'" . common-lisp-mode)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"
	slime-contribs '(slime-fancy))
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'lisp-mode-hook 'smartparens-mode)
  (add-hook 'lisp-mode-hook 'yas-minor-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
	      ("C-c C-q" . hs-toggle-hiding))
  :init
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
  (add-hook 'python-mode-hook 'jedi:setup)
  ;; (add-hook 'python-mode-hook 'run-python)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
  ;; (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'linum-mode)
  ;; FIXME: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24401
  (setq python-shell-interpreter "python2")
  )

(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ([tab] . helm-execute-persistent-action)
	 ("C-z" . helm-select-action))
  :init
  (setq helm-split-window-in-side-p t
	helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t))

(use-package elfeed
  :load-path ("lisp/" "super-secret-directory/")
  :bind (("C-x w" . elfeed)
	 :map elfeed-search-mode-map
	 ("x" . elfeed-search-open-entry-in-mpv))
  :init
  (setq elfeed-curl-program-name "curl"
	elfeed-use-curl t
	elfeed-search-filter "@2-weeks-ago +unread ")

  (setq browse-url-chromium-arguments '("-incognito")
      browse-url-generic-program "chromium"
      browse-url-browser-function 'browse-url-chromium)

  (defun elfeed-search-open-entry-in-mpv ()
    (interactive)
    (let* ((entry (elfeed-search-selected :single))
	   (url (elfeed-entry-link entry)))
      (elfeed-search-untag-all 'unread)
      (start-process "mpv-emacs" nil "mpv" url)))

  :config
  (use-package user-feeds))

(use-package dired
  :bind (:map dired-mode-map
	      ([backspace] . dired-up-directory)
	      ("b" . browse-url-of-dired-file)))

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)
	 ("C-c C-l" . org-insert-link))
  :init
  (setq org-log-done t
	org-agenda-files '("~/Documents/org/agenda.org")
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
			    (sequence "WAITING(w)" "|")
			    (sequence "SOME DAY" "|")
			    (sequence "|" "DID NOT DO")
			    (sequence "|" "CANCELED(c)"))
	org-todo-keyword-faces '(("WAITING" . "yellow")
				 ("CANCELED" . (:foreground "grey" :weight "bold"))))
  (add-hook 'org-mode-hook 'yas-minor-mode))
  ;; (add-hook 'org-mode-hook 'smartparens-mode)
  ;; :config
  ;; (sp-with-modes 'org-mode
  ;;   (sp-local-pair "*" "*" :unless '(sp-point-after-word-p sp-point-at-bol-p))
  ;;   (sp-local-pair "_" "_")
  ;;   (sp-local-pair "~" "~")
  ;;   (sp-local-pair "=" "=")))
