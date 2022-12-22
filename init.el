(defvar old-file-name-handler-alist file-name-handler-alist)

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

  ;; load custom directory and folder name variables
  (load-file (emacs-dir (concat "personal/settings")))

  (setq custom-file (emacs-dir "custom.el"))
  (load custom-file)

  (setq-default indent-tabs-mode nil)

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

  (setq-default fill-column 80)

  (setq initial-scratch-message nil
	ring-bell-function 'ignore
	inhibit-startup-screen t
	scroll-step 1)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq backup-by-copying t
	backup-directory-alist `(("." . ,asd::folders::emacs-backups))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t)

  (defvar trim-ws-modes nil
    "Modes in which whitespace should automatically be trimmed")

  (defun enable-automatic-whitespace-trimming (mode)
    "Automatically trim ws in MODE. if NO-TRIM is non-nil, then
MODE disable ws trimming."
    (unless (memq mode trim-ws-modes)
      (push mode trim-ws-modes)))

  (defun smart-whitespace-trim ()
    (when (memq major-mode trim-ws-modes)
      (delete-trailing-whitespace)))

  (add-hook 'before-save-hook 'smart-whitespace-trim t))

(bind-key "C-c C-d"
          (lambda ()
            (interactive)
            (move-beginning-of-line 1)
            (kill-line)
            (yank)
            (open-line 1)
            (next-line 1)
            (yank)))

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

(bind-key "C-c q" 'quick-find-directory)

 (defun quick-find-directory (directory)
    (interactive
     (let* ((hist (mapcar 'car asd::folders::quick-dirs))
	    (choice (completing-read "directory: " hist)))
       (list (cdr (assoc choice asd::folders::quick-dirs)))))
    (dired directory))

(bind-key "C-c e" 'replace-or-delete-pair)

(use-package ace-window
  :ensure t
  :config
  (bind-key "C-x o" 'ace-window))

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds asd::rss-feeds))

(use-package dired
  :ensure nil  ; dired is already installed by default
  :defer t
  :bind (:map dired-mode-map
	      ([backspace] . dired-up-directory)
	      ("b" . browse-url-of-dired-file)
	      ("\"" . do-shell-and-copy-to-kill-ring))
  :init
  (add-hook 'dired-mode-hook 'toggle-truncate-lines)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :config
  (use-package dired-x :ensure nil)  ; so is dired-x
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$\\|__pycache__"))
  (setq dired-auto-revert-buffer t
	;; no "." and "..", long-list, human readable, classify, dirs first
	dired-listing-switches "-AlhF --group-directories-first")

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

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
	 ("C-c C-l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c i" . org-insert-link))
  :config
  (yas-minor-mode)
  (enable-automatic-whitespace-trimming 'org-mode)
	     
  (setq org-agenda-files asd::folders::org-files
	org-log-reschedule t
	org-log-done t))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-commander))
  :config
  (require 'subr-x)
  (projectile-mode 1))

(use-package hydra
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind ("C-c m" . hydra-mc/body)
  :config
  (defhydra hydra-mc (:hint nil)
    "
Mark multiple things (_n_) next item, (_p_) previous item. (_q_) quit"
    ("n" mc/mark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("q" nil)))

(defhydra hydra-resize-windows (global-map "C-c r")
  "Resize buffer"
  ("h" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window 3 t))))
  ("l" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window -3 t))))
  ("j" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window -3))))
  ("k" (lambda (n) (interactive "p") (dotimes (i n) (shrink-window 3))))
  ("q" nil))

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
  (enable-automatic-whitespace-trimming 'python-mode)
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
	python-indent-offset 4))

;; (pdf-tools-install :no-query)

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

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq ansi-color-names-vector
      ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"])

(defun insert-header-guard (prefix)
  "Insert a C/C++ style header guard with a name derived from the
filename."
  (interactive
   (list (read-string "prefix: ")))
  (let ((ext (file-name-extension (buffer-file-name))))
    (let ((is-cpp (string= ext "hpp"))
	  (is-c (string= ext "h")))
      (unless (or is-cpp is-c)
	(error "Not in a c or c++ header file ... "))
      (let* ((filename (file-name-base (buffer-file-name)))
	     guard)
	(when (stringp filename)
	  (if prefix
	      (setq guard (format "%s_%s_%s" (upcase prefix) (upcase filename) (upcase ext)))
	    (setq guard (format "%s_%s" (upcase filename) (upcase ext))))
	  (insert (format "#ifndef %s\n" guard))
	  (insert (format "#define %s\n\n" guard))
	  (forward-line)
	  (save-excursion
	    (goto-char (point-max))
	    (insert (format "\n\n#endif %s" (format "// %s" guard)))))))))

(defconst clang-format-file-of-buffer-p t)

(defun clang-format-file-of-buffer ()
  (interactive)
  (when clang-format-file-of-buffer-p
    (message "formatting buffer with clang-format")
    (let ((filename (buffer-file-name)))
      (call-process "clang-format" nil nil nil filename "-i" "--style=Google")
      (revert-buffer nil t t))))

(bind-key "C-c f" 'clang-format-file-of-buffer)

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-selectable-prompt t))

(ivy-mode t)

(use-package c++-mode
  :ensure nil
  :mode ("\\.cpp\\'" "\\.h\\'")
  :bind (:map c++-mode-map              
	      ([ret] . newline-and-indent))
  :init
  (use-package modern-cpp-font-lock :ensure t)
  (projectile-mode 1)
  (modern-c++-font-lock-global-mode t)
  (enable-automatic-whitespace-trimming 'c++-mode)
  ;; define a custom c++ style where namespaces and lambdas have no indentation.
  (defconst cpp-no-ns-indent
    '("linux" (c-offsets-alist . ((innamespace . [0])
                                  (topmost-intro-cont 0 nil)
                                  (access-label -1)
				  (inlambda . 0)))))
  (c-add-style "cpp-no-ns-indent" cpp-no-ns-indent)
  (add-hook 'c++-mode-hook (lambda ()
			     (c-set-style "cpp-no-ns-indent")
			     (setq c-basic-offset 2)
                             (setq indent-tabs-mode nil)
                             (define-key c++-mode-map (kbd "C-c C-d") nil)))
  (setq lsp-clients-clangd-args '("--fallback-style=Google"))
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c++-mode-hook (lambda () (setq fill-column 80)))
  (add-hook 'c++-mode-hook (lambda () (yas-global-mode 1)))
  :config
  (setq lsp-enable-on-type-formatting nil))

(use-package lsp
  :ensure nil
  :init
  (add-hook 'lsp-mode-hook (lambda ()
                             (let ((lsp-keymap-prefix "C-c l"))
                               'lsp-enable-which-key-integration)))
  (require 'dap-cpptools)
  (yas-global-mode)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(eval-and-compile
  (require 'iso-transl)
  (set-face-attribute 'default nil :height 90 :font "DejaVu Sans Mono")
  
  (require 'color-theme-sanityinc-tomorrow)
  (color-theme-sanityinc-tomorrow 'night)

  (setq frame-background-mode nil)
  
  ;; Fun with bitmaps :-)
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
     #b00000000]))
