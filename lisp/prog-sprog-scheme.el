(defun scheme-send-buffer-and-go ()
  "Send entire content of the buffer to the Inferior Scheme process\
   and goto the Inferior Scheme buffer."
  (interactive)
  (scheme-send-region-and-go (point-min) (point-max)))

;; Configuration run when scheme-mode is loaded
(add-hook 'scheme-mode-hook
  (lambda ()
    ;; indent with spaces
    (setq indent-tabs-mode nil)
    ;; Danvy-style key bindings
    (local-set-key (kbd "C-c d")   'scheme-send-definition-and-go)
    (local-set-key (kbd "C-c C-b") 'scheme-send-buffer-and-go)
    ;; fix indentation of some special forms
    (put 'cond   'scheme-indent-hook 0)
    (put 'guard  'scheme-indent-hook 1)
    (put 'when   'scheme-indent-hook 1)
    (put 'unless 'scheme-indent-hook 1)
    ;; special forms from Petite Chez Scheme
    (put 'trace-lambda  'scheme-indent-hook 2)
    (put 'extend-syntax 'scheme-indent-hook 1)
    (put 'with          'scheme-indent-hook 0)
    (put 'parameterize  'scheme-indent-hook 0)
    (put 'define-syntax 'scheme-indent-hook 1)
    (put 'syntax-case   'scheme-indent-hook 0)
    ;; special forms for Schelog
    (put '%rel   'scheme-indent-hook 1)
    (put '%which 'scheme-indent-hook 1)
    ))

(add-hook 'inferior-scheme-mode-hook
  (lambda ()
  ;; Overwrite the standard 'switch-to-buffer' to use
  ;; 'switch-to-buffer-other-window'
  (defun switch-to-scheme (eob-p)
    "Switch to the scheme process buffer.
     With argument, position cursor at end of buffer."
    (interactive "P")
    (if (or (and scheme-buffer (get-buffer scheme-buffer))
            (scheme-interactively-start-process))
        (switch-to-buffer-other-window scheme-buffer)
        (error "No current process buffer.  See variable `scheme-buffer'"))
    (when eob-p
      (push-mark)
      (goto-char (point-max))))))

(setq auto-mode-alist
      (append '(("\\.ss$" . scheme-mode)
                ("\\.scm$" . scheme-mode)
                ("\\.sim$" . scheme-mode))
              auto-mode-alist))

(provide 'prog-sprog-scheme)
