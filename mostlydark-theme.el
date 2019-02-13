(deftheme mostlydark
  "Somewhat monochrome theme, that is dark")

(defgroup mostlydark-theme nil "")

(let ((almost-white "#ecf0f1"))
  (custom-theme-set-faces
   'mostlydark
   `(default ((t (:foreground ,almost-white :background "black"))))
   '(cursor  ((t (:foreground "black" :background "white"))))

   '(font-lock-comment-face           ((t (:foreground "#aaaaaa"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "#aaaaaa"))))
   '(font-lock-doc-face               ((t (:foreground "#aaaaaa"))))
   '(font-lock-type-face              ((t (:foreground "cyan"))))
   '(font-lock-keyword-face           ((t (:foreground "cyan" :bold t))))
   '(font-lock-builtin-face           ((t (:inherit (default)))))
   '(font-lock-function-name-face     ((t (:inherit (default)))))
   '(font-lock-variable-name-face     ((t (:inherit (default)))))
   '(font-lock-string-face            ((t (:foreground "green"))))
   '(sh-quoted-exec                    ((t (:inherit (default)))))
   '(font-lock-preprocessor-face       ((t (:foreground "cyan3" :bold t))))

   ;; dired
   '(dired-directory-face              ((t (:foreground "cyan" :bold t))))
   '(dired-directory                   ((t (:foreground "cyan" :bold t))))

   ;; latex
   '(font-latex-math-face              ((t (:foreground "cyan"))))
   '(font-latex-subscript-face         ((t (:foreground "cyan"))))
   '(font-latex-superscript-face       ((t (:foreground "cyan"))))
   '(superscript                       ((t (:foreground "cyan"))))
   '(subscript                         ((t (:foreground "cyan"))))
   '(tex-math                          ((t (:foreground "cyan"))))
   `(font-latex-sectioning-1-face      ((t (:foreground ,almost-white :height 1.0 :bold t))))
   `(font-latex-sectioning-2-face      ((t (:foreground ,almost-white :height 1.0 :bold t))))
   `(font-latex-sectioning-3-face      ((t (:foreground ,almost-white :height 1.0 :bold t))))
   `(font-latex-sectioning-4-face      ((t (:foreground ,almost-white :height 1.0 :bold t))))
   `(font-latex-sectioning-5-face      ((t (:foreground ,almost-white :height 1.0))))
   '(font-latex-string-face            ((t (:inherit (font-lock-string-face)))))
   '(font-latex-sedate-face            ((t (:inherit (font-lock-keyword-face)))))
   '(font-latex-italic-face            ((t (:italic t))))

   ;; Makefile
   '(makefile-targets                  ((t (:inherit (font-lock-keyword-face)))))

   ))

(provide-theme 'mostlydark)
