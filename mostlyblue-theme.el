(deftheme mostlyblue
  "Somewhat monochrome theme that uses different shades of blue
  for most colors.")

(defgroup mostlyblue-theme nil
  "customization options for the `duck-knight' theme")

(custom-theme-set-faces
 'mostlyblue
 '(default                           ((t (:foreground "black" :background "white"))))
 '(cursor                            ((t (:foreground "white" :background "black"))))
 '(region                            ((t (:background "PaleTurquoise1"))))

 ;; programming related faces
 '(font-lock-comment-face            ((t (:foreground "SlateGray4"))))
 '(font-lock-comment-delimiter-face  ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-doc-face                ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-type-face               ((t (:foreground "blue"))))
 '(font-lock-keyword-face            ((t (:foreground "blue" :bold t))))
 '(font-lock-string-face             ((t (:foreground "dark green"))))
 '(font-lock-constant-face           ((t (:foreground "red3"))))
 '(font-lock-builtin-face            ((t (:inherit (default)))))
 '(font-lock-function-name-face      ((t (:inherit (default)))))
 '(font-lock-variable-name-face      ((t (:inherit (default)))))
 '(sh-quoted-exec                    ((t (:inherit (default)))))
 '(font-lock-preprocessor-face       ((t (:foreground "red3"))))

 ;; make
 '(makefile-targets                  ((t (:inherit (font-lock-keyword-face)))))


 ;; dired
 '(dired-directory-face              ((t (:foreground "blue" :bold t))))
 '(dired-directory                   ((t (:foreground "blue" :bold t))))

 ;; latex
 '(font-latex-math-face              ((t (:foreground "blue"))))
 '(font-latex-subscript-face         ((t (:foreground "blue"))))
 '(font-latex-superscript-face       ((t (:foreground "blue"))))
 '(superscript                       ((t (:foreground "blue"))))
 '(subscript                         ((t (:foreground "blue"))))
 '(tex-math                          ((t (:foreground "blue"))))
 '(font-latex-sectioning-1-face      ((t (:foreground "black" :height 1.0 :bold t))))
 '(font-latex-sectioning-2-face      ((t (:foreground "black" :height 1.0 :bold t))))
 '(font-latex-sectioning-3-face      ((t (:foreground "black" :height 1.0 :bold t))))
 '(font-latex-sectioning-4-face      ((t (:foreground "black" :height 1.0 :bold t))))
 '(font-latex-sectioning-5-face      ((t (:foreground "black" :height 1.0 :bold nil))))
 '(font-latex-string-face            ((t (:inherit (font-lock-string-face)))))
 '(font-latex-sedate-face            ((t (:inherit (font-lock-keyword-face)))))
 '(font-latex-italic-face            ((t (:italic t))))
 '(font-latex-slide-title-face       ((t (:inherit (font-latex-sectioning-1-face)))))

 ;; flyspell
 '(flyspell-incorrect                ((t (:underline (:color "red3")))))
 '(flyspell-duplicate                ((t (:underline (:color "red3")))))

 ;; mu4e
 '(mu4e-cited-1-face                 ((t (:foreground "SteelBlue4"))))
 '(mu4e-cited-2-face                 ((t (:inherit (mu4e-cited-1-face)))))
 '(mu4e-cited-3-face                 ((t (:inherit (mu4e-cited-1-face)))))
 '(mu4e-cited-4-face                 ((t (:inherit (mu4e-cited-1-face)))))
 '(mu4e-cited-5-face                 ((t (:inherit (mu4e-cited-1-face)))))
 '(mu4e-cited-6-face                 ((t (:inherit (mu4e-cited-1-face)))))
 '(mu4e-cited-7-face                 ((t (:inherit (mu4e-cited-1-face)))))

 ;; org-mode faces
 '(org-level-1                       ((t (:foreground "#0000aa" :bold t :background "#eefeff"))))
 '(org-level-2                       ((t (:foreground "#000088" :bold t))))
 '(org-level-3                       ((t (:foreground "#000066" :bold t))))
 '(org-level-4                       ((t (:foreground "#000044" :bold t))))
 '(org-level-5                       ((t (:foreground "#000022" :bold t))))
 '(org-level-6                       ((t (:foreground "#000022" :bold t))))
 '(org-level-7                       ((t (:foreground "#000022" :bold t))))
 '(org-level-8                       ((t (:foreground "#000022" :bold t)))))

(provide-theme 'mostlyblue)
