;;; asd-blog.el --- Stuff for blogging
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2018, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Settings and so on, that enables me to blog using Emacs. Inspired
;; by https://ogbe.net/blog/blogging_with_org.html
;;
;;; Code:

(require 'ox-publish)
(require 'ox-html)
(require 'ox-rss)

(setq org-export-html-coding-system 'utf-8-unix)
(setq org-html-viewport nil)

(defvar asd-blog:base-dir (expand-file-name "~/docs/blog/"))
(defvar asd-blog:default-sitemap-filename "index.org")

(defvar asd-blog:header-links
  '(("/" . "Front")
    ("/blog" . "Blog")
    ("/contact.html" . "Contact")
    ("/links.html" . "Links")))

;; small hack that stops `org-publish' from polluting `recentf-list'.
(when (fboundp 'recentf-mode)
  (advice-add #'org-publish :around
	      #'(lambda (fun &rest args)
		  (recentf-mode -1)
		  (apply fun args)
		  (recentf-mode 1))))

;; return `asd-blog:base-dir/<dir1>/<dir2>...'
(defsubst asd-blog:df (&rest dirs)
  (let ((d asd-blog:base-dir))
    (dolist (sd dirs d)
      (setq d (concat (file-name-as-directory d) sd)))))

;; return `asd-blog:base-dir/www/<dir1>/<dir2>...'
(defsubst asd-blog:pub-df (&rest dirs)
  (apply #'asd-blog:df `("www" ,@dirs)))

(defsubst asd-blog:insert-author-html (str)
  (let ((str (if (listp str) (car str) str)))
    (insert (format "<p class=\"author\">Author: %s</p>" str))))

(defsubst asd-blog:insert-last-modified-html (obj)
  (insert (format-time-string
	   "<p class=\"date\">Last modified: %Y-%m-%d %a %H:%M</p>"
	   obj)))

(defsubst asd-blog:insert-published-html (obj)
  (insert (org-timestamp-format (car obj)
	   "<p class=\"date\">Published: %Y-%m-%d</p>")))

;; construct the postamble (thing at bottom of the page)
(defun asd-blog:postamble (options)
  (let ((author (plist-get options :author))
	(published (plist-get options :date)))
    (with-temp-buffer
      (insert "<hr>")
      (when author
	(asd-blog:insert-author-html author))
      (when published
	(asd-blog:insert-published-html published))
      (asd-blog:insert-last-modified-html (current-time))
      (buffer-string))))

;; thing at the top of the page
(defun asd-blog:preamble (options)
  (with-temp-buffer
    (insert "<ul>")
    (dolist (e asd-blog:header-links)
      (insert (format "<a href=\"%s\">%s</a>" (car e) (cdr e))))
    (insert "</ul><hr>")
    (buffer-string)))

;; recover post preview that is located in a #+BEGIN_PREVIEW and #+END_PREVIEW
;; block.
(defun asd-blog:get-preview (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (1+ (re-search-forward "^#\\+BEGIN_PREVIEW$")))
	  (end (progn (re-search-forward "^#\\+END_PREVIEW$")
		      (match-beginning 0))))
      (buffer-substring beg end))))

(defsubst asd-blog:insert-sitemap-title-html (title-string)
  (insert "#+TITLE: " title-string "\n\n"))

;; construct sitemap file
(defun asd-blog:sitemap (title flist)
  (with-temp-buffer
    (asd-blog:insert-sitemap-title-html title)
    (let ((flist (cdr flist)) s)
      (while (setq s (car (pop flist)))
	(let* ((s1 (split-string s "\\]\\["))
	       (s2 (split-string (car s1) "file:"))
	       (s3 (split-string (cadr s1) "\\]\\]"))
	       (filename (asd-blog:df "blog" (cadr s2)))
	       (entry-title (car s3))
	       (preview (asd-blog:get-preview filename)))
	  (insert "* " entry-title "\n")
	  (insert preview " ")
	  (insert "[[file:" (cadr s2) "][... read more]]\n" )
	  (unless (null flist)
	    (insert "---------------\n")))))
    (buffer-string)))

(setq org-html-mathjax-options
      '((path "/res/MathJax/MathJax.js?config=TeX-AMS_HTML")))

(setq org-publish-project-alist
      `(("blog"
	 :components ("B-posts" "B-image" "B-video" "B-static" "B-audio" "B-drafts"))

	("B-posts" ;; posts
	 :base-directory ,(asd-blog:df "blog")
	 :publishing-directory ,(asd-blog:pub-df "blog")
	 :base-extension "org"
	 :recursive t
	 :htmlized-source t

	 :with-author t
	 :with-date t
	 :with-toc nil
	 :with-creator nil

	 :html-doctype "html5"
	 :html-link-home ""
	 :html-link-up ""
	 :html-html5-fancy t
	 :html-postamble asd-blog:postamble
	 :html-preamble asd-blog:preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"

	 :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"

	 :publishing-function org-html-publish-to-html

	 :headline-levels 4
	 :todo-keywords nil
	 :section-numbers nil

	 ;; sitemap stuff
	 :auto-sitemap t
	 :sitemap-filename ,asd-blog:default-sitemap-filename
	 :sitemap-date-format "%d-%m-%Y"
	 :sitemap-title "Latests posts"
	 :sitemap-sort-files anti-chronologically
	 :sitemap-function asd-blog:sitemap)

	("B-drafts"
	 :base-directory ,(asd-blog:df "drafts")
	 :publishing-directory ,(asd-blog:pub-df "drafts")
	 :base-extension "org"
	 :recursive t
	 :htmlized-source t
	 :with-author t
	 :with-date nil
	 :with-toc nil
	 :with-creator nil
	 :html-doctype "html5"
	 :html-link-home ""
	 :html-link-up ""
	 :html-postamble asd-blog:postamble
	 :html-preamble asd-blog:preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"
	 :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :todo-keywords nil
	 :section-numbers nil
	 )

	("B-static" ;; contact, links, and so on
	 :base-directory ,(asd-blog:df "pages")
	 :publishing-directory ,(asd-blog:pub-df)
	 :base-extension "org"
	 :recursive t
	 :htmlized-source t

	 :with-author nil
	 :with-date t
	 :with-toc nil
	 :with-creator nil
	 :headline-levels 4
	 :todo-keywords nil
	 :section-numbers nil

	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-postamble nil
	 :html-preamble asd-blog:preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"

	 :publishing-function org-html-publish-to-html)

	("B-image" ;; images
	 :base-directory ,(asd-blog:df "res")
	 :publishing-directory ,(asd-blog:pub-df "res")
	 :base-extension "jpg\\|jpeg\\|png\\|gif"
	 :publishing-function org-publish-attachment
	 :recursive t)

	("B-video" ;; video
	 :base-directory ,(asd-blog:df "res")
	 :publishing-directory ,(asd-blog:pub-df "res")
	 :base-extension "mp4\\|webm"
	 :publishing-function org-publish-attachment
	 :recursive t)

	("B-audio" ;; audio
	 :base-directory ,(asd-blog:df "res")
	 :publishing-directory ,(asd-blog:pub-df "res")
	 :base-extension "ogg"
	 :publishing-function org-publish-attachment
	 :recursive t)
	))

(provide 'asd-blog)
;;; asd-blog.el ends here
