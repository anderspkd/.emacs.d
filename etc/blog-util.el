;;; blog-util.el --- Utility functions for creating blogs with org mode.
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2020, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Utility functions and settings for creating and maintaining a lightweight
;; static blog using org-mode. Inspired by
;; https://ogbe.net/blog/blogging_with_org.html
;;
;;; Code:

(require 'ox-publish)
(require 'ox-html)

(setq org-export-html-coding-system 'utf-8-unix)
(setq org-html-viewport nil)

(setq blog::url "anderspkd.github.io")
(setq blog::base-directory (expand-file-name "~/Documents/Homepage"))
(setq blog::base-publishing-directory blog::url)
(setq blog::default-sitemap-filename "index.org")

(setq blog::directories-and-files-plist
  '(("Front" . "/")
    ("Posts" . "/posts")
    ("Publications" . "/publications.html")))

;; Stops `org-publish' from polluting `recentf-list'.
(when (fboundp 'recentf-mode)
  (advice-add #'org-publish :around
	      #'(lambda (fun &rest args)
		  (recentf-mode -1)
		  (apply fun args)
		  (recentf-mode 1))))

;; Construct a path of the form "base-dir/dir1/dir2/.../dirn" where base-dir is
;; defined in `blog::base-directory' and `dirs' contains a list of directories to add
;; after.
(defsubst blog::make-directory (&rest dirs)
  (let ((path blog::base-directory))
    (dolist (dir dirs path)
      (setq path (concat (file-name-as-directory path) dir)))))

;; return `blog::base-directory/www/<dir1>/<dir2>...'
(defsubst blog::make-publishing-directory (&rest dirs)
  (apply #'blog::make-directory `(,blog::base-publishing-directory ,@dirs)))

(defsubst blog::insert-last-modified-html (obj)
  (insert (format-time-string
	   "<p class=\"date\">Last modified: %Y-%m-%d %a %H:%M</p>"
	   obj)))

(defsubst blog::insert-published-html (obj)
  (insert (org-timestamp-format (car obj)
	   "<p class=\"date\">Published: %Y-%m-%d</p>")))

(defsubst blog::insert-cc-license-html ()
  (insert "<p class=\"license\">CC BY-SA</p>"))

;; Construct a postamble at the bottom of the page of the form:
;;
;; <hr>
;; License: CC BY-SA
;; Last modified: <some date>
;;
;; In case auhthor and/or published are set in `options' then these are included
;; as well.
(defun blog::postamble (options)
  (let ((published (plist-get options :date)))
    (with-temp-buffer
      (insert "<hr>")
      (blog::insert-cc-license-html)
      (when published
	(blog::insert-published-html published))
      (blog::insert-last-modified-html (current-time))
      (buffer-string))))

;; Construct a preamble at the top of the page. The preamble has the form:
;;
;; <link1> <link2> ....
;;
;; Where the different links and their names are defined by the variable
;; `blog::header-links'.
(defun blog::preamble (options)
  (with-temp-buffer
    (insert "<ul>")
    (dolist (name-link-pair blog::directories-and-files-plist)
      (insert (format "<a href=\"%s\">%s</a>" (cdr name-link-pair) (car name-link-pair))))
    (insert "</ul><hr>")
    (buffer-string)))

;; Extract the preview contained at the beginning of a blog post.
(defun blog::get-preview (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (1+ (re-search-forward "^#\\+BEGIN_PREVIEW$")))
	  (end (progn (re-search-forward "^#\\+END_PREVIEW$")
		      (match-beginning 0))))
      (buffer-substring beg end))))

(defun blog::get-tags (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (re-search-forward "^#\\+TAGS:"))
	  (end (search-forward "\n")))
      (split-string (buffer-substring beg end)))))

(defsubst blog::insert-sitemap-title-html (title-string)
  (insert "#+TITLE: " title-string "\n\n"))

(defun blog::sitemap (title flist)
  (with-temp-buffer
    (blog::insert-sitemap-title-html title)
    (let ((flist (cdr flist)) s)
      (while (setq s (car (pop flist)))
	(let* ((s1 (split-string s "\\]\\["))
	       (s2 (split-string (car s1) "file:"))
	       (s3 (split-string (cadr s1) "\\]\\]"))
	       (filename (blog::make-directory "posts" (cadr s2)))
	       (entry-title (car s3))
	       (preview (blog::get-preview filename))
	       (tags (blog::get-tags filename)))
	  (insert "* " entry-title " :" (reduce (lambda (x y) (concat x ":" y)) tags) ":\n")
	  (insert preview " ")
	  (insert "[[file:" (cadr s2) "][... read more]]\n" )
	  (unless (null flist)
	    (insert "---------------\n")))))
    (buffer-string)))

(setq org-html-mathjax-options
      '((path "/res/MathJax/MathJax.js?config=TeX-AMS_HTML")))

(setq org-publish-project-alist
      `(("blog"
	 :components ("B-posts" "B-image" "B-video" "B-static" "B-audio" "B-files"))

	("B-posts"
	 :base-directory ,(blog::make-directory "posts")
	 :publishing-directory ,(blog::make-publishing-directory "posts")
	 :base-extension "org"
	 :recursive t
	 :htmlized-source t

	 :with-author nil
	 :with-date t
	 :with-toc nil
	 :with-creator nil

	 :html-doctype "html5"
	 :html-link-home ""
	 :html-link-up ""
	 :html-html5-fancy t
	 :html-postamble blog::postamble
	 :html-preamble blog::preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"

	 :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"

	 :publishing-function org-html-publish-to-html

	 :headline-levels 4
	 :todo-keywords nil
	 :section-numbers nil

	 :auto-sitemap t
	 :sitemap-filename ,blog::default-sitemap-filename
	 :sitemap-date-format "%d-%m-%Y"
	 :sitemap-title "Latest posts"
	 :sitemap-sort-files anti-chronologically
	 :sitemap-function blog::sitemap)

	("B-static"
	 :base-directory ,(blog::make-directory "static")
	 :publishing-directory ,(blog::make-publishing-directory)
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
	 :html-postamble blog::postamble
	 :html-preamble blog::preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"

	 :publishing-function org-html-publish-to-html)

	("B-image"
	 :base-directory ,(blog::make-directory "res")
	 :publishing-directory ,(blog::make-publishing-directory "res")
	 :base-extension "jpg\\|jpeg\\|png\\|gif"
	 :publishing-function org-publish-attachment
	 :recursive t)

	("B-video"
	 :base-directory ,(blog::make-directory "res")
	 :publishing-directory ,(blog::make-publishing-directory "res")
	 :base-extension "mp4\\|webm"
	 :publishing-function org-publish-attachment
	 :recursive t)

	("B-audio"
	 :base-directory ,(blog::make-directory "res")
	 :publishing-directory ,(blog::make-publishing-directory "res")
	 :base-extension "ogg"
	 :publishing-function org-publish-attachment
	 :recursive t)

	("B-files"
	 :base-directory ,(blog::make-directory "res" "files")
	 :publishing-directory ,(blog::make-publishing-directory "res" "files")
	 :base-extension "pdf\||txt"
	 :publishing-function org-publish-attachment
	 :recursive t)

	))

(provide 'blog-util)
;;; blog-util.el ends here
