;;; asd-blog.el --- Stuff for blogging
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Settings and so on, that enables me to blog using Emacs. Inspired
;; by https://ogbe.net/blog/blogging_with_org.html
;;
;;; Code:

(require 'htmlize)
(require 'ox-publish)
(require 'ox-html)

(defvar asd/blog/navbar-items
  '(("/" . "about")
    ("/posts/blog.html" . "blog")
    ("/contact.html" . "contact")
    ("/links.html" . "links")))

(defsubst asd/blog/base-dir (&rest subdirs)
  (let ((d "~/Documents/blog/"))
    (dolist (sd subdirs d)
      (setq d (concat (file-name-as-directory d) sd)))))

(defsubst asd/blog/pub-dir (&rest subdirs)
  (apply #'asd/blog/base-dir `("www" ,@subdirs)))

(defun asd/blog/postamble (options)
  (with-temp-buffer
    (insert "<hr>")
    ;; Author is sometimes nil (e.g., sitemap) so don't include it in that case.
    (let ((author (car (plist-get options :author))))
      (when author
	(insert (format "<p class=\"author\">Author: %s</p>" author))))
    (insert (format-time-string "<p class=\"date\">Last modified: %Y-%m-%d %a %H:%M</p>"))
    (buffer-string)))

(defun asd/blog/preamble (options)
  (with-temp-buffer
    (insert "<ul>")
    (dolist (it asd/blog/navbar-items)
      (insert (format "<li><a href=\"%s\">%s</a></li>" (car it) (cdr it))))
    (insert "</ul><hr>")
    (buffer-string)))

(defun asd/blog/get-preview (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((b (1+ (re-search-forward "^#\\+BEGIN_PREVIEW$")))
	  (e (progn (re-search-forward "^#\\+END_PREVIEW$")
		    (match-beginning 0))))
      (buffer-substring b e))))

(defun asd/blog/sitemap (project &optional sitemap-fn)
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory (plist-get project-plist :base-directory)))
	 (sitemap-fn (concat dir (or sitemap-fn "sitemap.org")))
	 (visiting (find-buffer-visiting sitemap-fn))
	 (localdir (file-name-directory dir))
	 (files (nreverse (org-publish-get-base-files project)))
	 file sitemap-buffer)
    (with-current-buffer
	(let ((org-inhibit-startup t))
	  (setq sitemap-buffer (or visiting (find-file sitemap-fn))))
      (erase-buffer)
      (insert "#+TITLE: " (plist-get project-plist :sitemap-title) "\n\n")
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file)))
	  (unless (equal (file-truename sitemap-fn) (file-truename file))
	    (let ((title (org-publish-format-file-entry "%t" file project-plist))
		  (date (org-publish-format-file-entry "%d" file project-plist))
		  (preview (asd/blog/get-preview file)))
	      (insert "* [[file:" fn "][" title "]] \n")
	      (insert preview "\n")
	      (insert "Posted: " date ".\n\n")))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

;;; This is currently needed. Org ignores project settings. Options
;;; does fuck-all (besides setting the path...)
(setq org-html-mathjax-options '((path "/res/mathjax/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
				 (scale "100") (align "left") (indent "2em") (mathml nil))
      org-html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>")

(setq org-publish-project-alist
      `(("blog"
	 :components ("blog-posts" "blog-image" "blog-video" "blog-static"))
	("blog-posts" ;; posts
	 :base-directory ,(asd/blog/base-dir "posts")
	 :publishing-directory ,(asd/blog/pub-dir "posts")
	 :base-extension "org"
	 :recursive t
	 :htmlized-source t

	 :with-author t
	 :with-date t
	 :with-toc nil
	 :with-creator nil

	 :html-doctype "html5"
	 :html-html5-fancy t
	 :html-postamble asd/blog/postamble
	 :html-preamble asd/blog/preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"

	 ;; :html-mathjax-options ,asd/blog/mathjax-options
	 ;; :html-mathjax-template "<script type=\"text/javascript\" src=\"%PATH\"></script>"

	 :headline-levels 4
	 :todo-keywords nil
	 :section-numbers nil

	 ;; sitemap stuff
	 :auto-sitemap t
	 :sitemap-filename "blog.org"
	 :sitemap-title "Latests posts"
	 :sitemap-date-format "%Y-%m-%d"
	 :sitemap-sort-files anti-chronologically
	 :sitemap-function asd/blog/sitemap

	 :publishing-function org-html-publish-to-html)

	("blog-static" ;; contact, links, and so on
	 :base-directory ,(asd/blog/base-dir)
	 :publishing-directory ,(asd/blog/pub-dir)
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
	 :html-postamble asd/blog/postamble
	 :html-preamble asd/blog/preamble
	 :html-head "<link rel=\"stylesheet\" href=\"/res/style.css\" type=\"text/css\"/>"

	 :publishing-function org-html-publish-to-html)

	("blog-image" ;; images
	 :base-directory ,(asd/blog/base-dir "img")
	 :publishing-directory ,(asd/blog/pub-dir "img")
	 :base-extension ".*" ;; TODO: restrict
	 :publishing-function org-publish-attachment
	 :recursive t)

	("blog-video" ;; video
	 :base-directory ,(asd/blog/base-dir "vid")
	 :publishing-directory ,(asd/blog/pub-dir "vid")
	 :base-extension ".*" ; TODO: restrict
	 :publishing-function org-publish-attachment
	 :recursive t)
	 ))

(provide 'asd-blog)
;;; asd-blog.el ends here
