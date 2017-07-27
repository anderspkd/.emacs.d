;;; asd-feeds.el --- RSS (el)feed related stuff
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Some functions and stuff related to feeds
;;
;;; Code:

(require 'feeds)
(require 'cl)

(defvar asd/feeds/youtube-fmt "https://www.youtube.com/feeds/videos.xml?%s")
(defvar asd/feeds/reddit-fmt "https://www.reddit.com/r/%s/.rss")

(defvar asd/feeds/tags nil)

;;; Fonts n faces

;; A bit wonky. Works, but doesn't really add the face def to elfeed-search-face-alist?
(defmacro defface-elfeed-face (name options)
  (let ((face-name (make-symbol "fn")))
    `(let ((,face-name (intern (format "%s-elfeed-face" (symbol-name ',name)))))
       (defface ,face-name ,options
	 ,(format "Face/font settings for %s" name))
       (push ',(list name face-name)
	     elfeed-search-face-alist))))

(defface-elfeed-face
  reddit '((t :foreground "#2874a6")))
(defface-elfeed-face
  advisory '((t :foreground "#58d68d")))
(defface-elfeed-face
  youtube '((t :foreground "#E74C3C")))
(defface-elfeed-face
  news '((t :foreground "#996633")))

;;; Some hooks

(defvar asd/feeds/max-string-length 90)

;; TODO: Smarter detection of sentence/paragraph delimiters (reuse
;; fill-column/paragraph in some way?)

(defun asd/feeds/fill-string (content html-p)
  (let ((new "")
	(nl (if html-p "<br>" "\r\n"))) ;; has to be this kind of newline?
    (dolist (paragraph (split-string content nl) new)
      (if (> (length paragraph) 0)
	  (let ((l 0)
		(new-str ""))
	    (dolist (str (split-string paragraph) new-str)
	      (setq l (+ l (length str)))
	      ;; break line
	      (setq new-str (concat new-str str))
	      (if (> l asd/feeds/max-string-length)
		  (setq new-str (concat new-str nl)
			l 0)
		(setq new-str (concat new-str " "))))
	    (setq new (concat new (substring new-str 0 (1- (length new-str))))))
	(setq new (concat new nl nl))))
    new))

(defun asd/feeds/fill-entry (entry)
  (when (intersection asd/feeds/fill-tags (elfeed-entry-tags entry))
    (let* ((original (elfeed-deref (elfeed-entry-content entry)))
	   (new (asd/feeds/fill-string original (eq 'html (elfeed-entry-content-type entry))))
	   (new-ref (elfeed-ref new)))
      (setf (elfeed-entry-content entry) new-ref))))

(add-hook 'elfeed-new-entry-hook #'asd/feeds/fill-entry)

;;; Setup feeds

(defun asd/feeds/expand-feed (feed)
  (cl-destructuring-bind (link . tags) feed
    ;; add tags to a list containing all tags
    (dolist (tag tags)
      (unless (member tag asd/feeds/tags)
	(push tag asd/feeds/tags)))
    (cond ((member 'youtube tags)
	   (cons (format asd/feeds/youtube-fmt link) tags))
	  ((member 'reddit tags)
	   (cons (format asd/feeds/reddit-fmt link) tags))
	  (t feed))))

(defmacro asd/feeds/feeds (&optional feeds)
  `(setq elfeed-feeds
	 (mapcar #'(lambda (x)
		     (if (listp x)
			 (asd/feeds/expand-feed x)
		       x))
		 ,feeds)))

(defsubst load-rss-feeds ()
  (asd/feeds/feeds asd/feeds))

(provide 'asd-feeds)

;;; asd-feeds.el ends here
