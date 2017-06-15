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

(defvar asd/feeds/youtube-fmt "https://www.youtube.com/feeds/videos.xml?user=%s")
(defvar asd/feeds/reddit-fmt "https://www.reddit.com/r/%s/.rss")

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

;;; Setup feeds

(defun asd/feeds/expand-feed (feed)
  (cl-destructuring-bind (link . tags) feed
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
