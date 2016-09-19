(require 'user-secrets)

;; Don't tag old entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

;;; Some settings relating to faces
(defface reddit-elfeed-entry
  '((t :foreground "#2874a6"))
  "colors for 'reddit tagged entries")
(push '(reddit reddit-elfeed-entry)
      elfeed-search-face-alist)

(defface advisory-elfeed-entry
  '((t :foreground "#58d68d"))
  "colors for 'advisory tagged entries")
(push '(advisory advisory-elfeed-entry)
      elfeed-search-face-alist)

(defface youtube-elfeed-entry
  '((t :foreground "#E74C3C"))
  "colors for 'youtube tagged entries")
(push '(youtube youtube-elfeed-entry)
      elfeed-search-face-alist)

(defun expand-feed (feed)
  (cl-destructuring-bind (link . tags) feed
    (cond ((member 'youtube tags)
           (cons (format "https://www.youtube.com/feeds/videos.xml?user=%s" link) tags))
          ((member 'reddit tags)
           (cons (format "https://www.reddit.com/r/%s/.rss" link) tags))
          (t feed))))

(defmacro feeds (feeds)
  `(setq elfeed-feeds
         (mapcar #'(lambda (x)
                     (if (listp x)
                         (expand-feed x)
                       x))
                 ,feeds)))

(feeds secret/feeds-alist)

(provide 'user-feeds)
