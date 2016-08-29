(require 'user-secrets)

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
