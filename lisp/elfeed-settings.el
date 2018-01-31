(eval-when-compile
  (require 'cl))

(defvar asd-feeds-tags '(youtube reddit))
(defvar asd-feeds-youtube-fmt "https://www.youtube.com/feeds/videos.xml?%s")
(defvar asd-feeds-reddit-fmt "https://www.reddit.com/r/%s/.rss")

;; Helper for customizing face options
(defmacro defface-helper (name options)
  (let ((face-name (make-symbol "fn")))
    `(let ((,face-name (intern (format "%s-elfeed-face" (symbol-name ',name)))))
       (defface ,face-name ,options
	 ,(format "Face settings for tag %s" name)
	 :group 'elfeed)
       (push ',(list name face-name) elfeed-search-face-alist))))

(defface-helper reddit '((t :foreground "#2874a6")))
(defface-helper advisory '((t :foreground "#58d68d")))
(defface-helper youtube '((t :foreground "#e74c3c")))
(defface-helper news '((t :foreground "#996633")))

(defun asd-feeds-fill-string (content)
  (with-temp-buffer
    (insert content)
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun asd-feeds-fill-entry (entry)
  (when (intersection asd-feeds-tags (elfeed-entry-tags entry))
    (let* ((old (elfeed-deref (elfeed-entry-content entry)))
	   (new (asd-feeds-fill-string old))
	   (new-ref (elfeed-ref new)))
      (setf (elfeed-entry-content entry) new-ref))))

(defvar asd-feeds-favorite-tag 'favorite)

(defun asd-feeds-mark-favorite ()
  "Mark or unmark the entry below the cursor as a favorite."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (if (member asd-feeds-favorite-tag (elfeed-entry-tags entry))
	(elfeed-untag-1 entry asd-feeds-favorite-tag)
      (elfeed-tag-1 entry asd-feeds-favorite-tag))
    (elfeed-search-update-entry entry)))

(defun asd-feeds-show-favorites ()
  "display all entries marked as favorites."
  (interactive)
  (elfeed-toggle-tag (symbol-name asd-feeds-favorite-tag)))

(defsubst concat-ext (sep &rest seq)
  (if sep
      (let ((r "") s)
	(while (setq s (pop seq))
	  (setq r (concat r s (unless (null seq) sep))))
	r)
    (apply #'concat seq)))

(defun asd-feeds-toggle-tag (tag &optional exclude-p)
  "Toggle `tag'. If the current tags are (A, B, C) and
`asd-feeds-toggle-tag' is called with D, then the new list of
tags will be (A, B, C, D). Calling the function again reverts the
tag list to (A, B, C).

`exclude-p' determines whether or not the added tag is used to
include or exclude results (i.e. whether or not \"-\" or \"+\" is
added in front of `tag'). Default is nil"
  (interactive
   (if current-prefix-arg
       (list (completing-read "toggle tag (exclude): " asd-feeds-tags) t)
     (list (completing-read "toggle tag (exclude): " asd-feeds-tags))))
  (let* ((filter-list (split-string elfeed-search-filter))
	 (tag-i (concat (if exclude-p "+" "-") tag))

	 ;; E.g. if we're adding `+tag', we must remove `-tag', if it
	 ;; is present
	 (filter-list (remove-if #'(lambda (x) (string= x tag-i)) filter-list))
	 (tag (concat (if exclude-p "-" "+") tag)))
    (when (member tag filter-list)
	;; `tag' is present, so we remove it (un-toggle)
	(setq filter-list (remove-if #'(lambda (x) (string= x tag)) filter-list)))
    (setq elfeed-search-filter (apply #'concat-ext ,filter-list))
    (elfeed-search-update--force)))

;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el#L146
(defun asd-feeds-expand-feed (feed)
  (if (not (listp feed))
      feed
    (cl-destructuring-bind (link . tags) feed
      (dolist (tag tags)
	(unless (member tag asd-feeds-tags)
	  (push tag asd-feeds-tags)))
      (cond ((member 'youtube tags)
	     (cons (format asd-feeds-youtube-fmt link) tags))
	    ((member 'reddit tags)
	     (cons (format asd-feeds-reddit-fmt link) tags))
	    (t feed)))))

;;;###autoload
(defun reload-feeds (file-name)
  "reloads RSS feeds"
  (interactive "fFile: ")
  (load file-name)
  (setq elfeed-feeds (mapcar #'asd-feeds-expand-feed feeds))
  (elfeed-search-update--force))

(provide 'elfeed-settings)
