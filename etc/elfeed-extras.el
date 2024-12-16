(require 'elfeed)

(defmacro elfeed-extras-defface-for-tag (tag options)
  "defines the face options for entries with a particular tag."
  (let ((face-name (make-symbol "fn")))
    `(let ((,face-name (intern (format "elfeed-tag-%s-face" (symbol-name ,tag)))))
       (defface ,face-name ,options
	 ,(format "Face settings for tag %s" tag)
	 :group 'elfeed)
       (push (list ,tag ,face-name) elfeed-search-face-alist))))

(defun elfeed-extras-toggle-favorite ()
  "toggle 'favorite tag on entry under point"
  (interactive)
  (let ((entry (elfeed-search-selected :single))
	(favorite-tag (apkd-get-setting :feeds-favorite-tag-name)))
    (if (member favorite-tag (elfeed-entry-tags entry))
	(elfeed-untag-1 entry favorite-tag)
      (elfeed-tag-1 entry favorite-tag))
    (elfeed-search-update-entry entry)))

(defun elfeed-extras-show-favorites ()
  "show all entries marked with the 'favorite tag"
  (interactive)
  (elfeed-toggle-tag (symbol-name (apkd-get-setting :feeds-favorite-tag-name))))

(provide 'elfeed-extras)
