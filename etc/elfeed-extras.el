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
  (elfeed-search-toggle-all (apkd-get-setting :feeds-favorite-tag-name)))

(provide 'elfeed-extras)
