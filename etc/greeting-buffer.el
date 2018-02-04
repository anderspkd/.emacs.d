(require 'recentf)

(defvar gb-buffer-name " gb")
(defvar gb-recent-files-keys "1 2 3 4 5 6 7 8 9 0")

(define-derived-mode greeting-mode special-mode " HELLO")

(defun gb-make-entry (file-name &optional key)
  (let ((entry file-name)
	(key-cmd (when key `(,key . (lambda () (interactive) (find-file ,file))))))
    (if key
	(setq entry (concat "[" key "] " entry))
      (setq entry (concat "    " entry)))
    (cons entry key-cmd)))

(defun gb-goto-file-on-line (&optional file-name)
  (interactive)
  (let ((file-name (or file-name (thing-at-point 'filename))))
    (when (file-exists-p file-name)
      (find-file file-name))))

(defun gb-make-buffer (&optional image)
  (let ((buf (get-buffer-create gb-buffer-name))
	(header-string "[R] feeds, [T] TODOs, [A] Agenda, [B] Bookmarks\nRecent Files")
	additional-keys)
    (set-buffer buf)

    ;; not implemented
    (when image)

    (insert (propertize "H" 'display (propertize header-string 'face '((:height 120) bold))))
    (newline)

    (save-excursion
      (let ((quick-keys (split-string gb-recent-files-keys)))
	(dotimes (i (length recentf-list) additional-keys)
	  (let ((e (gb-make-entry (nth i recentf-list)
				  (and (< i (length quick-keys)) (nth i quick-keys)))))
	    (insert (car e))
	    (newline)
	    (push (cdr e) additional-keys)))))

    (greeting-mode)
    (local-set-key [return] #'gb-goto-file-on-line)
    (local-set-key "n" #'next-line)
    (local-set-key "p" #'previous-line)
    (local-set-key "R" #'elfeed)
    (local-set-key "T" #'org-todo-list)
    (local-set-key "A" #'org-agenda-list)
    (local-set-key "B" #'bookmark-bmenu-list)
    (dolist (kc additional-keys)
      (when kc
	(local-set-key (car kc) (cdr kc))))

    buf))

;;;###autoload
(defun gb-init ()
  (interactive)
  (switch-to-buffer (gb-make-buffer)))

(provide 'greeting-buffer)
