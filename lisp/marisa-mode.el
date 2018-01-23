;;; marisa-mode.el --- List recent files and an optional image
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Shows content of `recentf-list' and makes them clickable
;; (via. return). If `mm/image' is set, use it to determine an image
;; that is shown as well.
;;
;; Use case is obviously ftso. of being greeted by your favorite 2hu
;; (Marisa Kirisame), when Emacs is started :-)
;;
;; To use simply call `mm/init' somewhere in your Emacs init file. If
;; you want an image as well, `mm/image' set to something.
;;
;;; Code:

(require 'recentf)

(defvar mm/image nil
  "image to be displayed. Can be `nil', a `string' (in which case
  it should be a path to an image) or a `function' (in which
  case it should return a path to an image)")
(defvar mm/buffer-name " mm")

(defvar mm/current-image nil
  "Holds the data of the current image. Lets us redraw the screen
  without having to re-creating an image (which is expensive)")

;; Ensure recentf-list is populated
(unless recentf-list
  (recentf-load-list))

;; Creates an image from `mm/image'. If imagemagick is not installed,
;; then typ must be supplied. In this case, the max-{height,weight}
;; will also be ignored (IIRC).
(defun mm/make-image (&optional typ scale)
  (when mm/image
    (let ((image-loc (if (functionp mm/image) (funcall mm/image) mm/image))
	  (image-typ (if (fboundp 'imagemagick-types) 'imagemagick typ))
	  (scale (or scale 0.6)))
      (create-image image-loc image-typ nil
		    :max-height (truncate (* scale (frame-pixel-height)))
		    :max-width (truncate (* scale (frame-pixel-width))) ;no need to constrain on width, i think.
		    ))))

(defun mm/goto-file-from-line (&optional file)
  (interactive)
  (let ((thing (or file (thing-at-point 'filename))))
    (if (and thing (file-exists-p thing))
	(find-file thing)
      ;; for lines of the form `[k] filepath'
      (mm/goto-file-from-line (cadr (split-string (thing-at-point 'line)))))))

(defun mm/create-entry (file &optional key)
  (let ((entry file)
	(key-cmd (when key `(,key . (lambda () (interactive) (find-file ,file))))))
    (if key
	(setq entry (concat "[" key "] " entry))
      (setq entry (concat "... " entry)))
    (cons entry key-cmd)))

(define-derived-mode marisa-mode special-mode " HELLO")

(defun mm/make-buffer (&optional image)
  (let ((buffer (get-buffer-create mm/buffer-name))
	(image (or image (mm/make-image nil 0.5)))
	(header-str "[R] RSS, [T] TODOs, [A] Agenda [B] Bookmarks\nRecent Files")
	additional-keys)
    (set-buffer buffer)

    ;; show image
    (when image
      (unless mm/current-image
	(setq mm/current-image image))
      (put-image image 0)
      (newline))

    ;; header "Recent Files". Inserting the heading this way ensures
    ;; it only occupies 1 character.
    (insert (propertize "H" 'display (propertize header-str 'face '((:height 120) bold))))
    (newline)

    ;; recent files
    (save-excursion
      (let* ((quick-keys (split-string "1 2 3 4 5 6 7 8 9 0 a s d f g h"))
	     (quick-keys-len (length quick-keys)))
	(dotimes (i (length recentf-list) additional-keys)
	  (let ((e (mm/create-entry (nth i recentf-list)
				    (and (< i quick-keys-len) (nth i quick-keys)))))
	    (insert (car e))
	    (newline)
	    (setq additional-keys (cons (cdr e) additional-keys))))))

    (marisa-mode)
    (local-set-key [return] #'mm/goto-file-from-line)
    (local-set-key "n" #'next-line)
    (local-set-key "p" #'previous-line)
    (local-set-key "R" #'elfeed)
    (local-set-key "T" #'org-todo-list)
    (local-set-key "A" #'org-agenda-list)
    (local-set-key "B" #'bookmark-bmenu-list)
    (dolist (kc additional-keys)
      (when kc
	(cl-destructuring-bind (key . cmd) kc
	  (local-set-key key cmd))))
    buffer))

(defvar mm/old-buffer nil "Holds the first buffer created")

;; TODO: should not run when init.el is reloaded from within an emacs
;; session.
;; Should be more "intelligent".
(defun mm/init ()
  (if mm/old-buffer
      (switch-to-buffer mm/old-buffer)
    (when (and (= (length command-line-args) 1)
	       (string= (car command-line-args) "emacs")) ; can != even happen here?
      (let ((buf (mm/make-buffer mm/current-image)))
	(switch-to-buffer (setq mm/old-buffer buf))))))

(defun mm/goto ()
  (interactive)
  (mm/init))

(provide 'marisa-mode)

;;; marisa-mode.el ends here
