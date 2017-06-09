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

;; Must be a string or function that returns a string
(defvar mm/image nil)
(defvar mm/buffer-name " mm")

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
		    ;; :max-width (truncate (* scale (frame-pixel-width))) ;no need to constrain on width, i think.
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

;; Do this in order to not screw with other modes' keymaps when
;; binding the above function to return.
(define-derived-mode marisa-mode special-mode " HELLO")

(defun mm/make-buffer ()
  (let ((buffer (get-buffer-create mm/buffer-name))
	(image (mm/make-image nil 0.5)))
    (set-buffer buffer)

    ;; show image
    (when image
      (put-image image 0)
      (newline))

    ;; header "Recent Files". Inserting the heading this way ensures
    ;; it only occupies 1 character.
    (insert (propertize "H" 'display (propertize "Recent Files" 'face '((:height 120) bold))))
    (newline)

    ;; recent files
    (save-excursion
      (dolist (f recentf-list)
	(insert f)
	(newline)))

    (marisa-mode)
    (local-set-key [return] #'mm/goto-file-from-line)
    (local-set-key "n" #'next-line)
    (local-set-key "p" #'previous-line)
    buffer))

;; TODO: should not run when init.el is reloaded from within an emacs
;; session
(defun mm/init ()
  (when (and (= (length command-line-args) 1)
	     (string= (car command-line-args) "emacs")) ; can != even happen here?
    (switch-to-buffer (mm/make-buffer))))

(provide 'marisa-mode)

;;; marisa-mode.el ends here
