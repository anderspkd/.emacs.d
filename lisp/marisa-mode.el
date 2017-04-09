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
;; To use, put
;;
;;   (setq initial-buffer-choice #'mm/make-buffer)
;;
;; somewhere in your Emacs init file. If you want an image, set
;;
;;   (setq mm/image "path/to/image")
;;
;; Can also be a function that returns a string. This can be used to
;; pick a random image from a folder, for example.
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
		    :max-width (truncate (* scale (frame-pixel-width)))))))

(defun mm/goto-file-from-line ()
  (interactive)
  (let ((thing (thing-at-point 'filename)))
    (when (and thing (file-exists-p thing))
      (find-file thing))))

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

    ;; header "Recent Files"
    (insert (propertize "H" 'display (propertize "Recent Files" 'face '((:height 120) bold))))
    (newline)

    ;; recent files
    (save-excursion
      (dolist (f recentf-list)
	(insert f)
	(newline)))

    (marisa-mode)
    (local-set-key [return] #'mm/goto-file-from-line)
    buffer))

(provide 'marisa-mode)

;;; marisa-mode.el ends here
