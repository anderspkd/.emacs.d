;;; asd-funcs.el --- Misc functions
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; Various miscellaneous functions. All functions are prefixed with
;; asd/ ftso. avoiding name conflicts, but also because it makes it
;; easy to see which functions I've personally defined somewhere.
;;
;;; Code:

(defun asd/recreate-scratch ()
  "Recreates the scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun asd/kill-all-buffers ()
  "Kill everything and open *scratch*. I.e., reset the session."
  (interactive)
  (when (y-or-n-p "Kill all buffers? ")
    (mapc 'kill-buffer (buffer-list))
    (delete-other-windows)
    (asd/recreate-scratch)))

(defun asd/reload-dotemacs ()
  "Reload .emacs.d/init.el."
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun asd/byte-recompile-everything ()
  "Recompile all the files."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/" 0 t))

(defun asd/base64-string-length (beg end)
  "Decode region between BEG and END as base64 and output the
length of the segment in the minibuffer."
  (interactive "r")
  (when (and beg end)
    (let* ((selection (buffer-substring-no-properties beg end))
	   (b64-decd (base64-decode-string selection)))
      (message "length=%d" (length b64-decd)))))

(defun asd/unixtime->ts (beg end)
  "Convert the unix timestamp between BEG and END to something
readable."
  (interactive "r")
  (when (and beg end)
    (let ((ts-string (buffer-substring-no-properties beg end)))
      (message (format-time-string "%F %T%z" (string-to-number ts-string))))))

(defun asd/back-to-indent-or-beg ()
  "Move cursor to indentation or, if already there, to beginning
of line."
  (interactive)
  (when (= (point) (progn (back-to-indentation) (point)))
    (beginning-of-line)))

(defun asd/file-size-human-readable (beg end)
  "Super thin wrapper around `file-size-human-readable. Converts
selection into something readable and messages it."
  (interactive "r")
  (let ((thing (buffer-substring-no-properties beg end)))
    (if thing
	(message "%s" (file-size-human-readable (string-to-number thing) 'iec)))))

(defsubst asd/send-to-mpv (url)
  "Open URL in mpv. Useful for youtube entries in elfeed."
  (when (stringp url)
    (start-process "mpv-emacs" nil "mpv" url)))

(provide 'asd-funcs)

;;; asd-funcs.el ends here
