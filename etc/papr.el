;;; papr.el --- Small reference management system for emacs
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; PAPR (pronounced "paper") is a small Reference Management System for
;; GNU/Emacs. It is nothing more than a bunch of functions built on top of
;; org-mode.
;;
;;; Code:

(require 'org)

(defvar papr-file)
(defvar papr-directory)

(defvar papr::hash-fun '("sha1sum" . 40)
  "A tuple with the format (HASH-CMD. HASH-LEN) specifying the hash function to
use when checking if a paper has already been added to PAPR-FILE. The default
value is:

  (\"sha1sum\" . 40)

Note that HASH-LEN is the hex length (twice the output size in bytes).")

(defvar papr::debug-p nil
  "Print additional debug messages. Default is NIL.")

(defmacro papr::debug (&rest args)
  (when papr::debug-p
    `(message (concat "[PAPR::DEBUG] " (car ',args)) ,@(cdr args))))

(defsubst papr::exec-cmd (command filename)
  (let ((cmd (concat command " " (shell-quote-argument (expand-file-name filename)))))
    (papr::debug "running cmd=%s" cmd)
    (shell-command-to-string cmd)))

(defsubst papr::get-file-checksum (filename)
  (car (split-string (papr::exec-cmd (car papr::hash-fun) filename))))

(defvar papr::has-pdfinfo (executable-find "pdfinfo"))

(defsubst papr::fix-author-string (author-str)
  (replace-regexp-in-string "\\\(, and \\\)\\\|, " " and " author-str))

(defun papr::get-pdfinfo-info (filename)
  (let ((raw-str (papr::exec-cmd "pdfinfo" filename))
	info)
    (dolist (line (split-string raw-str "\n") info)
      (let ((line1 (split-string line ":")))
	(when (> (length line1) 1)
	  (let ((key (car line1))
		(val (string-trim-left (cadr line1))))
	    (pcase key
	      ("Title" (setq info (plist-put info :title val)))
	      ("Author" (setq info (plist-put info :author
					      (papr::fix-author-string val)))))))))))

(defun papr::make-new-filename (title year)
  (let ((fn-title (replace-regexp-in-string " \\\|\\\.\\\|,\\\|:" "_" title)))
    (format "[%s] %s.pdf" year fn-title)))

(defun papr::new-paper-entry (hash filename)
  (let* ((info (and papr::has-pdfinfo (papr::get-pdfinfo-info filename)))
	 (year (read-string "year: "))
	 ;; get these two from pdf-info or from the user.
	 (title (read-string "title: " (plist-get info :title)))
	 (author (read-string "authors: " (plist-get info :author)))
	 (copy-file-p (yes-or-no-p (format "copy file? [new location: %s]" papr-file))))
    (papr::debug "info=%s, year=%s, copy-file-p=%s, title=%s, author=%s"
		 info year copy-file-p title author)

    (when copy-file-p
      (let ((newname (concat (file-name-as-directory papr-directory)
			     (papr::make-new-filename title year))))
	(copy-file filename newname))
    (format "* %s
  :PROPERTIES:
  :hash:       %s
  :authors:    %s
  :year:       %s
  :END:\n"
	    title hash author year))))

;;;###autoload
(defun papr::get-paper-create (filename &optional no-switch)
  "Creates a new entry if FILENAME has not already been added.
Jumps to the (potentially new) entry in PAPR-FILE unless NO-SWITCH is non-nil."
  (interactive
   (list (read-file-name "File: " nil nil t) (not (null current-prefix-arg))))
  (let ((hash (papr::get-file-checksum filename))
	(buf (find-file-noselect papr-file))
	pos)
    (unless buf
      (error "no such file: %s" papr-file))

    (with-current-buffer buf

      (papr::debug "filename=%s, no-switch=%s" filename no-switch)

      ;; check if paper have already been added.
      (setq pos
	    (org-element-map (org-element-parse-buffer 'element) 'headline
	      (lambda (heading)
		(let ((header (cadr heading)))
		  (when (and (plist-get header :HASH)
			     (string= (plist-get header :HASH) hash))
		    (plist-get header :begin))))
	      nil t))

      (papr::debug "pos=%s" pos)

      ;; if POS got set, jump there. Otherwise we create a new entry.
      (if (not pos)
	  (progn
	    (goto-char (point-max))
	    (insert (papr::new-paper-entry hash filename)))
	(unless no-switch
	  (switch-to-buffer buf)
	  (goto-char pos))))))

;;;###autoload
(defun papr::goto-papr-file ()
  (interactive)
  (find-file papr-file))

(provide 'papr)
;;; papr.el ends here
