;;; rmse.el --- Small reference management system for emacs
;;
;; Author: Anders Dalskov
;; Copyright: (C) 2017, Anders Dalskov, all rights reserved.
;;
;;; Commentary:
;;
;; RMSE (pronounced "ramsee") is a small Reference Management System for
;; GNU/Emacs. It is nothing more than a bunch of functions built on top of
;; org-mode.
;;
;;; Code:
(require 'org)

(defvar rmse-file)       ;; database file
(defvar rmse-directory)  ;; directory for storing PDFs

(defvar rmse::hash-fun "sha1sum")  ;; hash function for checking if an entry exists.

(defvar rmse::debug-p nil)  ;; enables RMSE::DEBUG macro

(defmacro rmse::debug (&rest args)
  (when rmse::debug-p
    `(message (concat "[RMSE::DEBUG] " (car ',args)) ,@(cdr args))))

;;; Helper function for constructing and running a shell command.
(defsubst rmse::exec-cmd (command filename)
  (let ((cmd (concat command " " (shell-quote-argument (expand-file-name filename)))))
    (rmse::debug "running cmd=%s" cmd)
    (shell-command-to-string cmd)))

;;; Get hash of a file (assumes *sum type outputs)
(defsubst rmse::get-file-checksum (filename)
  (car (split-string (rmse::exec-cmd rmse::hash-fun filename))))

;;; Read title with pdfinfo, if it is installed on the system
(if (executable-find "pdfinfo")
    (defun rmse::get-title-pdfinfo (filename)
      (let ((raw-str (rmse::exec-cmd "pdfinfo" filename))
	    info)
	(dolist (line (split-string raw-str "\n") info)
	  (let ((line1 (split-string line ":")))
	    (when (> (length line1) 1)
	      (let ((key (car line1))
		    (val (string-trim-left (cadr line1))))
		(when (string= key "Title") val)))))))
  (defun rmse::get-title-pdfinfo (filename) nil))

;;; Creates a new filename of the form "[year] title.pdf"
(defun rmse::make-new-filename (title year)
  (let ((fn-title (replace-regexp-in-string " \\\|\\\.\\\|,\\\|:" "_" title)))
    (when (> (length fn-title) (- 249 (length year)))
      (setq fn-title (substring fn-title 0 (- 249 (length year) 1))))
    (format "[%s]_%s.pdf" year fn-title)))

(defun rmse::read-authors-from-minibuffer ()
  (let (author (author-string "") (num 0))

    ;; read author input from user until they input nothing
    (while (progn
	     (setq author (completing-read
			   (format "Author #%s (Return to exit): " (setq num (1+ num)))
			   rmse::authors))

	     ;; seperate current author and previous ones with an "and"
	     (unless (or (string= author-string "") (string= author ""))
	       (setq author-string (concat author-string " and " author)))

	     (rmse::debug "author-string=%s" author-string)

	     (unless (string= author "")

	       ;; new author so add to suggestions
	       (unless (member author rmse::authors)
		 (push author rmse::authors))

	       (when (string= author-string "")
		 (setq author-string (concat author-string author)))

	       t)))
    author-string))

(defvar rmse::authors nil)

(defun rmse::find-all-authors ()
  (with-current-buffer (find-file-noselect rmse-file)
    (org-element-map (org-element-parse-buffer 'element) 'headline
      (lambda (heading)
	(let* ((header (cadr heading))
	       (authors (plist-get header :AUTHORS)))
	  (when authors
	    (dolist (author (split-string authors " and "))
	      (unless (member author rmse::authors)
		(push author rmse::authors))))))
      nil t)))

;; if this is the first time this function is called, we scan the database
;; file and add any authors we find.
(unless rmse::authors
  (setq rmse::authors (rmse::find-all-authors)))

(defvar rmse::conferences nil)

(defun rmse::find-all-conferences ()
  (with-current-buffer (find-file-noselect rmse-file)
    (org-element-map (org-element-parse-buffer 'element) 'headline
      (lambda (heading)
	(let* ((header (cadr heading))
	       (conference (plist-get header :CONFERENCE)))
	  (when (and conference (not (member conference rmse::conferences)))
	    (push conference rmse::conferences))))
      nil t)))

;; Same deal as above
(unless rmse::conferences
  (setq rmse::conferences (rmse::find-all-conferences)))

;;; Creates a new entry. Will read a bunch of information from the minibuffer
(defun rmse::new-paper-entry (hash filename)
  (let ((title (read-string "Title: " (rmse::get-title-pdfinfo filename)))
	(authors (rmse::read-authors-from-minibuffer))
	(year (read-string "Year: "))
	(conference (completing-read "Conference: " rmse::conferences))

	(copy-file-p (yes-or-no-p (format "copy file? [new location: %s]" rmse-directory)))
	link)

    ;; if we're copying the file, also insert a link for easy access.
    (when copy-file-p
      (let ((newname (concat (file-name-as-directory rmse-directory)
			     (rmse::make-new-filename title year))))
	(copy-file filename newname)
	(setq link (concat "file:" newname))))  ;; assume link is local

    (format "* %s
  :PROPERTIES:
  :hash:       %s
  :authors:    %s
  :year:       %s
  :conference: %s
  :link:       [[%s]]
  :END:\n"
	    title hash authors year conference link)))

;;; because why not
(define-derived-mode rmse-mode org-mode "RMSE")

;;;###autoload
(defun rmse::get-paper-create (filename &optional no-switch)
  "Creates a new entry if FILENAME has not already been added.
Jumps to the (potentially new) entry in RMSE-FILE unless NO-SWITCH is non-nil."
  (interactive
   (list (read-file-name "File: " nil nil t) (not (null current-prefix-arg))))
  (let ((hash (rmse::get-file-checksum filename))
	(buf (find-file-noselect rmse-file))
	pos)
    (unless buf
      (error "no such file: %s" rmse-file))

    (with-current-buffer buf

      (rmse::debug "filename=%s, no-switch=%s" filename no-switch)

      ;; check if paper have already been added.
      (setq pos
	    (org-element-map (org-element-parse-buffer 'element) 'headline
	      (lambda (heading)
		(let ((header (cadr heading)))
		  (when (and (plist-get header :HASH)
			     (string= (plist-get header :HASH) hash))
		    (plist-get header :begin))))
	      nil t))

      (rmse::debug "pos=%s" pos)

      ;; if POS got set, jump there. Otherwise we create a new entry.
      (if (not pos)
	  (progn
	    (goto-char (point-max))
	    (insert (rmse::new-paper-entry hash filename))
	    (unless no-switch
	      (switch-to-buffer buf)))
	(unless no-switch
	  (switch-to-buffer buf)
	  (goto-char pos)))

      (save-buffer))))

;;;###autoload
(defun rmse::goto-rmse-file ()
  (interactive)
  (when (and (not (file-exists-p rmse-directory))
	      (yes-or-no-p "rmse dir missing. Create new one?"))
    (make-directory rmse-directory))
  (find-file rmse-file)
  (rmse-mode))

(provide 'rmse)
;;; rmse.el ends here
