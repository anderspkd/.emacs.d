(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "emacs.org"
                   user-emacs-directory))
