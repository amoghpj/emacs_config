;; (add-to-list 'package-archives
;; 	     '("melpa" . "https://melpa.org/packages/") t)

;; Swtiched on 2018-06-11
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require 'package)
(package-initialize)

(org-babel-load-file "~/.emacs.d/config.org")
;; To debug, fix el file and load it
;; (load-file "~/.emacs.d/config.el")
