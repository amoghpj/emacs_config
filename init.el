(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(require 'package)
(package-initialize)

(org-babel-load-file "~/.emacs.d/config.org")
