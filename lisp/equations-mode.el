;;; aj-equation-mode.el --- Attempt at syntax highlighting for an equations file

;; Author: Amogh Jalihal
;; Created: 26th December 2018

;; Commentary:
;; Syntax highlighting

(defgroup equations nil
  "Syntax highlighting for a tab separated variables.txt file."
  :prefix "equations-"
  )

;; (setq equations-operators
;;       '(("*\\|+\\|-\\|\\/" . font-lock-function-name-face)
;;         ("shs\\|min" . font-lock-constant-face)))

(defface equations-lhs-face
  '((t (:foreground "Indianred3" :weight bold)))
  "red")
(defface equations-operators-face
  '((t (:foreground "Dodgerblue2" :weight bold)))
  "blue")


(defface equations-rhs-face
  '((t (:foreground "Dodgerblue2" :weight bold)))
  "blue")

(defface equations-comment-face
  '((t (:foreground "grey" :weight bold)))
  "grey")





(define-derived-mode equations-mode fundamental-mode "equations"
  "equations-mode is a major mode for viewing variables.txt file."
  
  (font-lock-add-keywords nil '(("#.*\\|^#.*" . 'equations-comment-face)))
  (font-lock-add-keywords nil '(("^.*\t" . 'equations-lhs-face)))
  ;;(font-lock-add-keywords nil '(("^\\([[A-Za-z0-9]]+\\)"  1 'equations-lhs-face)))
  ;; (font-lock-add-keywords nil '(("\t.*$" . 'equations-rhs-face)))

  (font-lock-add-keywords nil '(("*\\|+\\|-\\|shs\\|(\\|)" . 'equations-operators-face)))
  ;; If the size is greater than 1024^2 Emacs switch to read-only-mode in order
  ;; to save performance.

  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo))

  ;; Comment syntax
  ;;(setq comment-start "#")
  ;; (setq comment-end "")

  )

;;;###autoload
(add-to-list 'auto-mode-alist '("variables.txt" . equations-mode))

;; add the mode to the `features' list
(provide 'equations-mode)

;; Local Variables&#58;
;; coding: utf-8
;; End:

;;; fasta-mode.el ends here
