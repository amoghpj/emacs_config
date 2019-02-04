;;; aj-devanagari-mode.el --- Attempt at syntax highlighting for an devanagaris file

;; Author: Amogh Jalihal
;; Created: 3rd February 2019

;; Commentary:
;; Syntax highlighting

(defgroup devanagari nil
  "Syntax highlighting for a tab separated variables.txt file."
  :prefix "devanagari-"
  )



(define-derived-mode devanagari-mode fundamental-mode "devanagari"
  "Devanagari mode is for editing latex files to be compiled with devnag."
  ;;(latex-mode t)
  (defun devanagari-compile ()
  (interactive)
    "Compile current buffer to produce a tex file, then compile the tex file"
    (set 'devnag-file-name (file-name-nondirectory (buffer-file-name)))
    (set 'latex-file-name (file-name-base (buffer-file-name)))
    (call-process-shell-command
     (concat "devnag " devnag-file-name)
     nil "*Devanagari Shell Command Output*" t
     )
    (call-process-shell-command
     (concat "pdflatex " latex-file-name)
     nil "*Devanagari Shell Command Output*" t
     )
    ;; (shell-command (concat "devnag " devnag-file-name))
    ;; (shell-command (concat "pdflatex " latex-file-name))
    )

  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo))


  )

(defvar devanagari-mode-map nil "keymap for `devanagari-mode'")

(setq devanagari-mode-map (make-sparse-keymap))

(define-key devanagari-mode-map (kbd "C-c C-c") 'devanagari-compile)

;; (defvar devanagari-mode-shared-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "C-c C-c" 'devanagari-compile)    
;;     map)
;;   "Keymap for devanagari mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dn\\'" . devanagari-mode))

;; add the mode to the `features' list
(provide 'devanagari-mode)

;; Local Variables&#58;
;; coding: utf-8
;; End:

;;; devanagari-mode.el ends here
