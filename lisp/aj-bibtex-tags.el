;; Author: Amogh Jalihal
;; Date: 5th March 2020
;; Commentary:
;; Utility function to presenting a prompt to pick
;; preexisting tags from bib file

(defcustom bibfile "~/jalihal_projects/Research/references.bib"
  "Default path to bib file")

(defcustom buffername "*my-bibtex-tags*"
  "name of shell output capture buffer")

(defun choose-tags ()
  "Make an interface to choose tags in bibtex"
  (interactive)
  (let ((b (get-buffer-create buffername))
        (all-tags ()))
    (call-process "grep" bibfile buffername nil "tags")
    (with-current-buffer buffername
      (dolist (line (split-string (buffer-string) "\n"))
        (when (> (length line) 1)
          (setq current-tags (get-tags-from-line line))
          (setq all-tags (append all-tags current-tags)))
        ))
    (setq all-tags (delete-dups all-tags))
    (insert (completing-read "Choose tag: " all-tags))
    (kill-buffer buffername)
    ))

(defun get-tags-from-line (line)
  "extract tags from LINE"
  (setq start-tags (1+ (string-match "\\({\\)" line)))
  (setq end-tags (string-match "\\(}\\)" line))
  (setq current-tags (split-string (substring
                                    line
                                    start-tags
                                    end-tags)
                                   ","))
  (setq cleaned-tags ())
  (dolist (tag current-tags)
    (setq cleaned-tags (cons (split-string tag) cleaned-tags)))
  cleaned-tags)

(define-key bibtex-mode-map (kbd "\C-ct") 'choose-tags)
