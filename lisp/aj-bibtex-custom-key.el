;; Author: Amogh Jalihal
;; Date: 8th March 2020
;; Commentary:
;; Provides interactive function 'aj/bibtex-make-key' that
;; creates a new bib key following the format YEAR_SHORTJOURNALNAME_LASTNAME_SELECTEDKEYSWORDS
;; The user is prompted for SELECTEDKEYSWORDS using 'completing-read-multiple'

(defun aj/bibtex-make-key ()
  (interactive)
  (save-excursion
    (search-backward "@")
    (beginning-of-line)
    (setq start-entry (point))  
    (setq aj/separator "_" )
    (setq aj/bib-entry (bibtex-parse-entry))
    ;; Author
    (setq aj/author-list-string (remove-curlies (cdr (assoc "author" aj/bib-entry))))
    (setq aj/first-author (downcase
                           (car (last (split-string
                                       (car
                                        (split-string aj/author-list-string
                                                      " and "))
                                       " ")))))
    ;; Journal
    (setq aj/journal "")
    (setq aj/journal-terms
          (split-string (remove-curlies
                         (cdr (assoc "journal" aj/bib-entry)))
                        " "))
    (dolist (my-term aj/journal-terms)
      (if (or (string= (downcase my-term) "journal")
              (string= (downcase my-term) "theoretical"))
          (setq aj/journal (concat aj/journal (substring (downcase my-term) 0 1)))
        (progn
          (if (> (length my-term) 3)
              (setq aj/journal (concat aj/journal
                                       (substring (downcase my-term) 0 3)))))))
    ;; year
    (setq aj/year (cdr (assoc "year" aj/bib-entry)))
    ;; keywords
    (setq aj/title-string (remove-curlies (cdr (assoc "title" aj/bib-entry))))
    (setq aj/keywords-selection (completing-read-multiple "Select relevant: " (split-string aj/title-string " ")))
    (setq aj/keywords "")
    (setq aj/keywords-final ())
    (dolist (kw aj/keywords-selection)
      (setq aj/keywords-final (cons (downcase kw) aj/keywords-final)))
    (setq aj/keywords-final (reverse aj/keywords-final))
    (setq aj/keywords (string-join aj/keywords-final "_"))
    ;; Make the new bib key
    (setq aj/bib-key (format  "%s_%s_%s_%s" aj/year  aj/journal  aj/first-author  aj/keywords))
    ;; Now go to the beggining of the entry, kill the old key, and insert the new one
    (goto-char start-entry)
    (setq begkey (1+ (string-match "\\({\\)" (thing-at-point 'line))))
    (setq endkey  (string-match "\\(,\\)" (thing-at-point 'line)))
    (kill-region (+ start-entry begkey) (+ start-entry endkey))
    (goto-char (+ start-entry begkey))
    (insert aj/bib-key)))


(defun remove-curlies (somestr)
  (interactive)
  (setq cleanedstr "")
  (setq cleanedstr (replace-regexp-in-string "{" "" somestr))
  (setq cleanedstr (replace-regexp-in-string "}" "" cleanedstr))
  cleanedstr)


(define-key bibtex-mode-map (kbd "\C-ck") 'aj/bibtex-make-key)
