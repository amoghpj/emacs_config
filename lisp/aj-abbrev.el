(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ;; Queen's English abbreviations
    ("cant" "cannot")
    ("dont" "do not")
    ("wont" "will not")

    ;; Biology
    ("sce" "Saccharomyces cerevisiae")

    ;; Modeling
;;    ("ss" "steady state")
    ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("inp" "IN-PROGRESS")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
