;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("ipy" "#+BEGIN_SRC ipython :session :exports both :results raw drawer :async t\n  $0\n#+END_SRC" "ipython block" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/ipython" nil nil)
                       ("relfilepath" "`(concat \"~\" (string-remove-prefix  (getenv \"PWD\") default-directory))`" "Relative File Path" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/RelativeFilePath" nil nil)
                       ("modantemp" "* META \nName: Amogh Jalihal\nDate: `(current-time-string)`\n\n** Import Headers\n\n#+BEGIN_SRC ipython :session :exports both :results raw drawer :async t\n%matplotlib inline\nimport sys\nimport os\nimport matplotlib.pyplot as plt\nimport matplotlib\nmatplotlib.rcParams['axes.facecolor'] = (1,1,1,0)\nPATH = \"/jalihal_projects/Research/data/ModelAnalysis/\"\nHOME = os.path.expanduser(\"~\")\n\nsys.path.append(HOME + PATH)\nos.chdir(HOME + PATH)\nimport modelreader as md\nimport simulator as sim\n\n\n\n\n#+END_SRC\n* Analysis\n** Objective" "Model Analysis Template" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/Model_Analysis_Template" nil nil)
                       ("relfilepath" "`(concat \"~\" (string-remove-prefix  (getenv \"PWD\") default-directory))`" "Relative File Path" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/+new-snippet+" nil nil)))


;;; Do not edit! File generated at Sat Apr 21 23:58:49 2018
