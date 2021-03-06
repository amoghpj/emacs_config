;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("pysrc" "#+BEGIN_SRC python :results output :async t\n$0\n#+END_SRC" "pysrc" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/pysrc" nil nil)
                       ("ipy" "#+BEGIN_SRC ipython :session :exports results :results raw drawer :async t\n  $0\n#+END_SRC" "ipython block" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/ipython" nil nil)
                       ("hpl" "#+latex:" "hplatex" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/hplatex" nil nil)
                       ("hpal" "#+attr_latex: " "hpattrlatex" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/hpattrlatex" nil nil)
                       ("elispsrc" "#+BEGIN_SRC emacs-lisp :results raw\n\n#+END_SRC" "elispsrc block" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/elisp_src_block" nil nil)
                       ("dnit" "\\item[{\\dn $1}] $2" "dnit" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/dnit" nil nil)
                       ("atlat" "#+ATTR_LATEX: :width \\textwidth" "atlat" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/atlat" nil nil)
                       ("relfilepath" "`(concat \"~\" (string-remove-prefix  (getenv \"PWD\") default-directory))`" "Relative File Path" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/RelativeFilePath" nil nil)
                       ("modantemppy" "* META \nName: Amogh Jalihal\nDate: `(current-time-string)`\n\n** Import Headers\n\n#+BEGIN_SRC python :session :exports both :results raw drawer :async t\nimport sys\nimport os\nimport matplotlib.pyplot as plt\nimport matplotlib\nmatplotlib.rcParams['axes.facecolor'] = (1,1,1,0)\nPATH = \"/jalihal_projects/Research/data/ModelAnalysis/\"\nHOME = os.path.expanduser(\"~\")\n\nsys.path.append(HOME + PATH)\nos.chdir(HOME + PATH)\nimport modelreader as md\nimport simulator as sim\n#+END_SRC\n* Analysis\n** Objective" "Model Analysis Template - Py" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/Model_Analysis_Template_Py" nil nil)
                       ("modantemp" "* META \nName: Amogh Jalihal\nDate: `(current-time-string)`\n\n** Import Headers\n\n#+BEGIN_SRC ipython :session :exports both :results raw drawer :async t\n%matplotlib inline\nimport sys\nimport os\nimport matplotlib.pyplot as plt\nimport matplotlib\nmatplotlib.rcParams['axes.facecolor'] = (1,1,1,0)\nPATH = \"/jalihal_projects/Research/data/ModelAnalysis/\"\nHOME = os.path.expanduser(\"~\")\n\nsys.path.append(HOME + PATH)\nos.chdir(HOME + PATH)\nimport modelreader as md\nimport simulator as sim\n#+END_SRC\n* Analysis\n** Objective" "Model Analysis Template" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/Model_Analysis_Template" nil nil)
                       ("analysistemp" "* META \nName: Amogh Jalihal\nDate: `(current-time-string)`\n\n** Import Headers\n\n#+BEGIN_SRC ipython :session :exports both :results raw drawer :async t\n%matplotlib inline\nimport matplotlib.pyplot as plt\nimport matplotlib\nmatplotlib.rcParams['axes.facecolor'] = (1,1,1,0)\n#+END_SRC\n* Analysis\n** Objective" "Analysis Template - Generic" nil nil nil "/home/jalihal/.emacs.d/snippets/org-mode/Analysis_Template" nil nil)))


;;; Do not edit! File generated at Sun Mar 29 12:11:45 2020
