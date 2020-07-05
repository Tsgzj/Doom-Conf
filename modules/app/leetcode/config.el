;;; app/leetcode/config.el -*- lexical-binding: t; -*-
(use-package! leetcode)

(map! :leader
      (:prefix-map ("a L" . "Leetcode")
       :desc "Leetcode"             "l" #'leetcode
       :desc "Show current problem" "s" #'leetcode-show-current-problem
       :desc "Refresh"              "r" #'leetcode-refresh
       :desc "Try the solution"     "t" #'leetcode-try
       :desc "Submit the solution"  "u" #'leetcode-submit))
