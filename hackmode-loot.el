;;; hackmode-loot.el --- Store Loot and other stuff you find. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: June 27, 2023
;; Modified: June 27, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/hackmode-loot
;; Package-Requires: ((emacs "29.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Store Loot and other stuff you find.
;;
;;; Code:

(require 'f)
(require 'elfeed)
(require 'elfeed-org)
(require 'sqlite)

(defvar hackmode-capture-templates '(("d" "default" entry (file+olp+datetree "scratchpad.org" "ScratchPad") "* %<%Y>\n** %<%m %B>\n*** %<%d %A>\n %?" :clock-keep :clock-in :jump-to-captured)
                                     ("l" "loot")
                                     ("lu" "Capture a a user" entry (file+olp "loot.org" "Loot" "Users") "*** %T\n %^{username}p %^{password}p\n**** Notes\n %?")
                                     ("lt" "program tech " entry (file+olp "tech.org" "Loot" "Tech") "** %T %^{url} %^{tech-name} " :kill-buffer t)
                                     ("r" "reports")
                                     ("rd" "Generate a report for the day." entry (file+headline "reports.org" "reports") "** %T\n %(hackmode-loot-capture-metadata) %?")
                                     ("rb" "Generate a report for a bug." entry (file+headline "reports.org" "bugs") "** %T\n %^{bug-type}p\n%^{cvss-score}p\n*** description\n %?\n*** steps to reproduce %i")
                                     ("pr" "Program Rss" entry (file+headline "rss.org" "RSS") "* %^{rss url} :elfeed: %^g")
                                     ("pn" "program notes" entry (file+headline "notes.org" "notes") "** %T\n %x")
                                     ("pt" "program todos" entry (file+headline "todos.org" "Todo Inbox") "** TODO %? ")
                                     ("ps" "program script" entry (file+headline "tasks.org" "Automation") "** %T %^g\n #+NAME:%(read-string \"Enter name of Script\")\n#+BEGIN_SRC sh :async :results output replace :tangle attack.sh\n%x\n#+END_SRC\n%?")))




(defun hackmode-capture ()
  "Capture a thought/data before it is lost to entropy."
  (interactive)
  (let* ((default-directory (hackmode-get-operation-path hackmode-operation))
         (org-directory default-directory)
         ;; NOTE you need to ensure this file is up to date.
         (org-capture-templates hackmode-capture-templates))
    (call-interactively #'org-capture)))

(defun hackmode-rss ()
  "View The rss feeds saved in $HACKMODE_PATH/rss.org."
  (interactive)
  (let ((rmh-elfeed-org-files (list (f-join (hackmode-get-operation-path hackmode-operation) "rss.org"))))
    (elfeed-update)
    (elfeed)))




(provide 'hackmode)
;;; hackmode-loot.el ends here
