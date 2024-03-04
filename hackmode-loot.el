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
;; Package-Requires: ((emacs "28.2"))
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


(setq hackmode-capture-templates '(("d" "default" entry (file+headline "operation.org" "ScratchPad") "* %<%Y>\n** %<%m %B>\n*** %<%d %A>\n %?" :clock-keep :clock-in)
                                   ("l" "loot")
                                   ("lu" "Capture a a user" entry (file+olp "operation.org" "Loot" "Users") "*** %T\n %^{username}p %^{password}p\n**** Notes\n %?")
                                   ("li" "Capture a a ip/host" entry (file+olp "operation.org" "Loot" "Hosts") "*** %T\n %^{hostname}p %?")
                                   ("lu" "program url" entry (file+headline "operation.org" "Loot" "URL") "** IDEA %^g %^{url}\n %^{server}p\n %^{alive}")
                                   ("lt" "program tech " entry (file+olp "operation.org" "Loot" "Tech") "** %T %^{url} %^{tech-name} " :kill-buffer t)
                                   ("r" "reports")
                                   ("rd" "Generate a report for the day." entry (file+headline "operation.org" "reports") "** %T\n %(hackmode-loot-capture-metadata) %?")
                                   ("rb" "Generate a report for a bug." entry (file+headline "operation.org" "bugs") "** %T\n %^{bug-type}p\n%^{cvss-score}p\n*** description\n %?\n*** steps to reproduce %i")
                                   ("p" "programs")
                                   ("pr" "Program Rss" entry (file+headline "operation.org" "RSS") "** %^{rss url} :elfeed: %^g")
                                   ("pn" "program notes" entry (file+headline "operation.org" "notes") "** %T\n %x")
                                   ("pt" "program todos" entry (file+headline "operation.org" "Todo Inbox") "** TODO %? ")
                                   ("ps" "program script" entry (file+headline "operation.org" "Automation") "** %T %^g\n #+NAME:%(read-string \"Enter name of Script\")\n#+BEGIN_SRC sh :async :results output replace :tangle attack.sh\n%x\n#+END_SRC\n%?")))

(defun hackmode-loot-capture-metadata ()
  "Return a string full of org properties for the hackmode-capture."
  (let* ((default-directory (hackmode-get-operation-path hackmode-operation))
         (findings-directory (f-join default-directory "findings/"))
         (org-directory findings-directory)
         (report-file (f-join findings-directory "findings.org"))
         ;; NOTE you need to ensure this file is up to date.
         (total-urls (shell-command-to-string (format "wc -l %s | cut -d ' ' -f 1" (f-join findings-directory "urls.txt"))))
         (total-subdomains (shell-command-to-string (format "wc -l %s | cut -d ' ' -f 1" (f-join findings-directory "subdomains.txt")))))

    (format ":PROPERTIES:\n:sub-domains: %s\n:urls: %s\n:END:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE" total-subdomains total-urls (shell-command-to-string (format  "git diff --staged %s" "findings/subdomains.txt")))))



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


(provide 'hackmode-loot)
;;; hackmode-loot.el ends here
