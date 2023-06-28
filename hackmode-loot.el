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
(require 'hackmode)
(defun hackmode-get-loot-file (name)
  "return the path to the lootfile in the operation path."
  (f-join (hackmode-get-operation-path name) ".loot.lisp"))




(defvar hackmode-loot (list (cons 'creds nil) (cons 'urls nil) (cons 'emails nil))
  "The list of loot that has been in found. includes loot from other operations.")

(defun hackmode-save-loot-data ()
  "Save loot to 'hackmode-loot-file'"
  (interactive)
  (with-temp-file (hackmode-get-loot-file hackmode-operation)
    (prin1 hackmode-loot (current-buffer))))

(defun hackmode-load-loot-data ()
  "Read an S-expression from a file and return it as a value."
  (with-temp-buffer
    (insert-file-contents (hackmode-get-loot-file hackmode-operation))
    (car (read-from-string (buffer-string)))))

(defun hackmode-loot-format-data (key value &optional note)
  "Format a loot VALUE to be saved as KEY in loot file. NOTE is optional."
  (list key (list :note note :value value)))

(defun hackmode-insert-loot (topic key value &optional note)
  "Insert a piece of loot if it does not exist already."
  (cl-pushnew (hackmode-loot-format-data key value note) (alist-get topic hackmode-loot))
  (hackmode-save-loot-data))



;; https://www.reddit.com/r/emacs/comments/10xhvd8/comment/j7tqsgn
(defun hackmode-loot-read-note ()
  "Read a note."
  (let ((delta 20))
    (window-resize (minibuffer-window) delta)
    (read-string "Note: ")))

(defun hackmode-loot-save-password ()
  "Save a credential to the loot file."
  (let* ((data (split-string (current-kill 0) ":"))
         (user (read-string "Enter User: " (if (> (length data) 1) (nth 0 data))))
         (password (read-string "Enter password: " (if (> (length data) 1) (nth 1 data))))
         (note  (if (yes-or-no-p "Enter a note?" ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'creds user password note)))




(defun hackmode-loot-save-url ()
  "Save a url to the loot file."
  (let* ((data (current-kill 0))
         (url (read-string "Enter url to save: " (if (org-url-p data) data)))

         (note  (if (yes-or-no-p "Enter a note?: " ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'urls (md5 url) url note)))
(defun hackmode-loot-save-host ()
  "Save a host to the loot file."
  (let ((addr (read-string "Enter host address: " (current-kill 0)))

        (note  (if (yes-or-no-p "Enter a note?: " ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'hosts (md5 addr) addr note)))


(defcustom hackmode-loot-menu '(("Credential" . hackmode-loot-save-password)
                                ("url" . hackmode-loot-save-url)
                                ("host" . hackmode-loot-save-host)
                                ("email" . hackmode-loot-save-email))
  "Alist of things to save to the loot file. The the menu values must be functions.")

(defun hackmode-save-loot ()
  "Save a piece of loot."
  (interactive)
  (hackmode-popup "Capture: " 0 hackmode-loot-menu))

(provide 'hackmode-loot)
;;; hackmode-loot.el ends here
