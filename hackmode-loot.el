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
(defun hackmode-get-loot-file (name)
  "return the path to the lootfile in the operation path."
  (f-join (hackmode-get-operation-path name) ".loot.lisp"))

(defvar hackmode-loot (list (cons 'creds nil) (cons 'urls nil) (cons 'emails nil))
  "The list of loot that has been in found. includes loot from other operations.")

(defun hackmode-init-loot-file (name)
  "Create The loot file if it does not exist"
  (if (f-exists-p (hackmode-get-loot-file name))
      nil
    (progn
      (f-touch (hackmode-get-loot-file name))
      (hackmode-save-loot-data))))


(defun hackmode-save-loot-data ()
  "Save loot to 'hackmode-loot-file'"
  (interactive)
  (with-temp-file (hackmode-get-loot-file hackmode-operation)
    (prin1 hackmode-loot (current-buffer))))

(defun hackmode-load-loot-data ()
  "Read an S-expression from the loot file."
  (interactive)
  (with-temp-buffer
    (insert-file-contents (hackmode-get-loot-file hackmode-operation))
    (setq hackmode-loot (car (read-from-string (buffer-string))))))



(defun hackmode-loot-format-data (key value &optional note)
  "Format a loot VALUE to be saved as KEY in loot file. NOTE is optional."
  (list key (list :note note :value value)))

(defun hackmode-loot-get-topic-keys (topic)
  "Return all keys for given loot TOPIC."
  (mapcar #'car  (cdr (assoc topic hackmode-loot))))

(defun hackmode-loot-get-key (topic key)
  (assoc key  (cdr (assoc topic hackmode-loot))))


 

(defun hackmode-insert-loot (topic key value &optional note)
  "Insert a piece of loot if it does not exist already."
  (cl-pushnew (hackmode-loot-format-data key value note) (alist-get topic hackmode-loot))
  (hackmode-save-loot-data))
;; TODO Fix this
(defun hackmode-loot-update (topic key value &optional note)
  "Update loot under TOPIC with new VALUE"
  (let* ((topics (copy-alist (alist-get topic hackmode-loot)))
         (keys (assoc-delete-all key topics)))
    (push (hackmode-loot-format-data key value) topics)
    (assoc-delete-all topic hackmode-loot)
    (push (cons topic topics) hackmode-loot)
    (hackmode-save-loot-data)))




;; https://www.reddit.com/r/emacs/comments/10xhvd8/comment/j7tqsgn
(defun hackmode-loot-read-note ()
  "Read a note."
  (let ((delta 20))
    (window-resize (minibuffer-window) delta)
    (read-string "Note: ")))

(defun hackmode-loot-save-password ()
  "Save a credential to the loot file."
  (interactive)
  (let* ((data (split-string (current-kill 0) ":"))
         (user (read-string "Enter User: " (if (> (length data) 1) (nth 0 data))))
         (password (read-string "Enter password: " (if (> (length data) 1) (nth 1 data))))
         (note  (if (yes-or-no-p "Enter a note?" ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'creds user password note)))




(defun hackmode-loot-save-url ()
  "Save a url to the loot file."
  (interactive)
  (let* ((data (current-kill 0))
         (url (read-string "Enter url to save: " (if (org-url-p data) data)))

         (note  (if (yes-or-no-p "Enter a note?: " ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'urls url nil note)))

(defun hackmode-loot-save-email ()
  "Save a url to the loot file."
  (interactive)
  (let* ((data (current-kill 0))
         (email (read-string "Enter Email to save: " (if (not (null data)) data)))

         (note  (if (yes-or-no-p "Enter a note?: " ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'emails email nil note)))




(defun hackmode-loot-save-user ()
  "Save a url to the loot file."
  (interactive)
  (let* ((data (current-kill 0))
         (user (read-string "Enter Email to save: " (if (not (null data)) data)))

         (note  (if (yes-or-no-p "Enter a note?: " ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'users user nil note)))





(defun hackmode-loot-save-host ()
  "Save a host to the loot file."
  (interactive)
  (let ((addr (read-string "Enter host address: " (current-kill 0)))

        (note  (if (yes-or-no-p "Enter a note?: " ) (hackmode-loot-read-note) "")))
    (hackmode-insert-loot 'hosts addr nil note)))


(defun hackmode-loot-save-port ()
  (interactive)
  (let ((addr (completing-read "Select Host: " (hackmode-get-topic-keys)))
        (port (read-string "Port Number: " "80")))
    (hackmode-insert-loot 'hosts)))

(defcustom hackmode-loot-menu '(("Credential" . hackmode-loot-save-password)
                                ("url" . hackmode-loot-save-url)
                                ("host" . hackmode-loot-save-host)
                                ("post" . hackmode-loot-save-port)
                                ("post" . hackmode-loot-save-user)
                                ("email" . hackmode-loot-save-email))
  "Alist of things to save to the loot file. The the menu values must be functions.")

(defun hackmode-save-loot ()
  "Save a piece of loot."
  (interactive)
  (hackmode-popup "Capture: " 0 hackmode-loot-menu))



(provide 'hackmode-loot)
;;; hackmode-loot.el ends here
