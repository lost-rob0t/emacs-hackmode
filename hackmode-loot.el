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
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Store Loot and other stuff you find.
;;
;;; Code:

(require 'f)
(defcustom hackmode-loot-file (f-expand "./loot.lisp")
  "Path to the loot file where `loot` will be stored.")

(defvar hackmode-loot (list (cons 'creds nil) (cons 'urls nil) (cons 'emails nil))
  "The list of loot that has been in found. includes loot from other operations.")

(defun hackmode-save-loot ()
  "Save loot to 'hackmode-loot-file'"
  (interactive)
  (with-temp-file hackmode-loot-file
    (prin1 hackmode-loot (current-buffer))))

(defun read-s-expression-from-file (filename)
  "Read an S-expression from a file and return it as a value."
  (with-temp-buffer
    (insert-file-contents filename)
    (car (read-from-string (buffer-string)))))

(defun hackmode-loot-format-data (key value &optional note)
  "Format a loot VALUE to be saved as KEY in loot file. NOTE is optional."
  (list key (list :note note :value value)))

(defun hackmode-insert-loot (topic key value &optional note)
  "Insert a piece of loot if it does not exist already."
  (cl-pushnew (hackmode-loot-format-data key value note) (alist-get topic hackmode-loot)))

(provide 'hackmode-loot)
;;; hackmode-loot.el ends here
