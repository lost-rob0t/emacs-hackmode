;;; hackmode-shell.el --- The Hackmode Shell -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <unseen@flake>
;; Maintainer:  <unseen@flake>
;; Created: May 22, 2023
;; Modified: May 22, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/hackmode-shell
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The Hackmode Shell
;;
;;; Code:
(require 'eshell)
(require 's)
;; TODO customize the way you send alerts

(defun hackmode-send-notification (title message)
  (call-process (executable-find "dunstify") nil nil nil title message))

(defun eshell-p ()
  "Eshell predicate."
  (if (eq major-mode 'eshell-mode)
      t))

(defmacro without-eshell (&rest body)
  "Execute BODY unless running in an eshell buffer."
  (declare (indent 0))
  `(unless (not (eq major-mode 'eshell-mode))
     ,@body))

(defun hackmode-shell-args-prompt (args &optional output-flags output-path default-arguments)
  "Check if in eshell buffer, else prompt prompt for output and arguments."
  (interactive)
  (let ((tmp-args (if (not (eshell-p)) (append (list output-flags output-path (read-string "Arguments: " default-arguments)) ) args)))
    tmp-args))


    
    
(provide 'hackmode-shell)
;;; hackmode-shell.el ends here
