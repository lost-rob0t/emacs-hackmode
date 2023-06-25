;;; chef.el --- Like cyber Chef but in emacs lisp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/lost-rob0t>
;; Maintainer:  <unseen@hunter-02>
;; Created: June 04, 2023
;; Modified: June 04, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/chef
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Like cyber Chef but in emacs lisp
;;
;;; Code:


;; https://stackoverflow.com/questions/611831/how-can-i-url-decode-a-string-in-emacs-lisp
(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun chef/url-encdoe-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun chef/url-decode-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun chef/url-decode-region-kill (start end)
  "de-urlencode the region between START and END in current buffer and kill the result."
  (interactive "r")
  (kill-new (func-region start end #'url-unhex-string)))

(defun chef/url-encode-region-kill (start end)
  "de-urlencode the region between START and END in current buffer and kill the result."
  (interactive "r")
  (kill-new (func-region start end #'url-unhex-string)))


(defun chef/b64-decode-region (start end)
  "decode base64 the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'base64-decode-string))
(defun chef/b64-encode-region (start end)
  "base64 encode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'base64-encode-string))

(defun chef/b64-encode-kill (start end)
  "base64 encode the region between START and END in current buffer and kill the result."
  (interactive "r")
  (kill-new (func-region start end #'base64-encode-string)))

(defun chef/b64-decode-kill (start end)
  "base64 decode the region between START and END in current buffer and kill the result."
  (interactive "r")
  (kill-new (func-region start end #'base64-decode-string)))



(defun chef/b64-encode-from-kill ()
  "base64 encode the current kill item and encode the result. Also adds it to the kill ring"
  (interactive)
  (kill-new (base64-encode-string (car kill-ring))))

(defun chef/b64-decode-from-kill ()
  "base64 decode the current kill item and decode the result. Also adds it to the kill ring"
  (interactive)
  (kill-new (base64-encode-string (car kill-ring))))







(provide 'chef)
;;; chef.el ends herE
