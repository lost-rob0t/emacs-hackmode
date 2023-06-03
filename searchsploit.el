;;; searchsploit.el --- Search for exploits from emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <https://github.com/unseen>
;; Maintainer:  <unseen@hunter-02>
;; Created: May 25, 2023
;; Modified: May 25, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/unseen/searchsploit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Search for exploits from emacs
;;
;;; Code:
(defvar hackmode-searchploit-path (executable-find "searchsploit")
  "Path to the searchsploit tool.")
(defvar searchsploit-input ""
  "Current search input")
(defconst searchsploit-buffer "*SearchSploit"
  "Name of buffer")


(defgroup searchsploit-faces nil
  "Faces used in Deft mode"
  :group 'searchsploit
  :group 'faces)

(defface searchsploit-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Deft header."

  :group 'searchsploit-faces)
(defun searchsploit-buffer-visible-p ()
  "Return Non nil if window is displaying searchsploit-buffer"
  (get-buffer-window searchsploit-buffer))

(defun searchsploit-header ()
  "Insert the searchsploit search header."
  (insert (propertize (format "Search: %s" searchsploit-input) 'face 'searchsploit-header-face))
  (insert "\n"))
    




(defun searchsploit-increment-search ()
  "Add a char to the search."
  (interactive)
  (let ((char last-command-event))))


(defun searchsploit-search-db (term)
  "Search Searchsploit for exploits or papers for TERM.
Returns an alist."
  (json-parse-string (shell-command-to-string (format "%s -j %s" hackmode-searchploit-path term)) :object-type 'alist))

(defun searchsploit--exploits (result)
  (cdr (assoc 'RESULTS_EXPLOIT result)))
(defun searchsploit--papers (result)
  (cdr (assoc 'RESULTS_PAPER result)))

(defun searchsploit--shellcodes (result)
  (cdr (assoc 'RESULTS_SHELLCODE result)))



(defun searchsploit-get-id (result)
  (cdr (assoc 'EDB-ID result)))

(defun searchsploit-get-title (result)
  (cdr (assoc 'Title result)))


(defun searchsploit-get-path (result)
  (cdr (assoc 'Path result)))

(defun searchsploit-get-type (result)
  (cdr (assoc 'Type result)))

(defun searchsploit-get-platform (result)
  (cdr (assoc 'Platform result)))

(defun searchsploit-get-date (result)
  (cdr (assoc 'Date_Updated result)))

(defun searchsploit-get-author (result)
  (cdr (assoc 'Author result)))






(defun searchsploit--create-result (result)
  "Create and attach properties to the result text"
  (let* ((id (searchsploit-get-id result))
         (author (searchsploit-get-author result))
         (title (searchsploit-get-title result))
         (type (searchsploit-get-type result))
         (date (searchsploit-get-date result))
         (line (format "%-4s %s %s %s %s"
                       id (propertize title 'face 'bold) author date type)))
    (save-excursion
      (let ((start (point)))
        (insert line)
        (newline)
        (add-text-properties start (point) (list 'id id))))
    (forward-line)))

(defun searchsploit--insert-results (results)
  "Insert the results into the searchsploit buffer."
  (cl-map 'vector #'(lambda (result) (searchsploit--create-result result)) (searchsploit--exploits results)))

(defun searchsploit-buffer--setup ()
  "Setup the searchsploit-buffer"
  (let* ((results (searchsploit-search-db searchsploit-input)))

   (with-current-buffer (get-buffer-create searchsploit-buffer)

     (erase-buffer)
     (searchsploit-header)
     (searchsploit--insert-results results)
     (switch-to-buffer searchsploit-buffer)

     (goto-char 1))))




  


(defun searchsploit-search ()
  "Search the exploit database."
  (interactive)
  (with-current-buffer (get-buffer-create searchsploit-buffer)

    (setq searchsploit-input (read-string "Enter search term: "))
    (searchsploit-buffer--setup)))



(defvar searchsploit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'searchsploit-search)
    map))

(defun searchsploit-mode ()
  "Major mode for searchsploit"
  (interactive)
  (kill-all-local-variables)
  (searchsploit-buffer--setup)
  (use-local-map searchsploit-mode-map)
  (setq major-mode 'searchsploit-mode)
  (setq mode-name "SearchSploit")
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun searchsploit ()
  "search for exploits from emacs"
  (interactive)
  (searchsploit-mode))


(provide 'searchsploit)
;;; searchsploit.el ends here
