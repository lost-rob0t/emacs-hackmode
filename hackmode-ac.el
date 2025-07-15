;;; bbrf-autocomplete.el --- Autocomplete for BBRF targets and hosts -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nsaspy
;; Keywords: convenience, completion, security, osint
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This package provides autocompletion for BBRF (Bug Bounty Reconnaissance Framework)
;; targets, hosts, and IPs. It caches data from hackmode-bbrf and provides
;; filtering completion for OSINT and penetration testing workflows.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'hackmode)
(defgroup hackmode-autocomplete nil
  "Autocomplete for BBRF targets and hosts."
  :group 'convenience
  :prefix "hackmode-ac-")

(defcustom hackmode-ac-hackmode-path
  (or (getenv "HACKMODE_PATH")
      (expand-file-name "~/.hackmode"))
  "Path to hackmode directory."
  :type 'string
  :group 'hackmode-autocomplete)

(defcustom hackmode-ac-cache-duration 300
  "Cache duration in seconds (default: 5 minutes)."
  :type 'integer
  :group 'hackmode-autocomplete)

(defcustom hackmode-ac-completion-styles '(domains ips urls services)
  "Types of completion data to include."
  :type '(repeat (choice (const domains)
                         (const ips)
                         (const urls)
                         (const services)))
  :group 'hackmode-autocomplete)

(defvar hackmode-ac-cache nil
  "Cache for BBRF data.")

(defvar hackmode-ac-cache-timestamp nil
  "Timestamp of last cache update.")

(defvar hackmode-ac-completion-data nil
  "Processed completion data.")

(defun hackmode-ac-cache-valid-p ()
  "Check if cache is still valid."
  (and hackmode-ac-cache-timestamp
       (< (- (float-time) hackmode-ac-cache-timestamp)
          hackmode-ac-cache-duration)))

(defun hackmode-ac-get-targets-file ()
  "Get path to targets.txt file."
  (expand-file-name "targets.txt" (hackmode-get-config-path hackmode-operation)))

(defun hackmode-ac-read-targets-file ()
  "Read targets from hackmode targets.txt file."
  (let ((targets-file (hackmode-ac-get-targets-file)))
    (when (file-exists-p targets-file)
      (with-temp-buffer
        (insert-file-contents targets-file)
        (split-string (buffer-string) "\n" t)))))



(defun hackmode-ac-get-bbrf-data ()
  "Retrieve data from bbrf."
  (let ((data (make-hash-table :test 'equal)))

    (when (memq 'domains hackmode-ac-completion-styles)
      (let ((domains (hackmode-bbrf-list-domains)))
        (when domains
          (puthash 'domains domains data))))

    (when (memq 'ips hackmode-ac-completion-styles)
      (let ((ips (hackmode-bbrf-list-ips)))
        (when ips
          (puthash 'ips ips data))))

    (when (memq 'urls hackmode-ac-completion-styles)
      (let ((urls (hackmode-bbrf-list-urls)))
        (when urls
          (puthash 'urls urls data))))

    (when (memq 'services hackmode-ac-completion-styles)
      (let ((services (hackmode-bbrf-list-services)))
        (when services
          (puthash 'services services data))))

    data))

(defun hackmode-ac-process-bbrf-entry (entry type)
  "Process a single BBRF entry based on type."
  (cond
   ((eq type 'domains)
    (if (hash-table-p entry)
        (gethash "domain" entry)
      entry))
   ((eq type 'ips)
    (if (hash-table-p entry)
        (gethash "ip" entry)
      entry))
   ((eq type 'urls)
    (if (hash-table-p entry)
        (gethash "url" entry)
      entry))
   ((eq type 'services)
    (if (hash-table-p entry)
        (format "%s:%s"
                (gethash "ip" entry)
                (gethash "port" entry))
      entry))
   (t (format "%s" entry))))

(defun hackmode-ac-flatten-data (data)
  "Flatten and process BBRF data into completion candidates."
  (let ((candidates '()))
    (maphash (lambda (type entries)
               (let ((entry-list (cond
                                  ((vectorp entries) (append entries nil))
                                  ((listp entries) entries)
                                  (t (list entries)))))
                 (dolist (entry entry-list)
                   (let ((processed (hackmode-ac-process-bbrf-entry entry type)))
                     (when (and processed (stringp processed))
                       (push (cons processed type) candidates))))))
             data)

    (let ((seen (make-hash-table :test 'equal))
          (unique-candidates '()))
      (dolist (candidate candidates)
        (let ((key (car candidate)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push candidate unique-candidates))))
      unique-candidates)))

(defun hackmode-ac-update-cache ()
  "Update the cache with fresh data."
  (message "Updating BBRF cache...")
  (let ((targets-data (hackmode-ac-read-targets-file))
        (bbrf-data (hackmode-ac-get-bbrf-data)))

    (when targets-data
      (let ((target-entries (mapcar (lambda (target)
                                      (cons target 'targets))
                                    targets-data)))
        (setq hackmode-ac-completion-data
              (append target-entries
                      (hackmode-ac-flatten-data bbrf-data)))))

    (unless targets-data
      (setq hackmode-ac-completion-data (hackmode-ac-flatten-data bbrf-data)))

    (setq hackmode-ac-cache-timestamp (float-time))
    (message "BBRF cache updated (%d entries)" (length hackmode-ac-completion-data))))

(defun hackmode-ac-get-completion-data ()
  "Get completion data, updating cache if necessary."
  (unless (hackmode-ac-cache-valid-p)
    (hackmode-ac-update-cache))
  hackmode-ac-completion-data)

(defun hackmode-ac-completion-function (string predicate action)
  "Completion function for BBRF targets."
  (let ((candidates (hackmode-ac-get-completion-data)))
    (cond
     ((eq action 'metadata)
      '(metadata (category . bbrf-target)
        (annotation-function . hackmode-ac-annotate)))
     ((eq action 'lambda)
      (test-completion string candidates predicate))
     ((eq action t)
      (all-completions string candidates predicate))
     ((eq action nil)
      (try-completion string candidates predicate)))))

(defun hackmode-ac-annotate (candidate)
  "Annotate completion candidate with type information."
  (let* ((data (hackmode-ac-get-completion-data))
         (entry (assoc candidate data)))
    (when entry
      (format " [%s]" (cdr entry)))))

(defun hackmode-ac-completing-read (prompt &optional initial-input)
  "Read a target using completion."
  (let ((completion-extra-properties
         '(:annotation-function hackmode-ac-annotate)))
    (completing-read prompt #'hackmode-ac-completion-function
                     nil nil initial-input)))

;;;###autoload
(defun hackmode-ac-insert-target ()
  "Insert a target at point using completion."
  (interactive)
  (let ((target (hackmode-ac-completing-read "Target: ")))
    (when target
      (insert target))))

;;;###autoload
(defun hackmode-ac-copy-target ()
  "Copy a target to kill ring using completion."
  (interactive)
  (let ((target (hackmode-ac-completing-read "Copy target: ")))
    (when target
      (kill-new target)
      (message "Copied: %s" target))))

;;;###autoload
(defun hackmode-ac-refresh-cache ()
  "Manually refresh the BBRF cache."
  (interactive)
  (setq hackmode-ac-cache-timestamp nil)
  (hackmode-ac-update-cache))

(defun hackmode-ac-company-backend (command &optional arg &rest ignored)
  "Company backend for BBRF completion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'hackmode-ac-company-backend))
    (prefix (and (derived-mode-p 'text-mode 'prog-mode)
                 (company-grab-symbol)))
    (candidates (let ((candidates (hackmode-ac-get-completion-data)))
                  (cl-remove-if-not
                   (lambda (candidate)
                     (string-prefix-p arg (car candidate)))
                   candidates)))
    (annotation (hackmode-ac-annotate arg))
    (meta (format "BBRF target: %s" arg))))

(defvar hackmode-ac-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b i") 'hackmode-ac-insert-target)
    (define-key map (kbd "C-c b c") 'hackmode-ac-copy-target)
    (define-key map (kbd "C-c b r") 'hackmode-ac-refresh-cache)
    map)
  "Keymap for BBRF autocomplete mode.")

;;;###autoload
(define-minor-mode hackmode-ac-mode
  "Minor mode for BBRF target autocompletion."
  :lighter " BBRF"
  :keymap hackmode-ac-mode-map
  (when hackmode-ac-mode
    (hackmode-ac-update-cache)))

;;;###autoload
(defun hackmode-ac-setup ()
  "Setup BBRF autocomplete globally."
  (interactive)
  (hackmode-ac-mode 1)
  (when (featurep 'company)
    (add-to-list 'company-backends 'hackmode-ac-company-backend)))

(provide 'hackmode-autocomplete)
;;; hackmode-autocomplete.el ends here
