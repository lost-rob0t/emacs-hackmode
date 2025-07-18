;;; hackmode.el --- Pentesters Lil lisp redpill -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@fedora.email>
;; Maintainer:  <nsaspy@fedora.email>
;; Created: May 21, 2023
;; Modified: July 14, 2025
;; Version: 0.0.10
;; Keywords: security pentesting reporting automation org
;; Homepage: https://github.com/lost-rob0t/emacs-hackmode
;; Package-Requires: ((emacs "28.2") (emacs-async "1.97") (f "v0.20.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Cyber Security tool automation, asset tracking and more.
;;  BBRF Is currently the standard for asset tracking in this package.
;;
;;
;;; Code:
(require 'async)
;;  TODO Remove f.el deps
(require 'f)
(require 'transient)
(require 'hackmode-tools)



(defcustom hackmode-data-dir (f-expand "~/.local/share/hackmode/")
  "The directory to be used to hold current state stating what and where the current operation is."
  :group 'hackmode
  :type 'string)


(defcustom hackmode-path-file (f-join hackmode-data-dir "op-path") "File with contents pointing to current hackmode path."
  :group 'hackmode
  :type 'string)


(defcustom hackmode-operation-file (f-join hackmode-data-dir "current-op") "File with contents the name of current hackmode op. The contests is just op-name. eg TMobile, forest.htb, ect "
  :group 'hackmode
  :type 'string)


(defcustom hackmode-dir "~/hackmode"
  "The base directory to store operation workspace in."
  :group 'hackmode :type 'string)

(defcustom hackmode-checklists nil "Alist of files . name to be used for checklists."
  :group 'hackmode)


(defcustom hackmode-capture-templates nil "Org Capture templates specific to hackmode,."
  :group 'hackmode
  :type 'list)

(defun hackmode--ensure-data-dir ()
  (unless (f-directory? hackmode-data-dir)
    (f-mkdir hackmode-data-dir)
    (f-touch hackmode-operation-file)
    (f-touch hackmode-path-file)))

(defun hackmode--get-current-operation ()
  "Read the current operation from the `hackmode-operation-file`"
  (hackmode--ensure-data-dir)
  (with-temp-buffer
    (insert-file-contents hackmode-operation-file)
    (buffer-string)))

;;; Common var Setups
(defcustom hackmode-default-operation (hackmode--get-current-operation)
  "The default operation to use."
  :group 'hackmode
  :type 'string)

(defvar hackmode-operation hackmode-default-operation
  "Current operation name. Do not set this, instead use 'hackmode-menu' or 'hackmode-switch-op'.")


(defcustom hackmode-operation-hook nil
  "Hook that runs when the Hackmode operation changes."
  :type 'hook
  :group 'hackmode)


(defcustom hackmode-tools-dir (f-join hackmode-dir ".hackmode-tools/")
  "The Default path where tools to be uploaded will be pulled from."
  :group 'hackmode
  :type 'string)

(defcustom hackmode-interface "tun0"
  "Network interface to use by default."
  :group 'hackmode
  :type 'string)


;;;TODO Suggest skeltor templates maybe?
(defcustom hackmode-templates (f-expand "~/.config/hackmode/templates")
  "Path to templates directory."
  :group 'hackmode
  :type 'list)

(defcustom hackmode-wordlist-dir "~/wordlists/"
  "The directory for where wordlists are stored."
  :group 'hackmode
  :type 'string)

;;; Check Lists
(defun hackmode-create-checklist-entry (checklist-file)
  "Create an entry in the 'check-lists.org' file linking to CHECKLIST-FILE."
  (let* ((todo-file (f-join default-directory "check-lists.org"))
         (entry (format "\n* TODO %s\n  [[file:%s]]" (f-filename checklist-file) checklist-file)))
    (unless (f-exists? todo-file)
      (f-write-text "" 'utf-8 todo-file))
    (with-temp-buffer
      (insert-file-contents todo-file)
      (goto-char (point-max))
      (insert entry)
      (write-region (point-min) (point-max) todo-file))))


(defun hackmode-use-checklist ()
  "Read a target from the user and copy the checklist file."
  (interactive)
  (let* ((target (or hackmode-target (hackmode-select-target)))
         (dir  (or default-directory (hackmode-get-operation-path  hackmode-operation)))
         (file-path (cdr (assoc (completing-read "Select a checklist: " hackmode-checklists nil t) hackmode-checklists)))
         (destination-dir (f-join dir "checklists"))
         (destination-file (f-join destination-dir (concat (string-replace "/" "-"
                                                                           target) "-checklist.org"))))
    (unless (f-exists? destination-dir)
      (f-mkdir destination-dir))
    (f-copy file-path destination-file)
    (hackmode-create-checklist-entry destination-file)
    (message "Checklist for %s copied to %s" target destination-file)))

;;; Hackmode capture from hacmode loot

(defun hackmode-capture ()
  "Capture a thought/data before it is lost to entropy."
  (interactive)
  (let* ((default-directory (hackmode-get-operation-path hackmode-operation))
         (org-directory default-directory)
         ;; NOTE you need to ensure this file is up to date.
         (org-capture-templates hackmode-capture-templates))
    (call-interactively #'org-capture)))

;; Operations and managment functions

(defun hackmode-get-operation-path (operation)
  "Get the full path for a OPERATION."
  (f-full (f-expand (f-join hackmode-dir operation))))

(defun hackmode-create-op-config (operation)
  "Create the config dirs for hackmode OPERATION and gloabl if not exists."
  (let* ((path (hackmode-get-operation-path operation))
         (default-directory path))
    (f-mkdir-full-path hackmode-data-dir)
    (f-mkdir-full-path (f-join path ".config/"))
    (f-mkdir-full-path (f-join path "findings/"))
    (f-touch (f-join path "findings/" ".keep"))))
;;  you can use either file



(defun hackmode-get-finds-path (operation)
  "Get the path to the OPERATION .config/ ."
  (f-join (hackmode-get-operation-path operation) "findings/"))


(defun hackmode-get-config-path (operation)
  "Get the path to the OPERATION .config ."
  (f-join (hackmode-get-operation-path operation) ".config/"))




(defun hackmode-read-config-file (operation filename)
  "Read OPERATION config file."
  (with-temp-buffer (insert-file-contents-literally
                     (f-join (hackmode-get-config-path operation) filename))
                    (buffer-string)))


(defun hackmode-write-operation-config (operation config-filename strings)
  "Write to the OPERATION's config."
  (f-write-text strings 'utf-8 (f-join  (hackmode-get-config-path operation) config-filename)))



(defun hackmode-create-envrc (operation)
  "Creates the .envrc for direnv for OPERATION."
  (let* ((op-path (hackmode-get-operation-path operation))
         (envrc-file (f-expand (f-join op-path ".envrc"))))
    (message "Writing envrc to: %s" envrc-file)
    (f-write-text (format  "export HACKMODE_PATH=%s; export HACKMODE_OP=%s " op-path operation) 'utf-8 envrc-file)))

(defun hackmode-create-targets-file (operation)
  "Create The targets file for OPERATION."
  (let ((targets (f-join (hackmode-get-config-path operation) "targets.txt")))
    (f-touch targets)))

(defun hackmode-set-env (operation)
  "Set env vars for OPERATION."
  (setenv "PATH" (concat (getenv "PATH") ":" (f-full (f-expand hackmode-tools-dir))))
  (setenv "HACKMODE_OP" operation)
  (setenv "HACKMODE_PATH" (hackmode-get-operation-path operation)))

(defun hackmode-set-metadata (operation)
  "Create the metadata for OPERATION."
  (hackmode-create-op-config operation)
  (f-write-text (hackmode-get-operation-path operation) 'utf-8 (f-join hackmode-data-dir "op-path"))
  (f-write-text operation 'utf-8 (f-join hackmode-data-dir "current-op"))
  (hackmode-set-env operation))

(defun hackmode-operations ()
  "Return a list of operations."
  (mapcar #'f-base (f-directories (f-full hackmode-dir))))


;;;###autoload
(defun hackmode-switch-op (&optional op)
  "Switch operation."
  (interactive (list (completing-read "Select operation: " (hackmode-operations) nil nil)))
  (setq hackmode-operation op)
  (hackmode-set-metadata op)
  (run-hooks 'hackmode-operation-hook))

(defun hackmode-new-operation ()
  "Create a new operation to group tasks"
  (interactive)
  (let ((name (read-string "Enter Operation Name: ")))
    (if (file-directory-p (hackmode-get-operation-path name))
        (message (format "operation %s already exists" name))
      (progn
        (make-directory (hackmode-get-operation-path name))
        (message (format "operation %s created" name))))))

;;;###autoload
(defun hackmode-goto-operation ()
  "Go to the operation directory."
  (interactive)
  (find-file (hackmode-get-operation-path hackmode-operation)))

;;; Utils
(defun hackmode--get-interface-ip (interface)
  "Get the IP address of a network interface."
  (let ((output (shell-command-to-string (concat "ip addr show dev " interface " | grep 'inet '"))))
    (when (string-match "\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" output)
      (match-string 1 output))))

(defun hackmode--get-interfaces ()
  "Get a list of network interface names."
  (let ((output (shell-command-to-string "ip addr show | awk '/^[0-9]+:/ {gsub(/:/,\"\"); print $2}'")))
    (split-string output "\n" t)))

(defun hackmode-add-host (hostname address)
  "Add a Host to /etc/hosts"
  (interactive "sEnter hostname: \nEnter IP: ")
  (append-to-file (format "%s\t%s\n" address hostname) nil "/sudo::/etc/hosts"))


(defun hackmode-init-metadata (operation-name)
  "Write the hackmode metadata to ~/.local/share/hackmode/."
  (let ((path (hackmode-get-operation-path operation-name)))
    (f-write-text path 'utf-8)))

(defun hackmode-get-scan-dir (operation)
  "Return The path to the output directoy for scans."
  (f-expand (f-join (hackmode-get-operation-path operation) "scans")))

(defun hackmode--ensure-scans-dir (operation &optional dir)
  "Ensure the the scans folder is created for the hackmode operation."
  (let ((dir (if dir (f-join (hackmode-get-scan-dir operation))) (hackmode-get-scan-dir operation)))
    (condition-case _
        (unless (f-directory? dir)
          (f-mkdir-full-path dir))
      (message "Failed to create scans directory %s: %s" dir (error-message-string err)))))

(defun hackmode-create-output-dir (operation tool-name)
  "Create the output directory for scan output."
  (let ((dir (f-expand (f-join (hackmode-get-scan-dir operation) tool-name (number-to-string (floor (time-to-seconds (current-time))))))))
    (unless (f-exists? dir)
      (f-mkdir-full-path dir))
    dir))

;;;###autoload
(defun hackmode-create-operation ()
  "Interactivly create a operation."
  (interactive)
  (let* ((template (completing-read "Select Template: " (f-directories hackmode-templates)))
         (operation (read-string "Enter Operaton Operation: "))
         (op-path (hackmode-get-operation-path operation)))
    (f-copy template op-path)
    (f-symlink op-path default-directory)
    ;; TODO Move this stuff to a function
    (setf hackmode-operation operation)
    (hackmode-create-envrc operation)
    (hackmode-create-targets-file operation)
    (hackmode-set-metadata operation)
    (run-hooks 'hackmode-operation-hook)
    (hackmode-goto-operation)))

;; TODO allow for buffer local targets like gptel transient menu.

(defvar hackmode-target nil "The currect active host or url or target.")
(defvar hackmode-targets-file "targets.txt"
  "The path to save targets under the HACKMODE_OP/.config/<file>.")





(defun hackmode-select-target ()
  "Select a target from a text-file, defaults to the $HACKMODE_OP/.config/targets.txt file."
  (interactive)
  (let* ((targets-file (read-file-name "Select Targets File: " (hackmode-get-config-path hackmode-operation) hackmode-targets-file)))
    (setf hackmode-target (completing-read "Select Target: " (remove-if #'(lambda (x)
                                                                            (= (length x) 0))
                                                                        (split-string (with-temp-buffer
                                                                                        (insert-file-contents-literally targets-file)
                                                                                        (buffer-string)) "\n"))))))



(defun hackmode-add-target (&optional arg)
  "Prompt to save a target to the targets file."
  (interactive "sEnter Target: ")
  (append-to-file (concat  arg "\n") nil (expand-file-name hackmode-targets-file (hackmode-get-config-path hackmode-operation))))


(defun hackmode-add-targets-region (&optional beg end)
  "Prompt to save a target to the targets file."
  (interactive "r")
  (append-to-file beg (concat end "\n") (expand-file-name hackmode-targets-file (hackmode-get-config-path hackmode-operation))))



(defun hackmode-kill-wordlist ()
  "Copy the path of a wordlist to the kill ring"
  (interactive)
  (kill-new (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))

;; TODO Move this to a yasnippet

(defun hackmode-insert-wordlist ()
  "Copy the path of a wordlist to the kill ring."
  (interactive)
  (insert (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))


(defun hackmode-upload-file ()
  "Copy a wget or curl command that can be used to upload a file."
  (interactive)
  (let* ((filename (f-relative (read-file-name "Tool to download: " hackmode-tools-dir "linpeas.sh") hackmode-tools-dir))
         (i-name (completing-read "interface: " (hackmode--get-interfaces)))
         (ip (hackmode--get-interface-ip i-name))
         (port (read-string "port: " "8000"))
         (download-cmd (read-string "Cmd to use to download: " "wget"))
         (cmd (format "%s http://%s:%s/%s" download-cmd ip port filename)))
    (message cmd)
    (kill-new cmd)
    cmd))

;;; BBRF Asset Tracking.

(defvar hackmode-bbrf-tags nil
  "Current tags to be added to bbrf documents.")

(defvar hackmode-bbrf-last-command nil
  "The last command ran.")

(defun hackmode-bbrf-build-command (sub &rest args)
  "Build the command string to run bbrf commands."
  (string-join
   (append
    '("bbrf")
    (list sub)
    (when (and hackmode-bbrf-tags (member "add" args))
      (cl-loop for (key . val) in hackmode-bbrf-tags
               collect "-t"
               collect (format "%s:%s" key val)))
    args)
   " "))

(defun hackmode-bbrf-execute-command (subcommand &rest args)
  "Execute SUBCOMMAND with optional ARGS (list of strings). If USE-TAGS is non-nil, include tags."
  (let ((command (apply #'hackmode-bbrf-build-command subcommand args)))
    (setq hackmode-bbrf-last-command command)
    (message "%s" command)
    (let ((result (shell-command-to-string command)))
      (string-trim result))))


(defun hackmode-bbrf-list-programs ()
  "Returns a list of programs in bbrf."
  (let ((result (hackmode-bbrf-execute-command "programs")))
    (string-split result)))

(defun hackmode-bbrf-read-program (prompt &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a bbrf program name."
  (completing-read prompt (hackmode-bbrf-list-programs) predicate require-match initial-input hist def inherit-input-method))



(defun hackmode-bbrf-list (type)
  "List assets by TYPE from BBRF. TYPE can be 'domains', 'ips', 'urls', or 'services'."
  (let* ((cmd (hackmode-bbrf-build-command type))
         (output (shell-command-to-string cmd)))
    (if (string-empty-p (string-trim output))
        nil
      (let ((lines (seq-filter (lambda (line) (not (string-empty-p (string-trim line))))
                               (split-string (string-trim output) "\n"))))
        (if (string-equal type "urls")
            (seq-map (lambda (line)
                       (car (split-string line " ")))
                     lines)
          lines)))))

(defun hackmode-bbrf-list-domains ()
  "Get a list of all domains from BBRF."
  (hackmode-bbrf-list "domains"))

(defun hackmode-bbrf-list-ips ()
  "Get a list of all IPs from BBRF."
  (hackmode-bbrf-list "ips"))

(defun hackmode-bbrf-list-urls ()
  "Get a list of all URLs from BBRF."
  (hackmode-bbrf-list "urls"))

(defun hackmode-bbrf-list-services ()
  "Get a list of all services from BBRF."
  (hackmode-bbrf-list "services"))

(defun hackmode-bbrf-list-all-assets ()
  "Get an alist of all assets from BBRF grouped by type."
  (list (cons 'domains (hackmode-bbrf-list-domains))
        (cons 'ips (hackmode-bbrf-list-ips))
        (cons 'urls (hackmode-bbrf-list-urls))
        (cons 'services (hackmode-bbrf-list-services))))

(defun hackmode-bbrf-read-asset (type prompt &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a BBRF asset of TYPE with completion."
  (completing-read prompt
                   (hackmode-bbrf-list type)
                   predicate
                   require-match
                   initial-input
                   hist
                   def
                   inherit-input-method))

(defun hackmode-bbrf-read-domain (prompt &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a domain from BBRF with completion."
  (hackmode-bbrf-read-asset "domains" prompt predicate require-match initial-input hist def inherit-input-method))

(defun hackmode-bbrf-read-ip (prompt &optional predicate require-match initial-input hist def inherit-input-method)
  "Read an IP from BBRF with completion."
  (hackmode-bbrf-read-asset "ips" prompt predicate require-match initial-input hist def inherit-input-method))

(defun hackmode-bbrf-read-url (prompt &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a URL from BBRF with completion."
  (hackmode-bbrf-read-asset "urls" prompt predicate require-match initial-input hist def inherit-input-method))

(defun hackmode-bbrf-read-service (prompt &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a service from BBRF with completion."
  (hackmode-bbrf-read-asset "services" prompt predicate require-match initial-input hist def inherit-input-method))

;; Interactive functions for quick access
(defun hackmode-bbrf-show-domains ()
  "Display all domains in a buffer."
  (interactive)
  (let ((domains (hackmode-bbrf-list-domains)))
    (with-current-buffer (get-buffer-create "*BBRF Domains*")
      (erase-buffer)
      (insert (mapconcat 'identity domains "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun hackmode-bbrf-show-ips ()
  "Display all IPs in a buffer."
  (interactive)
  (let ((ips (hackmode-bbrf-list-ips)))
    (with-current-buffer (get-buffer-create "*BBRF IPs*")
      (erase-buffer)
      (insert (mapconcat 'identity ips "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun hackmode-bbrf-show-urls ()
  "Display all URLs in a buffer."
  (interactive)
  (let ((urls (hackmode-bbrf-list-urls)))
    (with-current-buffer (get-buffer-create "*BBRF URLs*")
      (erase-buffer)
      (insert (mapconcat 'identity urls "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun hackmode-bbrf-show-services ()
  "Display all services in a buffer."
  (interactive)
  (let ((services (hackmode-bbrf-list-services)))
    (with-current-buffer (get-buffer-create "*BBRF Services*")
      (erase-buffer)
      (insert (mapconcat 'identity services "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun hackmode-bbrf-show-all ()
  "Display all assets in a formatted buffer."
  (interactive)
  (let ((all-assets (hackmode-bbrf-list-all-assets)))
    (with-current-buffer (get-buffer-create "*BBRF All Assets*")
      (erase-buffer)
      (dolist (asset-type all-assets)
        (let ((type (car asset-type))
              (assets (cdr asset-type)))
          (insert (format "=== %s ===\n" (upcase (symbol-name type))))
          (if assets
              (insert (mapconcat 'identity assets "\n"))
            (insert "No assets found"))
          (insert "\n\n")))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defgroup hackmode nil
  "HACKMODE customization group."
  :group 'tools
  :prefix "hackmode-")

(defcustom hackmode-bbrf-program-added-hook nil
  "Hook run when a program is added to HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-domain-added-hook nil
  "Hook run when a domain is added to HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-ip-added-hook nil
  "Hook run when an IP is added to HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-url-added-hook nil
  "Hook run when a URL is added to HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-service-added-hook nil
  "Hook run when a service is added to HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-program-removed-hook nil
  "Hook run when a program is removed from HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-domain-removed-hook nil
  "Hook run when a domain is removed from HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-ip-removed-hook nil
  "Hook run when an IP is removed from HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-url-removed-hook nil
  "Hook run when a URL is removed from HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-service-removed-hook nil
  "Hook run when a service is removed from HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-program-updated-hook nil
  "Hook run when a program is updated in HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-domain-updated-hook nil
  "Hook run when a domain is updated in HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-ip-updated-hook nil
  "Hook run when an IP is updated in HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-url-updated-hook nil
  "Hook run when a URL is updated in HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)

(defcustom hackmode-bbrf-service-updated-hook nil
  "Hook run when a service is updated in HACKMODE-BBRF."
  :type 'hook
  :group 'hackmode)


(defun hackmode-bbrf-create-program (name)
  "Create a new BBRF program with NAME and optional OPTIONS."
  (interactive
   (let ((name (read-string "Enter Program name: " hackmode-operation)))))
  (hackmode-bbrf-execute-command "new" name nil)
  (run-hooks 'hackmode-bbrf-program-added-hook)
  (message "Created new program: %s" name))



(defun hackmode-bbrf-set-program (name)
  "Set the current BBRF program to NAME."
  (interactive
   (list (hackmode-bbrf-read-program "Select a program: ")))
  (hackmode-bbrf-execute-command "use" name)
  (message "Set current program to: %s" name))



(defun hackmode-bbrf-add-inscope (domains)
  "Add DOMAINS to inscope for the current program."
  (interactive "sDomains to add to inscope (space-separated): ")
  (hackmode-bbrf-execute-command "inscope" "add" (split-string domains))
  (message "Added domains to inscope: %s" domains))

(defun hackmode-bbrf-add-outscope (domains)
  "Add DOMAINS to outscope for the current program."
  (interactive "sDomains to add to outscope (space-separated): ")
  (hackmode-bbrf-execute-command "outscope" "add" (split-string domains))
  (message "Added domains to outscope: %s" domains))


(defun hackmode-bbrf-add-domains (domains)
  "Add DOMAINS to the current program."
  (interactive "sDomains to add (space-separated): ")
  (hackmode-bbrf-execute-command "domain" "add" domains)
  (run-hooks 'hackmode-bbrf-domain-added-hook)

  (message "Added domains %s" domains))



(defun hackmode-bbrf-add-ips (ips)
  "Add IPS to the current program."
  (interactive "sIPS to add (space-separated): ")
  (hackmode-bbrf-execute-command "ip" "add" ips)
  (run-hooks 'hackmode-bbrf-ip-added-hook)
  (message "Added ips %s" ips))


(defun hackmode-bbrf-add-urls (urls)
  "Add URLS to the current program."
  (interactive "sURLS to add (space-separated): ")
  (hackmode-bbrf-execute-command "url" "add" urls)
  (run-hooks 'hackmode-bbrf-url-added-hook)
  (message "Added urls %s" urls))

(defun hackmode-bbrf-add-services (srvs)
  "add SRVS to the current program."
  (interactive "sServices to add (space-separated): ")
  (run-hooks 'hackmode-bbrf-service-added-hook)
  (hackmode-bbrf-execute-command "service" "add" srvs))

(defun hackmode-bbrf-remove-services (srvs)
  "Remove SRVS from the current program."
  (interactive "sServices to add (space-separated): ")
  (run-hooks 'hackmode-bbrf-service-removed-hook)
  (hackmode-bbrf-execute-command "service" "remove" srvs))

(defun hackmode-bbrf-remove-inscope (domains)
  "Remove DOMAINS to inscope for the current program."
  (interactive "sDomains to remove to inscope (space-separated): ")
  (hackmode-bbrf-execute-command "inscope" "remove" (split-string domains))
  (message "Removed domains to inscope: %s" domains))

(defun hackmode-bbrf-remove-outscope (domains)
  "Remove DOMAINS to outscope for the current program."
  (interactive "sDomains to remove to outscope (space-separated): ")
  (hackmode-bbrf-execute-command "outscope" "remove" (split-string domains))
  (message "Removed domains to outscope: %s" domains))


(defun hackmode-bbrf-remove-domains (domains)
  "Remove DOMAINS to the current program."
  (interactive "sDomains to remove (space-separated): ")
  (hackmode-bbrf-execute-command "domain" "remove" domains)
  (run-hooks 'hackmode-bbrf-domain-removed-hook)
  (message "Removed domains %s" domains))

(defun hackmode-bbrf-remove-ips (ips)
  "Remove IPS to the current program."
  (interactive "sIPS to remove (space-separated): ")
  (hackmode-bbrf-execute-command "ip" "remove" ips)
  (run-hooks 'hackmode-bbrf-ip-removed-hook)
  (message "Removed ips %s" ips))

(defun hackmode-bbrf-remove-urls (urls)
  "Remove URLS to the current program."
  (interactive "sURLS to remove (space-separated): ")
  (hackmode-bbrf-execute-command "url" "remove" urls)
  (run-hooks 'hackmode-bbrf-url-removed-hook)
  (message "Removed urls %s" urls))

;; TODO add-bulk domains command


(defun hackmode-bbrf-domains-from-region (start end)
  "Add domains from the selected region to BBRF."
  (interactive "r")
  (let ((domains (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-domains domains)))

(defun hackmode-bbrf-inscope-from-region (start end)
  "Add inscope from the selected region to BBRF."
  (interactive "r")
  (let ((scopes (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-inscope scopes)))

(defun hackmode-bbrf-outscope-from-region (start end)
  "Add inscope from the selected region to BBRF."
  (interactive "r")
  (let ((scopes (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-inscope scopes)))

(defun hackmode-bbrf-ips-from-region (start end)
  "Add domains from the selected region to BBRF."
  (interactive "r")
  (let ((ips (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-ips ips)))


(defun hackmode-bbrf-urls-from-region (start end)
  "Add domains from the selected region to BBRF."
  (interactive "r")
  (let ((urls (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-urls urls)))

(defun hackmode-bbrf-start-listener ()
  "Run the bbrf listener. launches shell scripts defined in ~/.bbrf/hooks."
  (interactive)
  (async-shell-command "while true; do bbrf listen; done" "*bbrf-listen*" "*bbrf-errors*"))

(defun hackmode-bbrf-add-tag (key val)
  "Add KEY and VAL to HACKMODE-BBRF-TAGS. when interacting with bbrf these will be used."
  (interactive (list
                (read-string "Enter key name: ")
                (read-string "Enter value: ")))
  (push (cons key val) hackmode-bbrf-tags))

(defun hackmode-bbrf-clear-tags ()
  "Remove all tags from HACKMODE-BBRF-TAGS."
  (interactive)
  (setf hackmode-bbrf-tags nil))

(defun hackmode-bbrf-delete-tag (name)
  "Delete tag by NAME."
  (interactive (list
                (completing-read "Key: " (mapcar #'car hackmode-bbrf-tags))))
  (setf hackmode-bbrf-tags (assoc-delete-all name hackmode-bbrf-tags)))

(defun hackmode-bbrf--display-tags ()
  "Formats a display string for the hackmode-bbrf-menu"
  (string-join (cl-loop for (key . val) in hackmode-bbrf-tags
                        collect (propertize  (format "%s:%s" key val) 'face 'transient-argument)) " "))

;;;###autoload
(transient-define-prefix hackmode-bbrf-menu ()
  "BBRF menu"
  [["Add To BBRF"
    ("u" "Add Urls" hackmode-bbrf-add-urls :transient t)
    ("i" "Add IPS" hackmode-bbrf-add-ips :transient t)
    ("d" "Add domains" hackmode-bbrf-add-domains :transient t)]

   ["Tags"
    (:info #'hackmode-bbrf--display-tags)
    ("tt" "add a tag" hackmode-bbrf-add-tag :transient t)
    ("td" "delete a tag" hackmode-bbrf-delete-tag :transient t)
    ("-td" "delete all tags" hackmode-bbrf-clear-tags :transient t)]

   ["Programs"
    (" " :info (lambda ()
                 (propertize (format  "Current: %s" (hackmode-bbrf-execute-command "program" "active")) 'face 'success)))
    ("c" "Create program" hackmode-bbrf-create-program :transient t)
    ("s" "Set program" hackmode-bbrf-set-program :transient t)
    ("I" "Add to inscope" hackmode-bbrf-add-inscope :transient t)
    ("o" "Add to outscope" hackmode-bbrf-add-outscope :transient t)
    ("l" "Start listener" hackmode-bbrf-start-listener :transient t)]])

;;;###autoload
(transient-define-prefix hackmode-menu ()
  "Main hackmode menu"
  [:description (lambda ()
                  (concat
                   "[" "Operation: "
                   (propertize hackmode-operation 'face 'transient-heading)
                   "]\n"
                   "["
                   "Target: "
                   (if hackmode-target
                       (propertize hackmode-target 'face 'transient-heading)
                     (propertize "NO TARGET SELECTED" 'face 'warning))
                   "]\n"))

                ["Assets"
                 ("b" "BBRF" hackmode-bbrf-menu)]

                ["Operations"
                 ("c" "Create operation." hackmode-create-operation :transient t)
                 ("so" "Select operation." hackmode-switch-op :transient t)
                 ("g" "Goto current operation." hackmode-goto-operation :transient nil)]

                ["Targets"
                 ("st" "Select Target" hackmode-select-target :transient t)]])




(provide 'hackmode)
;;; hackmode.el ends here
;;;
;;;
