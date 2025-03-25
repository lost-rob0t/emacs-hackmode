;;; hackmode.el --- Pentesters Lil lisp redpill -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@fedora.email>
;; Maintainer:  <nsaspy@fedora.email>
;; Created: May 21, 2023
;; Modified: Feb 5, 2025
;; Version: 0.0.8
;; Keywords: security pentesting reporting automation org
;; Homepage: https://github.com/unseen/hackmode
;; Package-Requires: ((emacs "28.2") (emacs-async "1.97") (f "v0.20.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Pentesters Lil Lisp redpill.
;;
;;; Code:
(require 'async)
(require 'f)


;;; Common var Setups
(defcustom hackmode-default-operation "default"
  "The default operation to use."
  :group 'hackmode
  :type 'string)

(defvar hackmode-operation hackmode-default-operation
  "Current operation name. Do not set this, instead use 'hackmode-menu' or 'hackmode-switch-op'.")

(defcustom hackmode-dir "~/hackmode"
  "The base directory to store operation workspace in."
  :group 'hackmode :type 'string)

(defcustom hackmode-checklists nil "Alist of files . name to be used for checklists."
  :group 'hackmode)

(defcustom hackmode-data-dir (f-expand "~/.local/share/hackmode/")
  "The directory to be used to hold current state stating what and where the current operation is."
  :group 'hackmode
  :type 'string)

(defcustom hackmode-capture-templates nil "Org Capture templates specific to hackmode,."
  :group 'hackmode
  :type 'list)

(defcustom hackmode-path-file (f-join hackmode-data-dir "op-path") "File with contents pointing to current hackmode path."
  :group 'hackmode
  :type 'string)

(defcustom hackmode-operation-file (f-join hackmode-data-dir "current-op") "File with contents the name of current hackmode op."
  :group 'hackmode
  :type 'string)

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


;;;###autoload
(defun hackmode-use-checklist ()
  "Read a target from the user and copy the checklist file."
  (interactive)
  (let* ((target (call-interactively (hackmode-fill-target)))
         (dir  (or default-directory (hackmode-get-operation-path  hackmode-operation)))
         (file-path (cdr (assoc (completing-read "Select a checklist: " hackmode-checklists nil t) hackmode-checklists)))
         (destination-dir (f-join dir "checklists"))
         (destination-file (f-join destination-dir (concat target "-checklist.org"))))
    (unless (f-exists? destination-dir)
      (f-mkdir destination-dir))
    (f-copy file-path destination-file)
    (hackmode-create-checklist-entry destination-file)
    (message "Checklist for %s copied to %s" target destination-file)))

;;; Hackmode capture from hacmode loot
;;;###autoload
(defun hackmode-capture ()
  "Capture a thought/data before it is lost to entropy."
  (interactive)
  (let* ((default-directory (hackmode-get-operation-path hackmode-operation))
         (org-directory default-directory)
         ;; NOTE you need to ensure this file is up to date.
         (org-capture-templates hackmode-capture-templates))
    (call-interactively #'org-capture)))


;; Operations and managment functions
;;;###autoload
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


;;;###autoload
(defun hackmode-get-finds-path (operation)
  "Get the path to the OPERATION .config/ ."
  (f-join (hackmode-get-operation-path operation) "findings/"))

;;;###autoload
(defun hackmode-get-config-path (operation)
  "Get the path to the OPERATION .config ."
  (f-join (hackmode-get-operation-path operation) ".config/"))



;;;###autoload
(defun hackmode-read-config-file (operation config-filename)
  "Read OPERATION config file."
  (with-temp-buffer (insert-file-contents-literally
                     (f-join (hackmode-get-config-path operation) filename))
                    (buffer-string)))

;;;###autoload
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


;;;###autoload
(defun hackmode-operations ()
  "Return a list of operations."
  (mapcar #'f-base (f-directories (f-full hackmode-dir))))

;;;###autoload
(defun hackmode-switch-op ()
  "Switch operation."
  (interactive)
  (let ((op (completing-read "Select operation: " (hackmode-operations) nil nil)))
    (setq hackmode-operation op)
    (hackmode-set-metadata op)
    (run-hooks 'hackmode-operation-hook)))

;;;###autoload
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



;;;###autoload
(defun hackmode-add-host (hostname address)
  "Add a Host to /etc/hosts"
  (interactive "sEnter hostname: \nEnter IP: ")
  (append-to-file (format "%s\t%s\n" address hostname) nil "/sudo::/etc/hosts"))

(defun hackmode-copy (name dest)
  "Copy a template NAME to DEST"
  (f-copy (f-join hackmode-templates name) dest))


;;;###autoload
(defun hackmode-init-subtarget ()
  "Create a sub target within hackmode."
  (interactive)
  (let ((target (read-string "Enter target: " nil t nil))
        (setq hackmode-operation (format "%s/%s" hackmode-operation target))
        (when (not (f-dir? (hackmode-get-operation-path hackmode-operation)))
          (f-mkdir (f-expand (hackmode-get-operation-path hackmode-operation)))))))



(defun hackmode-init-metadata (operation-name)
  "Write the hackmode metadata to ~/.local/share/hackmode/."
  (let ((path (hackmode-get-operation-path operation-name)))
    (f-write-text path 'utf-8)))

;;;###autoload
(defun hackmode-init ()
  "Interactivly create a operation."
  (interactive)
  (let* ((template (completing-read "Select Template: " (f-directories hackmode-templates)))
         (name (read-string "Enter Operaton Name: "))
         (op-path (hackmode-get-operation-path name)))
    (f-copy template op-path)
    (f-symlink op-path default-directory)
    ;; TODO Move this stuff to a function
    (setf hackmode-operation name)
    (hackmode-create-envrc name)
    (hackmode-create-targets-file name)
    (hackmode-set-metadata)
    (run-hooks 'hackmode-operation-hook)
    (hackmode-goto-operation)))

(defvar-local hackmode-target nil "The current target. This var is used for yasnippets.")

;;;###autoload
(defun hackmode-select-target ()
  "Select a target from a text-file, defaults to the $HACKMODE_OP/.config/targets.txt file."
  (interactive)
  (let* ((targets-file (read-file-name "Select Targets File: " (hackmode-get-config-path hackmode-operation) "targets.txt")))
    (completing-read "Select Target: " (remove-if #'(lambda (x)
                                                      (= (length x) 0))
                                                  (split-string (with-temp-buffer
                                                                  (insert-file-contents-literally targets-file)
                                                                  (buffer-string)) "\n")))))
;;;###autoload
(defun hackmode-set-target ()
  "Select a target and set it globally."
  (interactive)
  (setq-local hackmode-target (hackmode-select-target)))

;;;###autoload
(defun hackmode-fill-target ()
  "Set the buffers target if it hasent ben set otherwise return the target."
  (or hackmode-target (hackmode-set-target)))

;;;###autoload
(defun hackmode-kill-wordlist ()
  "Copy the path of a wordlist to the kill ring"
  (interactive)
  (kill-new (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))

;; TODO Move this to a yasnippet
;;;###autoload
(defun hackmode-insert-wordlist ()
  "Copy the path of a wordlist to the kill ring."
  (interactive)
  (insert (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))




;;;###autoload
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

;;;###autoload
(defun hackmode-bbrf-create-program (name)
  "Create a new BBRF program with NAME."
  (interactive (list (read-string "Enter Program name: " hackmode-operation)))
  (shell-command (format "bbrf new %s" name))
  (message "Created new program: %s" name))


;;;###autoload
(defun hackmode-bbrf-set-program (name)
  "Set the current BBRF program to NAME."
  (interactive
   (list (completing-read "Select program: "
                          (split-string (shell-command-to-string "bbrf programs") "\n" t))))
  (shell-command (format "bbrf use %s" name))
  (message "Set current program to: %s" name))

;;;###autoload
(defun hackmode-bbrf-add-inscope (domains)
  "Add DOMAINS to inscope for the current program."
  (interactive "sDomains to add to inscope (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf inscope add %s" domains))
    (message "Added %s to inscope for program %s" domains program)))

;;;###autoload
(defun hackmode-bbrf-add-outscope (domains)
  "Add DOMAINS to outscope for the current program."
  (interactive "sDomains to add to outscope (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf outscope add %s" domains))
    (message "Added %s to outscope for program %s" domains program)))

;;;###autoload
(defun hackmode-bbrf-add-domains (domains)
  "Add DOMAINS to the current program."
  (interactive "sDomains to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf domain add %s" domains))
    (message "Added domains %s to program %s" domains program)))


;;;###autoload
(defun hackmode-bbrf-add-ips (ips)
  "Add IPS to the current program."
  (interactive "sIPS to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf ip add %s" ips))
    (message "Added ips %s to program %s" ips program)))

;;;###autoload
(defun hackmode-bbrf-add-urls (urls)
  "Add URLS to the current program."
  (interactive "sURLS to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf url add %s" urls))
    (message "Added urls %s to program %s" ips program)))


;;;###autoload
(defun hackmode-bbrf-remove-services (srvs)
  "Remove SRVS from the current program."
  (interactive "sServices to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf service remove %s" srvs))
    (message "Added services %s to program %s" srvs program)))

;; TODO add-bulk domains command

;;;###autoload
(defun hackmode-bbrf-domains-from-region (start end)
  "Add domains from the selected region to BBRF."
  (interactive "r")
  (let ((domains (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-domains domains)))

;;;###autoload
(defun hackmode-bbrf-inscope-from-region (start end)
  "Add inscope from the selected region to BBRF."
  (interactive "r")
  (let ((scopes (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-inscope scopes)))

;;;###autoload
(defun hackmode-bbrf-ips-from-region (start end)
  "Add domains from the selected region to BBRF."
  (interactive "r")
  (let ((ips (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-ips ips)))

;;;###autoload
(defun hackmode-bbrf-urls-from-region (start end)
  "Add domains from the selected region to BBRF."
  (interactive "r")
  (let ((urls (split-string (buffer-substring-no-properties start end) "\n")))
    (mapcar #'hackmode-bbrf-add-urls urls)))


;;;###autoload
(defun hackmode-bbrf-start-listener ()
  "Run the bbrf listener. launches shell scripts defined in ~/.bbrf/hooks."
  (interactive)
  (async-shell-command "while true; do bbrf listen; done" "*bbrf-listen*" "*bbrf-errors*"))




(transient-define-prefix hackmode-bbrf-menu ()
  "BBRF menu"
  [["Add To BBRF"
    ("u" "Add Urls" hackmode-bbrf-add-urls)
    ("i" "Add IPS" hackmode-bbrf-add-ips)
    ("d" "Add domains" hackmode-bbrf-add-domains)]
   ["BBRF Managment"
    ("c" "Create program" hackmode-bbrf-create-program)
    ("s" "Set program" hackmode-bbrf-set-program)
    ("i" "Add to inscope" hackmode-bbrf-add-inscope)
    ("o" "Add to outscope" hackmode-bbrf-add-outscope)]])

;; TODO hackmode-bbrf or asset listing is NEEDED.

;;;Hackmode Menu
(transient-define-prefix hackmode-menu ()
  [["Operations"
    ("c" "Create operation." hackmode-init)
    ("s" "Select operation." hackmode-switch-op)
    ("g" "Goto current operation." hackmode-goto-operation)]])


;; Add hooks area
(add-hook 'hackmode-operation-hook #'hackmode-goto-operation)

(provide 'hackmode)
;;; hackmode.el ends here
;;;
;;;
