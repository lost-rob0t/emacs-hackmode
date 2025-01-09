;;; hackmode.el --- Pentesters Lil lisp redpill -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  <nsaspy@fedora.email>
;; Maintainer:  <nsaspy@fedora.email>
;; Created: May 21, 2023
;; Modified: Dec 1, 2023
;; Version: 0.0.8
;; Keywords: security hacking
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
(require 'hackmode-loot)
(require 'searchsploit)

;;; Common var Setups
(defvar hackmode-dir "~/hackmode")
(defcustom hackmode-checklists nil "Alist of files . name to be used for checklists.")
(defcustom hackmode-data-dir (f-expand "~/.local/share/hackmode/") "The directory to be used to hold current hackmode state, you should leave this default!")

(defvar hackmode-path-file (f-join hackmode-data-dir "op-path") "File with contents pointing to current hackmode path")
(defvar hackmode-operation-file (f-join hackmode-data-dir "current-op") "File with contents the name of current hackmode op")
(defvar hackmode-default-operation "default"
  "The default operation to use.")

(defvar hackmode-operation-hook nil
  "Hook for when operation is changed")

(defcustom hackmode-tools-dir (f-join hackmode-dir ".hackmode-tools/")
  "The Default path where tools to be uploaded will be pulled from."
  :group 'hackmode
  :type 'string)


(defvar hackmode-operation hackmode-default-operation
  "Current operation name. Do not set this, instead use 'hackmode-menu' or 'hackmode-switch-op'.")

(defcustom hackmode-interface "tun0"
  "Network interface to use by default")

;;;TODO Suggest skeltor templates maybe?
(defcustom hackmode-templates (f-expand "~/.config/hackmode/templates")
  "Path to templates directory.")

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
  (let* ((target (funcall hackmode-target-select-fn))
         (dir  (or default-directory (hackmode-get-operation-path  hackmode-operation)))
         (file-path (cdr (assoc (completing-read "Select a checklist: " hackmode-checklists nil t) hackmode-checklists)))
         (destination-dir (f-join dir "checklists"))
         (destination-file (f-join destination-dir (concat target "-checklist.org"))))
    (unless (f-exists? destination-dir)
      (f-mkdir destination-dir))
    (f-copy file-path destination-file)
    (hackmode-create-checklist-entry destination-file)
    (message "Checklist for %s copied to %s" target destination-file)))





;; Operations and managment functions
(defun hackmode-get-operation-path (operation)
  "Get the full path for a OPERATION."
  (f-full (f-expand (f-join hackmode-dir operation))))

(defun hackmode-create-op-config (operation)
  "Create the config dirs for hackmode operation and gloabl if not exists."
  (let* ((path (hackmode-get-operation-path operation))
         (default-directory path))
    (f-mkdir-full-path hackmode-data-dir)
    (f-mkdir-full-path (f-join path ".config/"))
    (f-mkdir-full-path (f-join path "findings/"))
    (f-touch (f-join path "findings/" ".keep"))))
;;  you can use either file


(defun hackmode-get-finds-path (op-name)
  "Get the path to the operation's .config/"
  (f-join (hackmode-get-operation-path op-name) "findings/"))

(defun hackmode-get-config-path (op-name)
  "Get the path to the operation's .config/"
  (f-join (hackmode-get-operation-path op-name) ".config/"))



(defun hackmode-read-config-file (op-name config-filename)
  "Read a config for a operation"
  (with-temp-buffer (insert-file-contents-literally
                     (f-join (hackmode-get-config-path op-name) filename))
                    (buffer-string)))

(defun hackmode-write-operation-config (op-name config-filename strings)
  "Write to the OPERATION's config."
  (f-write-text strings 'utf-8 (f-join  (hackmode-get-config-path op-name) config-filename)))



(defun hackmode-create-envrc (op-name)
  "Creates the .envrc for direnv."
  (let* ((op-path (hackmode-get-operation-path op-name))
         (envrc-file (f-expand (f-join op-path ".envrc"))))
    (message "Writing envrc to: %s" envrc-file)
    (f-write-text (format  "export HACKMODE_PATH=%s; export HACKMODE_OP=%s " op-path op-name) 'utf-8 envrc-file)))

(defun hackmode-create-targets-file (op-name)
  "Create The targets file for OP-NAME."
  (let ((targets (f-join (hackmode-get-config-path op-name) "targets.txt")))
    (f-touch targets)))

(defun hackmode-set-env (op-name)
  "Set env vars for sub shells and scripts."
  (setenv "PATH" (concat (getenv "PATH") ":" (f-full (f-expand hackmode-tools-dir))))
  (setenv "HACKMODE_OP" op-name)
  (setenv "HACKMODE_PATH" (hackmode-get-operation-path op-name)))

(defun hackmode-set-metadata (op-name)
  "create all the needed metadata for hackmode op."
  (hackmode-create-op-config op-name)
  (f-write-text (hackmode-get-operation-path op-name) 'utf-8 (f-join hackmode-data-dir "op-path"))
  (f-write-text op-name 'utf-8 (f-join hackmode-data-dir "current-op"))
  (hackmode-set-env op-name))


(defun hackmode-operations ()
  "Return a list of operations."
  (mapcar #'f-base (f-directories (f-full hackmode-dir))))

(defun hackmode-switch-op ()
  "Switch operation."
  (interactive)
  (let ((op (completing-read "Select operation: " (hackmode-operations) nil nil)))
    (setq hackmode-operation op)
    (hackmode-set-metadata op)
    (run-hooks 'hackmode-operation-hook)))

;; TODO move this to hackmode.el
(defun hackmode-new-operation ()
  "Create a new operation to group tasks"
  (interactive)
  (let ((name (read-string "Enter Operation Name: ")))
    (if (file-directory-p (hackmode-get-operation-path name))
        (message (format "operation %s already exists" name))
      (progn
        (make-directory (hackmode-get-operation-path name))
        (message (format "operation %s created" name))))))



;; TODO write the script
(defcustom hackmode-hook nil
  "hook run when entering hackmode"
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode evil-local-mode))

(defcustom hackmode-dir "~/hackmode"
  "Directory holding operations and scan output"
  :type 'string
  :options '("~/scans" "~/.local/share/hackmode"))
(defcustom hackmode-default-operation "default"
  "The Default name to use for when loading into hackmode.
You can also M-X hackmode-switch-op to switch"
  :type 'string)




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


(defvar hackmode-wordlist-dir "~/wordlists/")
(defun hackmode-add-host (hostname address)
  "Add a Host to /etc/hosts"
  (interactive "sEnter hostname: \nEnter IP: ")
  (append-to-file (format "%s\t%s\n" address hostname) nil "/sudo::/etc/hosts"))

(defun hackmode-copy (name dest)
  "Copy a template NAME to DEST"
  (f-copy (f-join hackmode-templates name) dest))


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

(defun hackmode-select-target ()
  "Select a target from a text-file, defaults to the $HACKMODE_OP/.config/targets.txt file."
  (interactive)
  (let* ((targets-file (read-file-name "Select Targets File: " (hackmode-get-config-path hackmode-operation) "targets.txt")))
    (completing-read "Select Target: " (remove-if #'(lambda (x)
                                                      (= (length x) 0))
                                                  (split-string (with-temp-buffer
                                                                  (insert-file-contents-literally targets-file)
                                                                  (buffer-string)) "\n")))))
(defun hackmode-set-target ()
  "Select a target and set it globally."
  (interactive)
  (setq-local hackmode-target (hackmode-select-target)))

(defun hackmode-fill-target ()
  "Set the buffers target if it hasent ben set otherwise return the target."
  (or hackmode-target (hackmode-set-target)))

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

(defun hackmode-http-server (root port)
  "Http server using python with dir listing on ROOT and listening on PORT."
  (let ((buffer (get-buffer-create "*http*")))
    (with-current-buffer (start-process buffer (format "http.server:%s" port) (executable-find "python") "-m" "http.server" port "-d" root)
      (add-hook window-buffer-change-functions #'(lambda () (alert "New request from http server" :title "*hackmode-http-serv*"))))))



(defun hackmode-stop-http ()
  "Stop the http server."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "http" (buffer-name buffer))
      (kill-buffer buffer))))



(defun hackmode-serve-tools ()
  "Serve the hackmode tools dir."
  (interactive)
  (let ((port (read-string "port: " "8000")))

    (hackmode-http-server hackmode-tools-dir port)))


;;; BBRF Asset Tracking.

(defvar hackmode-bbrf-new-docmain-hook nil "Hook that is ran when a new domain is added to a program")

(defun hackmode-bbrf-create-program (name)
  "Create a new BBRF program with NAME."
  (interactive (list (read-string "Enter Program name: " hackmode-operation)))
  (shell-command (format "bbrf new %s" name))
  (message "Created new program: %s" name))


(defun hackmode-bbrf-set-program (name)
  "Set the current BBRF program to NAME."
  (interactive
   (list (completing-read "Select program: "
                          (split-string (shell-command-to-string "bbrf programs") "\n" t))))
  (shell-command (format "bbrf use %s" name))
  (message "Set current program to: %s" name))

(defun hackmode-bbrf-add-inscope (domains)
  "Add DOMAINS to inscope for the current program."
  (interactive "sDomains to add to inscope (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf inscope add %s" domains))
    (message "Added %s to inscope for program %s" domains program)))

(defun hackmode-bbrf-add-outscope (domains)
  "Add DOMAINS to outscope for the current program."
  (interactive "sDomains to add to outscope (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf outscope add %s" domains))
    (message "Added %s to outscope for program %s" domains program)))

(defun hackmode-bbrf-add-domains (domains)
  "Add DOMAINS to the current program."
  (interactive "sDomains to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf domain add %s" domains))
    (message "Added domains %s to program %s" domains program)))


(defun hackmode-bbrf-add-ips (ips)
  "Add IPS to the current program."
  (interactive "sIPS to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf ip add %s" ips))
    (message "Added ips %s to program %s" ips program)))

(defun hackmode-bbrf-add-urls (urls)
  "Add URLS to the current program."
  (interactive "sURLS to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf url add %s" urls))
    (message "Added urls %s to program %s" ips program)))


(defun hackmode-bbrf-remove-services (srvs)
  "Remove SRVS from the current program."
  (interactive "sServices to add (space-separated): ")
  (let ((program hackmode-operation))
    (shell-command (format "bbrf service remove %s" srvs))
    (message "Added services %s to program %s" srvs program)))

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



(defun hackmode-bbrf ()
  "Open the BBRF menu."
  (interactive)
  (hackmode-bbrf-menu))

;;;Hackmode Menu
(transient-define-prefix hackmode-menu ()
  [["Operations"
    ("c" "Create operation." hackmode-init)
    ("s" "Select operation." hackmode-switch-op)
    ("g" "Goto current operation." hackmode-goto-operation)]
   ["Asset mgnt"
    ("b" "BBRF Menu." hackmode-bbrf-menu)]])

;; Add hooks area
(add-hook 'hackmode-operation-hook #'hackmode-goto-operation)

(provide 'hackmode)
;;; hackmode.el ends here
;;;
;;;
