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

(defvar-local hackmode-lib-hosts ()
  "Lists of known hosts. hosts are loaded from the dir names of the 'hackmode-lib-dir'.")


(defvar hackmode-dir "~/hackmode")
(defvar hackmode-default-operation "default"
  "The default operation to use.")

(defvar hackmode-operation-hook nil
  "Hook for when operation is changed")

(defcustom hackmode-tools-dir (f-join hackmode-dir "hackmode-tools/")
  "The Default path where tools to be uploaded will be pulled from.")


(defvar hackmode-operation hackmode-default-operation
  "Current operation name. Do not set this, instead use 'hackmode-lib-default-operation' or 'hackmode-lib-switch-op'.")

(defcustom hackmode-interface "tun0"
  "Network interface to use by default")

(defcustom hackmode-templates (f-expand "~/.config/hackmode/templates")
  "Path to templates directory.")
;; Alist of files that should be used.
;; NOTE i do like the idea of multiple checklists, maybe it could be org headings
;; or org files for a type of target too
;; TODO Allow selecting from the loot file
;; TODO also allow bulk import to the loot file
;; Expand this It is an alist of (<name> . <filepath>)
;; The user would be able to interactivly select a target
;; the command that selects the target should be advisable to let me use any function to select the target
(defcustom hackmode-checklists nil "Alist of files . name to be used for checklists.")

(defcustom hackmode-data-dir (f-expand "~/.local/share/hackmode/") "The directory to be used to hold current hackmode state, you should leave this default!")

(defun hackmode-read-target ()
  "read a target"
  (read-string "Enter a target: "))
(defvar hackmode-target-select-fn #'hackmode-read-target
  "The Function that will prompt to select a target. it must return a single string.")


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
    (hackmode-create-todo-entry destination-file)
    (message "Checklist for %s copied to %s" target destination-file)))

(defun hackmode-create-todo-entry (checklist-file)
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

;; Source: https://emacs.stackexchange.com/a/35033
(defun hackmode-popup (prompt default-index content)
  "Pop up menu
Takes args: prompt, default-index, content).
Where the content is any number of (string, function) pairs,
each representing a menu item."
  (cond
   ;; Ivy (optional)
   ((fboundp 'ivy-read)
    (ivy-read
     prompt content
     :preselect
     (cond
      ((= default-index -1)
       nil)
      (t
       default-index))
     :require-match t
     :action
     (lambda (x)
       (pcase-let ((`(,_text . ,action) x))
         (funcall action)))
     :caller #'custom-popup))

   ;; Fallback to completing read.
   (t
    (let ((choice
           (funcall completing-read-function
                    prompt
                    content
                    nil
                    t
                    nil
                    nil
                    (nth default-index content))))
      (pcase-let ((`(,_text . ,action) (assoc choice content)))
        (funcall action))))))



(defun get-interface-ip (interface)
  "Get the IP address of a network interface."
  (let ((output (shell-command-to-string (concat "ip addr show dev " interface " | grep 'inet '"))))
    (when (string-match "\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" output)
      (match-string 1 output))))

(defun get-iterfaces ()
  "Get a list of network interface names."
  (let ((output (shell-command-to-string "ip addr show | awk '/^[0-9]+:/ {gsub(/:/,\"\"); print $2}'")))
    (split-string output "\n" t)))



;;from org mode
(defun hackmode-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

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
    (f-touch (f-join path "findings/" "subdomains.txt"))
    (f-touch (f-join path "findings/" "urls.txt"))
    (f-touch (f-join path "findings/" "apex.txt"))
    (shell-command-to-string (format "git init %s" path))
    (shell-command-to-string (format "git add %s" (f-join path "findings/")))
    (shell-command-to-string "git commit -m \"Added Files\"")))



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


(defun hackmode-set-env (op-name)
  "Set env vars for sub shells and scripts."
  (setenv "PATH" (concat (getenv "PATH") ":" (f-full (f-expand hackmode-tools-dir))))
  (setenv "HACKMODE_OP" op-name)
  (setenv "HACKMODE_PATH" (hackmode-get-operation-path op-name)))

(defun hackmode-set-metadata (op-name)
  "create all the needed metadata for hackmode."
  (hackmode-create-op-config op-name)
  (f-write-text (hackmode-get-operation-path op-name) 'utf-8 (f-join hackmode-data-dir "op-path"))
  (f-write-text op-name 'utf-8 (f-join hackmode-data-dir "current-op"))
  (hackmode-set-env op-name)
  (hackmode-create-envrc op-name))

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

;; Host relation functions

(defun hackmode-get-host-path (operation host)
  "Get the full path to the HOST for the OPERATION."
  (f-full (f-join (hackmode-get-operation-path operation) host)))

(defun hackmode-init-host (operation host)
  "Create the HOST directory for OPERATION"
  (unless (f-exists-p (hackmode-get-host-path operation host))
    (f-mkdir-full-path (hackmode-get-host-path operation host))))






(defvar hackmode-new-host-hook nil
  "hook to run when adding a new host")
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




(defun hackmode-new-operation ()
  "Create a new operation to group tasks"
  (interactive)
  (let ((name (read-string "Enter Operation Name: ")))
    (if (file-directory-p (hackmode-lib-get-operation-path name))
        (message (format "operation %s already exists" name))
      (progn
        (make-directory (hackmode-lib-get-operation-path name))
        (message (format "operation %s created" name))))))


(defun hackmode-goto-operation ()
  "Go to the operation directory."
  (interactive)
  (find-file (hackmode-get-operation-path hackmode-operation)))


;; QOL Stuff
(defvar hackmode-wordlist-dir "~/wordlists/")
(defun hackmode-add-host ()
  "Add a Host to /etc/hosts"
  (interactive)
  (let ((hostname (read-string "Enter Host name: "))
        (ip (read-string "Enter IP: ")))
    (append-to-file (format "%s\t%s\n" ip hostname) nil "/sudo::/etc/hosts")))

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
    (f-symlink (f-expand hackmode-tools-dir) (f-join op-path "tools/"))
    (hackmode-create-envrc name)
    (setq hackmode-operation name)
    (hackmode-goto-operation)
    (run-hooks 'hackmode-operation-hook)))



(defun hackmode-kill-wordlist ()
  "Copy the path of a wordlist to the kill ring"
  (interactive)
  (kill-new (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))


(defun hackmode-insert-wordlist ()
  "Copy the path of a wordlist to the kill ring"
  (interactive)
  (insert (f-expand (read-file-name "Select Wordlist: " (f-expand hackmode-wordlist-dir)))))



;; (defun hackmode-create-checklist (name description &rest tasks)
;;   (list :name nam :description description :tasks (apply #'list tasks)))



(defun hackmode-kill-upload ()
  "Copy a wget or curl command to your kill ring.
It also return the command in string form."
  (interactive)
  (let* ((filename (f-relative (read-file-name "Tool to download: " hackmode-tools-dir "linpeas.sh") hackmode-tools-dir))
         (i-name (completing-read "interface: " (get-iterfaces)))
         (ip (get-interface-ip i-name))
         (port (read-string "port: " "8000"))
         (download-cmd (read-string "Cmd to use to download: " "wget"))
         (cmd (format "%s http://%s:%s/%s" download-cmd ip port filename)))
    (message cmd)
    (kill-new cmd)
    cmd))

(defun hackmode-http-server (root port)
  "Http server using python."
  (let ((buffer (get-buffer-create "(http*")))
    (with-current-buffer (start-process buffer (format "http.server:%s" port) (executable-find "python") "-m" "http.server" port "-d" root)
      (add-hook window-buffer-change-functions #'(lambda () (hackmode-send-notification "HTTP server" "New request!"))))))



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

(defcustom hackmode-pwncat-port "9001"
  "Default Port to use for pwncat-cs.")


;; TODO move this away from vterm
(defun hackmode-pwncat ()
  "Start a pwncat-cs shell using vterm."
  (interactive)
  (require 'vterm)
  (let ((buffer-name "*pwncat*")
        (port (read-string "Enter Port number: " hackmode-pwncat-port)))

    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (vterm-mode)

        (vterm-send-string (format  "pwncat-cs -lp %s && exit" port))
        (vterm-send-return)))
    (switch-to-buffer buffer-name)))





;; Add hooks area
(add-hook 'hackmode-operation-hook #'hackmode-goto-operation)

(provide 'hackmode)
;;; hackmode.el ends here
;;;
;;;
