;;; init-org.el --- Org Mode Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Paths
(require 'init-core)

(defvar shl-org-dir (concat shl--data-dir "org/")
  "Directory containing my org files.")

(defvar shl-notes-file (file-name-concat shl-org-dir "notes.org")
  "File containing my notes.")

(defvar shl-tasks-file (file-name-concat shl-org-dir "tasks.org")
  "File containing my tasks.")

(defvar shl-journal-file (file-name-concat shl-org-dir "journal.org")
  "File containing my journal.")

(defvar shl-inbox-file (file-name-concat shl-org-dir "inbox.org")
  "File containing my inbox.")

;;; Functions
(defun shl-notes-file-open ()
  (interactive)
  (find-file shl-notes-file)
  "Open notes file.")

(defun shl-tasks-file-open ()
  (interactive)
  (find-file shl-tasks-file)
  "Open tasks file.")

(defun shl-journal-file-open ()
  (interactive)
  (find-file shl-journal-file)
  "Open journal file.")

(defun shl-inbox-file-open ()
  (interactive)
  (find-file shl-inbox-file)
  "Open inbox file.")

;;; Configuration
(use-package org
  :ensure t
  :bind (:map global-map
         ("C-c l" . org-store-link)
         ("C-c o" . org-open-at-point-global)
         ("C-c c" . org-capture))
  :init
  (setopt org-directory shl-org-dir
          org-imenu-depth 7)
  :config
  (setopt org-adapt-indentation t)

  (setopt org-todo-keywords
          '((sequence "STARTED(s)" "TODO(t)" "WAITING(w@/!)"
                      "SOMEDAY(S!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")))

  (setq org-capture-templates
        '(("j" "Journal" entry (file+datetree shl-journal-file)
           "* %?\n%U\n%i\n\n")
          ("t" "Task" entry (file shl-tasks-file)
           "* TODO %?\n%U\n")
          ("n" "Task" entry (file shl-notes-file)
           "* %?\n%U\n%i\n\n"))))


;;; Babel
(use-package org
  :ensure nil
  :config
  (setopt org-confirm-babel-evaluate nil
          org-src-window-setup 'current-window
          org-edit-src-persistent-message nil
          org-src-fontify-natively t
          org-src-preserve-indentation t
          org-src-tab-acts-natively t
          org-edit-src-content-indentation 0))

;;; UI
(use-package org-modern
  :ensure t
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))

;; Org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory shl-org-dir)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
  

(provide 'init-org)
;;; init-org.el ends here
