;;; init-org.el --- Org Mode Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)

;;; Org-mode (personal information manager)
(use-package org
  :ensure nil
  :hook (org-mode . variable-pitch-mode)
  :init
  (setq org-directory (expand-file-name "~/data/org/"))
  (setq org-imenu-depth 7)

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :bind
  (:map global-map
	("C-c l" . org-store-link)
	("C-c o" . org-open-at-point-global)
	:map org-mode-map
	("C-c M-l" . org-insert-last-stored-link)
	("C-c C-M-l" . org-toggle-link-display)
	("M-." . org-edit-special) ; alias for C-c ' (mnenomic is global M-. that goes to source)
	:map org-src-mode-map
	("M-," . org-edit-src-exit) ; see M-. above
	:map narrow-map
	("b" . org-narrow-to-block)
	("e" . org-narrow-to-element)
	("s" . org-narrow-to-subtree))
  :config
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
  (setq org-fold-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)
  (setq org-read-date-prefer-future 'time)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-priority-faces nil))


;;;; `org-indent-mode' and related
(use-package org
  :ensure nil
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-adapt-indentation nil) ; No, non, nein, όχι to literal indentation!
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-indent-indentation-per-level 2))

;;;; refile, todo
(use-package org
  :ensure nil
  :config
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "|" "CANCELED(c@)" "DONE(d!)")
	  (sequence "NOTE(n)")
	  (sequence "READ(r) | COMPLETED(C!)")
	  (sequence "APPOINTMENT(a)")))

  (defface shl/org-todo-alternative
    '((t :inherit (italic org-todo)))
    "Face for alternative TODO-type Org keywords.")

  (defface shl/org-done-alternative
    '((t :inherit (italic org-done)))
    "Face for alternative DONE-type Org keywords.")

  (setq org-todo-keyword-faces
        '(("MAYBE" . shl/org-todo-alternative)
          ("CANCELED" . shl/org-done-alternative)))

  (defface shl/org-tag-coaching
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#004476")
      (((class color) (min-colors 88) (background dark))
       :foreground "#c0d0ef")
      (t :foreground "cyan"))
    "Face for coaching Org tag.")

  (defface shl/org-tag-shlasks
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#603f00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#deba66")
      (t :foreground "yellow"))
    "Face for shlasks Org tag.")

  (setq org-tag-faces
        '(("coaching" . shl/org-tag-coaching)
          ("shlasks" . shl/org-tag-shlasks)))

  (setq org-use-fast-todo-selection 'expert)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t))

(use-package org
  :ensure nil
  :general
  (shl/leader
    "n c" '(org-capture :which-key "capture")
    "n a" '(org-agenda :which-key "agenda")
    "n j" '(shl/open-journal :which-key "open journal"))
    ;; "n s" . '(shl/search-jounal :which-key "search journal")
  :init
  (defun shl/open-journal ()
    "Open the journal.org file in ~/data/org/ directory."
    (interactive)
    (find-file "~/data/org/journal.org"))
  :config
  (setq org-capture-templates
	(quote (("j" "Journal" entry (file "~/data/org/journal.org")
		 "* %^{Title} :JOURNAL:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
		 :prepend t
		 :empty-lines 1)
		("a" "Appointment" entry (file "~/data/org/journal.org")
		 "* APPOINTMENT %^{Title} :APPOINTMENT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
		 :prepend t
		 :empty-lines 1)
		("t" "Todo" entry (file "~/data/org/journal.org")
		 "* TODO %^{Title} :TODO:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
		 :prepend t
		 :empty-lines 1))))

  (setq org-agenda-files '("~/data/org/journal.org")
	org-log-into-drawer t))

(use-package org-super-agenda
  :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:name "Due Soon" :deadline future :order 1)
          (:name "High Priority" :priority "A" :order 2)
          (:name "Appointments" :tag "APPOINTMENT")
          (:name "Reading" :todo "READ")
          (:discard (:todo "CANCELED")))))

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


(use-package org-modern
  :ensure t
  :hook (elpaca-after-init . global-org-modern-mode))

(defun shl/org-open-file-from-property ()
  "Find and open the file specified in the \"file\" property of the current Org heading.

The function searches for a property like:
:PROPERTIES:
:file: /path/to/your/file.txt
:END:

and opens the specified file."
  (interactive)
  ;; We need to be in org-mode to use this function
  (unless (derived-mode-p 'org-mode)
    (error "This command can only be run from an Org mode buffer"))

  (let* (;; org-entry-get will get a property from the current headline.
         ;; The "t" argument tells it to search inherited properties as well.
         (file-path (org-entry-get (point) "file-path" t)))

    ;; Check if the file-path was found and is not an empty string
    (if (and file-path (not (string-empty-p file-path)))
        ;; If a valid path is found, expand it (to handle ~/) and open it
        (find-file (expand-file-name file-path))
      ;; If no "file" property is found, inform the user.
      (message "No \":file:\" property found at the current heading."))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r") #'shl/org-open-file-from-property))


(provide 'init-org)
;;; init-org.el ends here
