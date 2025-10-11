;;; init-workspaces.el --- Workspace & Project Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-completion)

(use-package project
  :ensure nil
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :bind (:map project-prefix-map
	      ("g" . consult-ripgrep)
	      ("R" . project-remember-projects-under))
  :custom
  (project-list-file (concat shl--cache-dir "projects"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))
  (project-vc-extra-root-markers '(".dir-locals.el" ".project.el" "package.json" "requirements.txt" "autogen.sh"))
  :config
  ;; Use Ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep))

  (setq shl-project-dir "~/data/source/")

  ;; remove deleted projects from list
  (project-forget-zombie-projects)

  (defun shl--project-name ()
    "Return name of project without path."
    (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-")))))

(use-package tab-bar
  :ensure nil
  :demand t
  :hook (elpaca-after-init . (lambda () (tab-bar-mode 2)))
  :after (project)
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-show 0)
  (tab-bar-tab-hints t) ;; show numbers in tabs
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  ;; (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-tab-name-format-function #'shl--tab-bar-tab-name-format)
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-menu-bar
		    tab-bar-format-history
                    tab-bar-format-tabs
                    shl--tab-bar-suffix
                    tab-bar-format-add-tab))
  :config

  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defvar shl-tab-bar--circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨")
      (10 . "⑩")
      (11 . "⑪")
      (12 . "⑫")
      (13 . "⑬")
      (14 . "⑭")
      (15 . "⑮"))

    "Alist of integers to strings of circled unicode numbers.")
  (defun shl--tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 16))
                       (alist-get i shl-tab-bar--circle-numbers-alist) "")))
      (propertize
       (concat
        " "
        tab-num
        (propertize " " 'display '(space :width (4)))
        (alist-get 'name tab)
        (or (and tab-bar-close-button-show
                 (not (eq tab-bar-close-button-show
                          (if current-p 'non-selected 'selected)))
                 tab-bar-close-button)
            "")
        (propertize " " 'display '(space :width (4))))
       'face (funcall tab-bar-tab-face-function tab))))


  ;; See https://github.com/rougier/nano-modeline/issues/33
  (defun shl--tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " ")


  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun shl-tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Otherwise use completion to select the tab."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t)))))))

;;;; Tab Workspaces
(use-package tabspaces
  :ensure (tabspaces :type git :host github :repo "mclear-tools/tabspaces")
  ;; Add some functions to the project map
  :bind (:map project-prefix-map
         ("p" . tabspaces-open-or-create-project-and-workspace))
  :hook (emacs-startup . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Home")
  :config
  (defun shl--consult-tabspaces ()
    "Deactivate isolated buffers when not using tabspaces."
    (require 'consult)
    (cond (tabspaces-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources 'consult--source-workspace))
          (t
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
  (add-hook 'tabspaces-mode-hook #'shl--consult-tabspaces))

;;;;; Consult Isolated Workspace Buffers
;; Filter Buffers for Consult-Buffer
(defun shl-buff-filter (buffer)
  (let ((blst (cl-remove (buffer-name) (frame-parameter nil 'buffer-list))))
    (memq buffer blst)))

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                           :predicate #'tabspaces--local-buffer-p
                           :sort 'visibility
                           :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer."))

(provide 'init-workspaces)
;;; init-workspaces.el ends here
