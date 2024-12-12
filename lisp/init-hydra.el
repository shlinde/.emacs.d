;;; init-hydra.el --- Setup Hydras -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(require 'init-org)

(use-package hydra
  :ensure t
  :defer t
  :init
  ;; Hydra for org agenda (graciously taken from Spaciemacs)
  (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                   :post (setq which-key-inhibit nil)
                                   :hint none)
    "
Org agenda (_q_uit)

^File^       ^Capture/Agenda^
^-----^----- ^-------------^
_i_ inbox   _c_ capture    
_t_ tasks   _a_ agenda     
_n_ notes   
_j_ journal ^^
^^           ^^           

"
    ;; Entry
    ("i" shl-inbox-file-open :exit t)
    ("t" shl-tasks-file-open :exit t)
    ("j" shl-journal-file-open :exit t)
    ("n" shl-notes-file-open :exit t)

    ("a" org-agenda :exit t)
    ("c" org-capture :exit t)

    ("q" nil :exit t))

  (global-set-key (kbd "C-c n") #'hydra-org-agenda/body))



(provide 'init-hydra)
;;; init-hydra.el ends here
