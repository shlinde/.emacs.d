;;; gptel-magit-review.el --- Git diff code review via gptel + Magit -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'magit nil t)
(require 'gptel) ; ensures gptel-mode, gptel-send, gptel-make-preset

(defgroup gptel-magit-review nil
  "Lightweight code review workflow using gptel + Magit."
  :group 'tools
  :prefix "gptel-magit-review-")

(defcustom gptel-magit-review-default-unified 10
  "Default unified context lines (-U) if user did not specify any."
  :type 'integer)

(defcustom gptel-magit-review-max-chars 180000
  "Soft max characters for (prompt + diff). If exceeded we reduce -U."
  :type 'integer)

(defcustom gptel-magit-review-min-unified 1
  "Lowest fallback for unified context when shrinking."
  :type 'integer)

(defcustom gptel-magit-review-buffer "*Code Review/"
  "Destination buffer for accumulating reviews."
  :type 'string)

(defcustom gptel-magit-review-system
  "You are a senior engineer performing a code review.
Focus on:
- Design & architecture
- Correctness & edge cases
- Security & performance
- Maintainability & clarity
- Testability & missing tests
Output:
1. Summary
2. Strengths
3. Issues (ordered by priority) with actionable suggestions
4. Suggested tests (if any)
Be concise but specific."
  "Review prompt"
  :type 'string)

(defcustom gptel-magit-review-use-preset t
  "If non-nil, define/apply a preset named code-review instead of setting variables directly."
  :type 'boolean)

(defcustom gptel-magit-review-preset-name 'code-review
  "Symbol name of the preset to define/use when gptel-magit-review-use-preset is non-nil."
  :type 'symbol)

(defcustom gptel-magit-review-model nil
  "Model symbol/string to bind for reviews (nil = keep current gptel-model)."
  :type '(choice (const :tag \"Leave as-is\" nil) symbol string))

(defcustom gptel-magit-review-temperature 0.2
  "Temperature used when applying the preset (if enabled)."
  :type 'number)

(defvar gptel-magit-review--last-args nil)

;; ------------------------------------------------------------
;; Utility
;; ------------------------------------------------------------
(defun gptel-magit-review--repo-root ()
  (or (when (featurep 'magit) (magit-toplevel))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun gptel-magit-review--parse-unified (args)
  (cl-loop for a in args
           if (string-match "\\=-U\\([0-9]+\\)\\'" a)
             return (string-to-number (match-string 1 a))
           if (string-match "\\=--unified=\\([0-9]+\\)\\'" a)
             return (string-to-number (match-string 1 a))))

(defun gptel-magit-review--ensure-unified (args default)
  (if (gptel-magit-review--parse-unified args)
      args
    (cons (format "-U%d" default) args)))

(defun gptel-magit-review--replace-unified (args new)
  (let ((found nil))
    (setq args
          (mapcar (lambda (a)
                    (cond
                     ((string-match "\\=-U[0-9]+\\'" a)
                      (setq found t) (format "-U%d" new))
                     ((string-match "\\=--unified=[0-9]+\\'" a)
                      (setq found t) (format "--unified=%d" new))
                     (t a)))
                  args))
    (unless found (push (format "-U%d" new) args))
    args))

(defun gptel-magit-review--git-diff (args)
  (let ((default-directory (gptel-magit-review--repo-root)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil "diff" args)))
        (unless (eq status 0)
          (error "git diff failed (exit %s)" status))
        (buffer-string)))))

(defun gptel-magit-review--shrink-if-needed (diff args)
  (when (> (length diff) gptel-magit-review-max-chars)
    (let* ((cur (or (gptel-magit-review--parse-unified args)
                    gptel-magit-review-default-unified))
           (ratio (/ (float gptel-magit-review-max-chars)
                     (max 1 (length diff))))
           (new (max gptel-magit-review-min-unified
                     (floor (/ cur ratio)))))
      (when (< new cur)
        (message "Shrinking unified context from %d → %d" cur new)
        (setq args (gptel-magit-review--replace-unified args new))
        (setq diff (gptel-magit-review--git-diff args))
        (when (> (length diff) gptel-magit-review-max-chars)
          (error "Still too large after shrinking; narrow diff or paths"))))
    (list diff args)))

(defun gptel-magit-review--read-args ()
  (let* ((seed (when gptel-magit-review--last-args
                 (string-join gptel-magit-review--last-args " ")))
         (raw (read-string
               "Git diff args (blank = working tree vs HEAD): "
               seed)))
    (if (string-empty-p (string-trim raw)) nil
      (split-string-and-unquote raw))))

(defun gptel-magit-review--read-context ()
  (let ((c (read-string "Additional context (optional): ")))
    (unless (string-empty-p (string-trim c)) c)))

;; ------------------------------------------------------------
;; Buffer / insertion
;; ------------------------------------------------------------
(defun gptel-magit-review--prepare-buffer ()
  (let ((buf (get-buffer-create gptel-magit-review-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless gptel-mode
        (gptel-mode 1)))
    buf))

(defun gptel-magit-review--insert-session (diff args extra)
  (let* ((buf (gptel-magit-review--prepare-buffer))
         (ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (heading (format "/ Review %s (args: %s)"
                          ts (string-join args " ")))
         (prompt-start)
         (prompt-end))
    (with-current-buffer buf
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert heading "\n")
      (when extra
        (insert "* Additional Context\n" extra "\n\n"))
      (insert "* Diff\n#+begin_src diff\n" diff "\n#+end_src\n\n")
      (insert "* Request\n"
              "Please review the diff above following the system directives.\n"
              "Provide: Summary, Strengths, Issues (priority ordered with actionable fixes), "
              "Suggested tests.\n\n"
              "* Model Response\n")
      ;; We'll put point where the model response should appear, but we only send
      ;; the region from heading start to just before “* Model Response”.
      (setq prompt-end (point))
      (save-excursion
        (search-backward heading)
        (setq prompt-start (point)))
      ;; Activate region to send only this block:
      (goto-char prompt-end)
      (set-mark prompt-start)
      (activate-mark))
    (pop-to-buffer buf)
    (list buf prompt-start prompt-end)))

;; ------------------------------------------------------------
;; System directive / preset
;; ------------------------------------------------------------
(defun gptel-magit-review--apply-system ()
  (if gptel-magit-review-use-preset
      (progn
        (gptel-make-preset gptel-magit-review-preset-name
          :system gptel-magit-review-system
          :temperature gptel-magit-review-temperature
          :model (or gptel-magit-review-model gptel-model))
        (gptel--apply-preset gptel-magit-review-preset-name
                             (lambda (sym val)
                               (set (make-local-variable sym) val))))
    ;; Direct variable setting (uses internal gptel--system-message)
    (setq-local gptel-model (or gptel-magit-review-model gptel-model))
    (setq-local gptel--system-message gptel-magit-review-system)
    (setq-local gptel-temperature gptel-magit-review-temperature)))

;; ------------------------------------------------------------
;; Public commands
;; ------------------------------------------------------------
;;;###autoload
(defun gptel-magit-review (args extra-context)
  "Generate an LLM review for git diff ARGS with optional EXTRA-CONTEXT."
  (interactive
   (let ((a (gptel-magit-review--read-args))
         (c (gptel-magit-review--read-context)))
     (list a c)))
  (unless (executable-find "git")
    (user-error "git not found in PATH"))
  (setq gptel-magit-review--last-args args)
  (setq args (gptel-magit-review--ensure-unified
              (or args '()) gptel-magit-review-default-unified))
  (let* ((diff (gptel-magit-review--git-diff args)))
    (when (string-empty-p (string-trim diff))
      (user-error "No changes found"))
    (pcase-let* ((`(,final-diff ,final-args)
                  (gptel-magit-review--shrink-if-needed diff args))
                 (buf-info (gptel-magit-review--insert-session
                            final-diff final-args extra-context)))
      (with-current-buffer (nth 0 buf-info)
        (gptel-magit-review--apply-system)
        ;; gptel-send will send active region only.
        (gptel-send)
        (deactivate-mark)))))

;;;###autoload
(defun gptel-magit-review-staged (&optional extra-context)
  "Review staged (index) changes only."
  (interactive (list (gptel-magit-review--read-context)))
  (gptel-magit-review (list "--cached") extra-context))

(provide 'gptel-magit-review)
;;; gptel-magit-review.el ends here


