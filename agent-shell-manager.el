;;; agent-shell-manager.el --- Buffer manager for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jethro Kuan

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Provides a buffer manager with tabulated list view of all open agent-shell buffers,
;; showing buffer name, session status, and other details.
;;
;; Features:
;; - View all agent-shell buffers in a tabulated list
;; - See real-time status (ready, working, waiting, initializing, killed)
;; - Kill, restart, or create new agent-shells
;; - Manage session modes
;; - View traffic logs for debugging
;; - Auto-refresh every 2 seconds
;; - Killed processes are displayed at the bottom in red
;; - Preview mode: split view with live buffer preview (p to enter, q to exit)
;; - Project grouping: buffers grouped by project root with visual headers
;; - Status filtering: cycle through status filters (f to cycle)
;;
;; Usage:
;;   M-x agent-shell-manager-toggle
;;
;; Key bindings in the manager buffer:
;;   RET   - Switch to agent-shell buffer
;;   g     - Refresh buffer list
;;   k     - Kill agent-shell process
;;   c     - Create new agent-shell
;;   r     - Restart agent-shell
;;   d     - Delete all killed buffers
;;   m     - Set session mode
;;   M     - Set session model
;;   C-c C-c - Interrupt agent
;;   t     - View traffic logs
;;   l     - Toggle logging
;;   p     - Enter preview mode
;;   f     - Cycle status filter
;;   q     - Quit (context-aware: exits preview or closes manager)

;;; Code:

(require 'agent-shell)
(require 'tabulated-list)

(defgroup agent-shell-manager nil
  "Buffer manager for `agent-shell'."
  :group 'agent-shell)

(defcustom agent-shell-manager-side 'bottom
  "Side of the frame to display the `agent-shell' manager.
Can be 'left, 'right, 'top, 'bottom, or nil.  When nil, buffer display
is controlled by the user's `display-buffer-alist'."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right)
          (const :tag "Top" top)
          (const :tag "Bottom" bottom)
          (const :tag "User-controlled" nil))
  :group 'agent-shell-manager)

(defcustom agent-shell-manager-transient nil
  "When non-nil, automatically hide the manager window after actions.
This includes switching to a shell buffer with RET.  When enabled,
the manager window can also be closed by `delete-other-windows' (C-x 1)."
  :type 'boolean
  :group 'agent-shell-manager)

(defvar agent-shell-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-manager-goto)
    (define-key map (kbd "g") #'agent-shell-manager-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k") #'agent-shell-manager-kill)
    (define-key map (kbd "c") #'agent-shell-manager-new)
    (define-key map (kbd "r") #'agent-shell-manager-restart)
    (define-key map (kbd "d") #'agent-shell-manager-delete-killed)
    (define-key map (kbd "m") #'agent-shell-manager-set-mode)
    (define-key map (kbd "M") #'agent-shell-manager-set-model)
    (define-key map (kbd "C-c C-c") #'agent-shell-manager-interrupt)
    (define-key map (kbd "t") #'agent-shell-manager-view-traffic)
    (define-key map (kbd "l") #'agent-shell-manager-toggle-logging)
    map)
  "Keymap for `agent-shell-manager-mode'.")

(defvar-local agent-shell-manager--refresh-timer nil
  "Timer for auto-refreshing the buffer list.")

(defvar agent-shell-manager--global-buffer nil
  "The global manager buffer for `agent-shell' buffer list.")

;; ============================================================
;; Preview Mode
;; ============================================================

(defvar agent-shell-manager-preview-active nil
  "Non-nil when preview mode is active.")

(defvar agent-shell-manager-preview-saved-config nil
  "Window configuration saved before entering preview mode.")

(defvar agent-shell-manager-preview-buffer nil
  "Buffer currently being previewed.")

(defvar agent-shell-manager-preview-highlight-overlay nil
  "Overlay used to highlight the current row during preview mode.")

(defface agent-shell-manager-preview-highlight
  '((t :background "#3c3836" :extend t))
  "Face for the currently previewed entry in the manager.")

;; ============================================================
;; Status Filtering
;; ============================================================

(defvar agent-shell-manager-filter-status nil
  "Current status filter. nil = show all.")

(defvar agent-shell-manager-filter-cycle '(nil "Working" "Waiting" "Ready" "Killed")
  "Cycle of filter values. nil means show all.")

;; ============================================================
;; Mode Definition
;; ============================================================

(define-derived-mode agent-shell-manager-mode tabulated-list-mode "Agent-Shell-Buffers"
  "Major mode for listing `agent-shell' buffers.

\\{agent-shell-manager-mode-map}"
  (setq tabulated-list-format
        [("Buffer" 40 t)
         ("Status" 15 t)
         ("Mode" 15 t)
         ("Model" 21 t)
         ("Pending Permissions" 20 t)
         ("Path" 20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Buffer" nil))
  (tabulated-list-init-header)

  (when agent-shell-manager--refresh-timer
    (cancel-timer agent-shell-manager--refresh-timer))

  ;; Set up auto-refresh timer (refresh every 2 seconds)
  (setq agent-shell-manager--refresh-timer
        (run-with-timer 2 2 #'agent-shell-manager-refresh))

  ;; Skip group header lines during navigation
  (add-hook 'post-command-hook #'agent-shell-manager--skip-group-headers nil t)

  ;; Cancel timer when buffer is killed
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when agent-shell-manager--refresh-timer
                (cancel-timer agent-shell-manager--refresh-timer)
                (setq agent-shell-manager--refresh-timer nil)))
            nil t))

;; ============================================================
;; Status Helpers
;; ============================================================

(defun agent-shell-manager--get-status (buffer)
  "Get the current status of `agent-shell' BUFFER.
Returns one of: waiting, ready, working, killed, or unknown."
  (with-current-buffer buffer
    (if (not (boundp 'agent-shell--state))
        "unknown"
      (let* ((state agent-shell--state)
             (acp-proc (map-nested-elt state '(:client :process)))
             (acp-process-alive (and acp-proc
                                     (processp acp-proc)
                                     (process-live-p acp-proc)
                                     (memq (process-status acp-proc) '(run open listen connect stop))))
             (comint-proc (get-buffer-process (current-buffer)))
             (comint-process-alive (and comint-proc
                                        (processp comint-proc)
                                        (process-live-p comint-proc)
                                        (memq (process-status comint-proc) '(run open listen connect stop))))
             (process-alive (and acp-process-alive comint-process-alive)))
        (cond
         ((or (not comint-proc)
              (and (processp comint-proc)
                   (not comint-process-alive)))
          "killed")
         ((and (map-elt state :client)
               (or (not acp-proc)
                   (and (processp acp-proc)
                        (not acp-process-alive))))
          "killed")
         ((and process-alive
               (map-elt state :tool-calls)
               (> (length (map-elt state :tool-calls)) 0))
          (let ((has-pending-permission
                 (seq-find (lambda (tool-call)
                             (map-elt (cdr tool-call) :permission-request-id))
                           (map-elt state :tool-calls))))
            (if has-pending-permission
                "waiting"
              "working")))
         ((and process-alive
               (fboundp 'shell-maker-busy)
               (shell-maker-busy))
          "working")
         ((and process-alive
               (map-nested-elt state '(:session :id)))
          "ready")
         ((not (map-elt state :initialized))
          "initializing")
         (t "unknown"))))))

(defun agent-shell-manager--get-buffer-name (buffer)
  "Get the buffer name for BUFFER."
  (buffer-name buffer))

(defun agent-shell-manager--get-session-status (buffer)
  "Get session status for BUFFER."
  (with-current-buffer buffer
    (let ((status (agent-shell-manager--get-status buffer)))
      (if (string= status "killed")
          "none"
        (if (and (boundp 'agent-shell--state)
                 (map-nested-elt agent-shell--state '(:session :id)))
            "active"
          "none")))))

(defun agent-shell-manager--get-combined-status (buffer)
  "Get combined status for BUFFER that merges operational and session state."
  (with-current-buffer buffer
    (let ((status (agent-shell-manager--get-status buffer))
          (session (agent-shell-manager--get-session-status buffer)))
      (cond
       ((string= status "killed")
        (propertize "Killed" 'face 'error))
       ((and (string= status "initializing")
             (string= session "none"))
        (propertize "Starting..." 'face 'font-lock-comment-face))
       ((and (string= status "ready")
             (string= session "none"))
        (propertize "No Session" 'face 'font-lock-comment-face))
       ((and (string= status "ready")
             (string= session "active"))
        (propertize "Ready" 'face 'success))
       ((string= status "working")
        (propertize "Working" 'face 'warning))
       ((string= status "waiting")
        (propertize "Waiting" 'face 'font-lock-keyword-face))
       (t
        (propertize "Unknown" 'face 'font-lock-comment-face))))))

(defun agent-shell-manager--get-session-mode (buffer)
  "Get the current session mode for BUFFER."
  (with-current-buffer buffer
    (if (and (boundp 'agent-shell--state)
             (map-nested-elt agent-shell--state '(:session :mode-id)))
        (or (agent-shell--resolve-session-mode-name
             (map-nested-elt agent-shell--state '(:session :mode-id))
             (map-nested-elt agent-shell--state '(:session :modes)))
            "-")
      "-")))

(defun agent-shell-manager--get-agent-kind (buffer)
  "Get the agent kind for BUFFER by parsing the buffer name."
  (with-current-buffer buffer
    (let ((buffer-name (buffer-name)))
      (if (string-match "^\\(.*?\\) Agent @ " buffer-name)
          (match-string 1 buffer-name)
        "-"))))

(defun agent-shell-manager--get-model-id (buffer)
  "Get the current model ID for BUFFER."
  (with-current-buffer buffer
    (if (and (boundp 'agent-shell--state)
             (map-nested-elt agent-shell--state '(:session :model-id)))
        (let* ((model-id (map-nested-elt agent-shell--state '(:session :model-id)))
               (models (map-nested-elt agent-shell--state '(:session :models)))
               (model-info (seq-find (lambda (model)
                                       (string= (map-elt model :model-id) model-id))
                                     models)))
          (or (and model-info (map-elt model-info :name))
              model-id))
      "-")))

(defun agent-shell-manager--count-pending-permissions (buffer)
  "Count the number of pending permission requests for BUFFER."
  (with-current-buffer buffer
    (if (and (boundp 'agent-shell--state)
             (map-elt agent-shell--state :tool-calls))
        (let ((count 0))
          (map-do
           (lambda (_tool-call-id tool-call-data)
             (when (and (map-elt tool-call-data :permission-request-id)
                        (let ((status (map-elt tool-call-data :status)))
                          (equal status "pending")))
               (setq count (1+ count))))
           (map-elt agent-shell--state :tool-calls))
          (if (> count 0)
              (propertize (number-to-string count)
                          'face 'warning
                          'font-lock-face 'warning)
            "-"))
      "-")))

(defun agent-shell-manager--status-face (status)
  "Return face for STATUS string."
  (cond
   ((string= status "ready") 'success)
   ((string= status "working") 'warning)
   ((string= status "waiting") 'font-lock-keyword-face)
   ((string= status "initializing") 'font-lock-comment-face)
   ((string= status "killed") 'error)
   (t 'default)))

(defun agent-shell-manager--get-cwd (buffer)
  "Get the current session directory for BUFFER."
  (with-current-buffer buffer
    default-directory))

;; ============================================================
;; Project Grouping
;; ============================================================

(defun agent-shell-manager--buffer-project-root (buffer)
  "Get the project root for BUFFER.
Uses projectile if available, falls back to `default-directory'."
  (with-current-buffer buffer
    (abbreviate-file-name
     (or (and (fboundp 'projectile-project-root)
              (ignore-errors (projectile-project-root)))
         default-directory))))

;; ============================================================
;; Entries (with filtering + project sorting)
;; ============================================================

(defun agent-shell-manager--entries ()
  "Return list of entries for tabulated-list.
Applies status filtering and sorts by project root with killed buffers
at the bottom of each group."
  (let* ((buffers (agent-shell-buffers))
         (buffers (if (listp buffers) buffers (list buffers)))
         (buffers (seq-filter #'buffer-live-p buffers))
         (entries (mapcar
                   (lambda (buffer)
                     (let* ((buffer-name (buffer-name buffer))
                            (status (agent-shell-manager--get-combined-status buffer))
                            (mode (agent-shell-manager--get-session-mode buffer))
                            (model (agent-shell-manager--get-model-id buffer))
                            (perms (agent-shell-manager--count-pending-permissions buffer))
                            (path (abbreviate-file-name (agent-shell-manager--get-cwd buffer))))
                       (list buffer
                             (vector
                              buffer-name
                              status
                              mode
                              model
                              perms
                              path))))
                   buffers))
         ;; Apply status filter
         (entries (if agent-shell-manager-filter-status
                      (seq-filter
                       (lambda (entry)
                         (let ((status (substring-no-properties (aref (cadr entry) 1))))
                           (string-equal-ignore-case status agent-shell-manager-filter-status)))
                       entries)
                    entries))
         ;; Annotate with project root for sorting
         (annotated (mapcar
                     (lambda (entry)
                       (let* ((buffer (car entry))
                              (project (if (buffer-live-p buffer)
                                           (agent-shell-manager--buffer-project-root buffer)
                                         "~/")))
                         (cons project entry)))
                     entries)))
    ;; Sort by project (alpha), killed-to-bottom within each group
    (setq annotated
          (sort annotated
                (lambda (a b)
                  (let ((proj-a (car a))
                        (proj-b (car b))
                        (status-a (substring-no-properties (aref (caddr a) 1)))
                        (status-b (substring-no-properties (aref (caddr b) 1))))
                    (cond
                     ((not (string= proj-a proj-b))
                      (string< proj-a proj-b))
                     ((and (string= status-a "Killed") (not (string= status-b "Killed")))
                      nil)
                     ((and (not (string= status-a "Killed")) (string= status-b "Killed"))
                      t)
                     (t nil))))))
    ;; Strip project annotations
    (mapcar #'cdr annotated)))

;; ============================================================
;; Group Headers
;; ============================================================

(defun agent-shell-manager--inject-group-headers ()
  "Insert visual group headers between project boundaries.
Headers have `agent-shell-manager-group-header' text property for cleanup."
  (when (derived-mode-p 'agent-shell-manager-mode)
    (let ((inhibit-read-only t)
          (last-project nil))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((entry-buffer (tabulated-list-get-id))
                 (project (when (and entry-buffer (buffer-live-p entry-buffer))
                            (agent-shell-manager--buffer-project-root entry-buffer))))
            (when (and project (not (equal project last-project)))
              (let* ((header-text (concat "-- " project " "
                                          (make-string (max 1 (- 60 (length project) 4)) ?-)
                                          "\n"))
                     (propertized (propertize header-text
                                             'face 'font-lock-comment-face
                                             'agent-shell-manager-group-header t)))
                (insert propertized))
              (setq last-project project)))
          (forward-line 1))))))

(defun agent-shell-manager--remove-group-headers ()
  "Remove all group header lines from the manager buffer."
  (when (derived-mode-p 'agent-shell-manager-mode)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (get-text-property (point) 'agent-shell-manager-group-header)
              (delete-region (line-beginning-position) (1+ (line-end-position)))
            (forward-line 1)))))))

(defun agent-shell-manager--skip-group-headers ()
  "Post-command-hook to skip cursor past group header lines."
  (when (and (derived-mode-p 'agent-shell-manager-mode)
             (get-text-property (line-beginning-position) 'agent-shell-manager-group-header))
    (let ((direction (cond
                      ((memq this-command '(next-line evil-next-line evil-next-visual-line
                                            evil-goto-line)) 1)
                      ((memq this-command '(previous-line evil-previous-line evil-previous-visual-line
                                            evil-goto-first-line)) -1)
                      (t nil))))
      (when direction
        (forward-line direction)
        (while (and (not (eobp)) (not (bobp))
                    (get-text-property (line-beginning-position) 'agent-shell-manager-group-header))
          (forward-line direction))
        (when (eobp) (forward-line -1))))))

;; ============================================================
;; Refresh (with group headers, no scroll drift)
;; ============================================================

(defun agent-shell-manager-refresh ()
  "Refresh the buffer list with group headers.
Preserves cursor position on the same entry. Only restores scroll
position when the manager window is actively selected to prevent
drift from the auto-refresh timer."
  (interactive)
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (let* ((target-buf (tabulated-list-get-id))
             (mgr-win (get-buffer-window (current-buffer)))
             ;; Only track scroll when we're the selected window
             (is-selected (eq mgr-win (selected-window)))
             (line-offset (when (and is-selected mgr-win)
                            (count-lines (window-start mgr-win) (point)))))
        ;; Remove old headers, refresh data, re-inject headers
        (agent-shell-manager--remove-group-headers)
        (setq tabulated-list-entries (agent-shell-manager--entries))
        (tabulated-list-print t)
        (agent-shell-manager--inject-group-headers)
        ;; Restore cursor to the same buffer entry
        (when target-buf
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (if (eq (tabulated-list-get-id) target-buf)
                  (setq found t)
                (forward-line 1)))))
        ;; Only restore scroll when we're the active window
        (when (and is-selected mgr-win (window-live-p mgr-win) line-offset)
          (set-window-start mgr-win
                            (save-excursion
                              (forward-line (- line-offset))
                              (point))
                            t))
        ;; Re-apply highlight overlay if preview mode is active
        (when agent-shell-manager-preview-active
          (if (and agent-shell-manager-preview-highlight-overlay
                   (overlay-buffer agent-shell-manager-preview-highlight-overlay))
              (move-overlay agent-shell-manager-preview-highlight-overlay
                            (line-beginning-position) (1+ (line-end-position)))
            (setq agent-shell-manager-preview-highlight-overlay
                  (make-overlay (line-beginning-position) (1+ (line-end-position))))
            (overlay-put agent-shell-manager-preview-highlight-overlay
                         'face 'agent-shell-manager-preview-highlight)
            (overlay-put agent-shell-manager-preview-highlight-overlay 'priority 100)))))))

;; ============================================================
;; Status Filtering UI
;; ============================================================

(defun agent-shell-manager--update-header-line ()
  "Update the manager buffer's header-line to show the current filter."
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (setq header-line-format
            (if agent-shell-manager-filter-status
                (format " Filter: %s  [f to cycle]" agent-shell-manager-filter-status)
              nil))
      (unless agent-shell-manager-filter-status
        (tabulated-list-init-header)))))

(defun agent-shell-manager-cycle-filter ()
  "Cycle through status filters. Updates header-line and refreshes."
  (interactive)
  (let* ((current-pos (cl-position agent-shell-manager-filter-status
                                   agent-shell-manager-filter-cycle
                                   :test #'equal))
         (next-pos (mod (1+ (or current-pos -1)) (length agent-shell-manager-filter-cycle))))
    (setq agent-shell-manager-filter-status (nth next-pos agent-shell-manager-filter-cycle))
    (agent-shell-manager--update-header-line)
    (agent-shell-manager-refresh)
    (message "Filter: %s" (or agent-shell-manager-filter-status "All"))))

;; ============================================================
;; Preview Mode
;; ============================================================

(defun agent-shell-manager--preview-window ()
  "Find the preview window dynamically.
Walks all windows and returns the one NOT showing the manager buffer."
  (when agent-shell-manager-preview-active
    (let ((mgr-buf (get-buffer "*Agent-Shell Buffers*"))
          (result nil))
      ;; First try: find window showing our preview buffer
      (when agent-shell-manager-preview-buffer
        (walk-windows
         (lambda (w)
           (when (and (not result)
                      (not (eq (window-buffer w) mgr-buf))
                      (eq (window-buffer w) agent-shell-manager-preview-buffer))
             (setq result w)))
         nil (selected-frame)))
      ;; Fallback: any non-manager window
      (unless result
        (walk-windows
         (lambda (w)
           (when (and (not result)
                      (not (eq (window-buffer w) mgr-buf)))
             (setq result w)))
         nil (selected-frame)))
      result)))

(defun agent-shell-manager--preview-update ()
  "Update the preview pane to show the buffer under cursor.
Called via `post-command-hook' in the manager buffer."
  (when (and agent-shell-manager-preview-active
             (derived-mode-p 'agent-shell-manager-mode))
    ;; Update highlight overlay on current line
    (if (and agent-shell-manager-preview-highlight-overlay
             (overlay-buffer agent-shell-manager-preview-highlight-overlay))
        (move-overlay agent-shell-manager-preview-highlight-overlay
                      (line-beginning-position) (1+ (line-end-position)))
      (setq agent-shell-manager-preview-highlight-overlay
            (make-overlay (line-beginning-position) (1+ (line-end-position))))
      (overlay-put agent-shell-manager-preview-highlight-overlay
                   'face 'agent-shell-manager-preview-highlight)
      (overlay-put agent-shell-manager-preview-highlight-overlay 'priority 100))
    ;; Update preview pane
    (when-let* ((buffer (tabulated-list-get-id)))
      (when (buffer-live-p buffer)
        (let ((preview-win (agent-shell-manager--preview-window))
              (switched (not (eq buffer agent-shell-manager-preview-buffer))))
          (when (and preview-win (window-live-p preview-win))
            (setq agent-shell-manager-preview-buffer buffer)
            (set-window-buffer preview-win buffer)
            (when switched
              (with-selected-window preview-win
                (goto-char (point-max))
                (recenter -3)))))))))

(defun agent-shell-manager-enter-preview ()
  "Enter preview mode.
Saves window config, creates split layout with preview on top
and manager on bottom (15 lines)."
  (interactive)
  (when agent-shell-manager-preview-active
    (message "Already in preview mode")
    (cl-return-from agent-shell-manager-enter-preview))
  (setq agent-shell-manager-preview-saved-config (current-window-configuration))
  (let ((mgr-buf (or agent-shell-manager--global-buffer
                     (get-buffer "*Agent-Shell Buffers*"))))
    (unless mgr-buf
      (user-error "No manager buffer. Open it first with agent-shell-manager-toggle"))
    ;; Close the manager's existing window first
    (let ((mgr-win (get-buffer-window mgr-buf)))
      (when (and mgr-win (window-live-p mgr-win))
        (when (eq (selected-window) mgr-win)
          (select-window (window-main-window)))
        (delete-window mgr-win)))
    ;; Clean slate
    (delete-other-windows)
    ;; Current window becomes the preview pane (top, large)
    (let ((first-shell (car (agent-shell-buffers))))
      (when first-shell
        (setq agent-shell-manager-preview-buffer first-shell)
        (set-window-buffer (selected-window) first-shell)))
    ;; Split: bottom window for the manager
    (let ((manager-win (split-window-below -15)))
      (select-window manager-win)
      (switch-to-buffer mgr-buf)
      (agent-shell-manager-refresh)
      (setq agent-shell-manager-preview-active t)
      (add-hook 'post-command-hook #'agent-shell-manager--preview-update nil t)
      (agent-shell-manager--preview-update)
      (message "Preview mode: j/k navigate, RET select, q quit"))))

(defun agent-shell-manager-exit-preview ()
  "Exit preview mode and restore original window configuration."
  (setq agent-shell-manager-preview-active nil)
  (setq agent-shell-manager-preview-buffer nil)
  (remove-hook 'post-command-hook #'agent-shell-manager--preview-update t)
  (when (and agent-shell-manager-preview-highlight-overlay
             (overlay-buffer agent-shell-manager-preview-highlight-overlay))
    (delete-overlay agent-shell-manager-preview-highlight-overlay))
  (setq agent-shell-manager-preview-highlight-overlay nil)
  (when agent-shell-manager-preview-saved-config
    (set-window-configuration agent-shell-manager-preview-saved-config)
    (setq agent-shell-manager-preview-saved-config nil)))

;; ============================================================
;; Context-Aware Navigation
;; ============================================================

(defun agent-shell-manager-select ()
  "Select the buffer at point.
If in preview mode, exit it first."
  (interactive)
  (if agent-shell-manager-preview-active
      (let ((buffer (tabulated-list-get-id)))
        (agent-shell-manager-exit-preview)
        (when (and buffer (buffer-live-p buffer))
          (agent-shell-manager-goto)))
    (agent-shell-manager-goto)))

(defun agent-shell-manager-quit ()
  "Context-aware quit.
If preview mode is active, exit preview. Otherwise close normally."
  (interactive)
  (if agent-shell-manager-preview-active
      (agent-shell-manager-exit-preview)
    (quit-window)))

;; ============================================================
;; Window Management
;; ============================================================

(defun agent-shell-manager--hide-window ()
  "Hide the manager window if `agent-shell-manager-transient' is non-nil."
  (when agent-shell-manager-transient
    (when-let* ((buffer agent-shell-manager--global-buffer)
                (window (and (buffer-live-p buffer)
                             (get-buffer-window buffer))))
      (delete-window window))))

(defun agent-shell-manager-goto ()
  "Go to the `agent-shell' buffer at point.
If `agent-shell-manager-transient' is non-nil, hide the manager window."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (if (buffer-live-p buffer)
        (let ((buffer-window (get-buffer-window buffer t))
              (agent-shell-window nil))
          (cond
           (buffer-window
            (select-window buffer-window))
           (t
            (walk-windows
             (lambda (win)
               (when (and (not agent-shell-window)
                          (not (eq win (selected-window)))
                          (with-current-buffer (window-buffer win)
                            (derived-mode-p 'agent-shell-mode)))
                 (setq agent-shell-window win)))
             nil t)
            (if agent-shell-window
                (progn
                  (set-window-buffer agent-shell-window buffer)
                  (select-window agent-shell-window))
              (agent-shell--display-buffer buffer))))
          (agent-shell-manager--hide-window))
      (user-error "Buffer no longer exists"))))

(defun agent-shell-manager-kill ()
  "Kill the `agent-shell' process at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (when (yes-or-no-p (format "Kill agent-shell process in %s? " (buffer-name buffer)))
      (with-current-buffer buffer
        (when (and (boundp 'agent-shell--state)
                   (map-elt agent-shell--state :client)
                   (map-nested-elt agent-shell--state '(:client :process)))
          (let ((proc (map-nested-elt agent-shell--state '(:client :process))))
            (when (process-live-p proc)
              (comint-send-eof)
              (message "Sent EOF to agent-shell process in %s" (buffer-name buffer))))))
      (run-with-timer 0.1 nil #'agent-shell-manager-refresh))))

(defun agent-shell-manager-new ()
  "Create a new `agent-shell'."
  (interactive)
  (agent-shell t)
  (if agent-shell-manager-transient
      (agent-shell-manager--hide-window)
    (agent-shell-manager-refresh)))

(defun agent-shell-manager--get-buffer-config (buffer)
  "Try to determine the config used for BUFFER."
  (with-current-buffer buffer
    (when (derived-mode-p 'agent-shell-mode)
      (let ((buffer-name-prefix (replace-regexp-in-string " Agent @ .*$" "" (buffer-name))))
        (seq-find (lambda (config)
                    (string= buffer-name-prefix (map-elt config :buffer-name)))
                  agent-shell-agent-configs)))))

(defun agent-shell-manager-restart ()
  "Restart the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (let ((config (agent-shell-manager--get-buffer-config buffer))
          (buffer-name (buffer-name buffer)))
      (when (yes-or-no-p (format "Restart agent-shell %s? " buffer-name))
        (with-current-buffer buffer
          (when (and (boundp 'agent-shell--state)
                     (map-elt agent-shell--state :client)
                     (map-nested-elt agent-shell--state '(:client :process)))
            (let ((proc (map-nested-elt agent-shell--state '(:client :process))))
              (when (process-live-p proc)
                (kill-process proc)))))
        (kill-buffer buffer)
        (if config
            (agent-shell-start :config config)
          (agent-shell t))
        (agent-shell-manager-refresh)
        (message "Restarted %s" buffer-name)))))

(defun agent-shell-manager-delete-killed ()
  "Delete all killed `agent-shell' buffers from the list."
  (interactive)
  (let ((killed-buffers (seq-filter
                         (lambda (buffer)
                           (and (buffer-live-p buffer)
                                (string= (agent-shell-manager--get-status buffer) "killed")))
                         (mapcar #'get-buffer (agent-shell-buffers)))))
    (if (null killed-buffers)
        (message "No killed buffers to delete")
      (when (yes-or-no-p (format "Delete %d killed buffer%s? "
                                 (length killed-buffers)
                                 (if (= (length killed-buffers) 1) "" "s")))
        (dolist (buffer killed-buffers)
          (kill-buffer buffer))
        (agent-shell-manager-refresh)
        (message "Deleted %d killed buffer%s"
                 (length killed-buffers)
                 (if (= (length killed-buffers) 1) "" "s"))))))

(defun agent-shell-manager-set-mode ()
  "Set session mode for the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-set-session-mode))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-set-model ()
  "Set session model for the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-set-session-model))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-interrupt ()
  "Interrupt the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-interrupt))
    (agent-shell-manager-refresh)))

(defun agent-shell-manager-view-traffic ()
  "View traffic logs for the `agent-shell' at point."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-mode)
        (user-error "Not an agent-shell buffer"))
      (agent-shell-view-traffic))))

(defun agent-shell-manager-toggle-logging ()
  "Toggle logging for `agent-shell'."
  (interactive)
  (agent-shell-toggle-logging)
  (agent-shell-manager-refresh))

;; ============================================================
;; Toggle
;; ============================================================

;;;###autoload
(defun agent-shell-manager-toggle ()
  "Toggle the `agent-shell' buffer list window."
  (interactive)
  (let* ((buffer (get-buffer-create "*Agent-Shell Buffers*"))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        (delete-window window)
      (let ((window (if agent-shell-manager-side
                        (let ((size-param (if (memq agent-shell-manager-side
                                                    '(left right))
                                              'window-width
                                            'window-height)))
                          (display-buffer-in-side-window
                           buffer
                           `((side . ,agent-shell-manager-side)
                             (slot . 0)
                             (,size-param . 0.3)
                             (preserve-size . ,(if (memq
                                                    agent-shell-manager-side
                                                    '(left right))
                                                   '(t . nil)
                                                 '(nil . t)))
                             ,@(unless agent-shell-manager-transient
                                 '((window-parameters .
                                    ((no-delete-other-windows . t))))))))
                      (display-buffer buffer))))
        (setq agent-shell-manager--global-buffer buffer)
        (with-current-buffer buffer
          (agent-shell-manager-mode)
          (agent-shell-manager-refresh))
        (set-window-dedicated-p window t)
        (select-window window)))))

(provide 'agent-shell-manager)

;;; agent-shell-manager.el ends here
