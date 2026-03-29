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

(defcustom agent-shell-manager-frame-alist
  '((name . "Agent Shell Manager")
    (width . 110)
    (height . 30)
    (minibuffer . t))
  "Frame parameters for the dedicated manager frame.
Used by `agent-shell-manager-toggle-frame' when creating the
standalone manager frame."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'agent-shell-manager)

(defvar agent-shell-manager--frame nil
  "The dedicated frame for the agent-shell manager, if any.")

(defvar agent-shell-manager--target-frame nil
  "Frame where shells are displayed when selected from the manager frame.
Automatically set to the previously-active frame when
`agent-shell-manager-toggle-frame' creates the manager frame.")

(defvar agent-shell-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-manager-goto)
    (define-key map (kbd "o") #'agent-shell-manager-peek)
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

(defvar agent-shell-manager-after-refresh-hook nil
  "Hook run after `agent-shell-manager-refresh' reprints the buffer.
Preview mode uses this to re-sync overlay and preview pane after
timer-driven refreshes (where `post-command-hook' does not fire).")

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
  '((t :background "#32302f" :underline (:color "#d65d0e" :style line) :extend t))
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
  (setq tabulated-list-groups #'agent-shell-manager--groups)
  (tabulated-list-init-header)

  (when agent-shell-manager--refresh-timer
    (cancel-timer agent-shell-manager--refresh-timer))

  ;; Set up auto-refresh timer (refresh every 2 seconds)
  (setq agent-shell-manager--refresh-timer
        (run-with-timer 2 2 #'agent-shell-manager-refresh))

  ;; Line numbers for quick navigation
  (display-line-numbers-mode 1)

  ;; Highlight current line during navigation
  (hl-line-mode 1)

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
;; Data: Grouped Entries (filtering + project grouping)
;; ============================================================

(defun agent-shell-manager--groups ()
  "Return entries grouped by project root for `tabulated-list-groups'.
Each group is (GROUP-NAME ENTRIES) where GROUP-NAME is a formatted
project path string and ENTRIES is a list of (BUFFER [COLUMNS...]).
Applies status filtering. Killed buffers sort to bottom within each group."
  (let* ((buffers (agent-shell-buffers))
         (buffers (if (listp buffers) buffers (list buffers)))
         (buffers (seq-filter #'buffer-live-p buffers))
         ;; Build entries
         (entries (mapcar
                   (lambda (buffer)
                     (let* ((buffer-name (buffer-name buffer))
                            (status (agent-shell-manager--get-combined-status buffer))
                            (mode (agent-shell-manager--get-session-mode buffer))
                            (model (agent-shell-manager--get-model-id buffer))
                            (perms (agent-shell-manager--count-pending-permissions buffer))
                            (path (abbreviate-file-name (agent-shell-manager--get-cwd buffer))))
                       (list buffer
                             (vector buffer-name status mode model perms path))))
                   buffers))
         ;; Apply status filter
         (entries (if agent-shell-manager-filter-status
                      (seq-filter
                       (lambda (entry)
                         (let ((status (substring-no-properties (aref (cadr entry) 1))))
                           (string-equal-ignore-case status agent-shell-manager-filter-status)))
                       entries)
                    entries))
         ;; Group by project root
         (groups (make-hash-table :test #'equal)))
    ;; Bucket entries by project
    (dolist (entry entries)
      (let* ((buffer (car entry))
             (project (if (buffer-live-p buffer)
                          (agent-shell-manager--buffer-project-root buffer)
                        "~/")))
        (puthash project (cons entry (gethash project groups)) groups)))
    ;; Build sorted group list
    (let ((result nil))
      (maphash
       (lambda (project group-entries)
         ;; Sort killed buffers to bottom within group
         (setq group-entries
               (sort (nreverse group-entries)
                     (lambda (a b)
                       (let ((sa (substring-no-properties (aref (cadr a) 1)))
                             (sb (substring-no-properties (aref (cadr b) 1))))
                         (cond
                          ((and (string= sa "Killed") (not (string= sb "Killed"))) nil)
                          ((and (not (string= sa "Killed")) (string= sb "Killed")) t)
                          (t nil))))))
         ;; Format group name
         (let ((name (propertize
                      (concat " " project " "
                              (make-string (max 1 (- 60 (length project) 2)) ?-))
                      'face 'font-lock-comment-face)))
           (push (cons name group-entries) result)))
       groups)
      ;; Sort groups alphabetically by project path
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun agent-shell-manager--skip-group-headers ()
  "Post-command-hook to skip cursor past group header lines.
Group headers inserted by `tabulated-list-groups' have no entry ID,
so `tabulated-list-get-id' returns nil on those lines.
Also triggers preview update after cursor settles, ensuring preview
always sees the final cursor position (not an intermediate header line)."
  (when (derived-mode-p 'agent-shell-manager-mode)
    (when (null (tabulated-list-get-id))
      (let ((direction (cond
                        ((memq this-command '(next-line evil-next-line evil-next-visual-line
                                              evil-goto-line)) 1)
                        ((memq this-command '(previous-line evil-previous-line evil-previous-visual-line
                                              evil-goto-first-line)) -1)
                        (t 1))))
        (forward-line direction)
        (while (and (not (eobp)) (not (bobp))
                    (null (tabulated-list-get-id)))
          (forward-line direction))
        (when (eobp) (forward-line -1))))
    ;; Always update preview after cursor has settled
    (agent-shell-manager--preview-update)))

;; ============================================================
;; Refresh
;; ============================================================

(defun agent-shell-manager-refresh ()
  "Refresh the buffer list.
Uses `tabulated-list-print' with REMEMBER-POS to preserve cursor.
Group headers are handled natively by `tabulated-list-groups'.
Runs `agent-shell-manager-after-refresh-hook' so preview mode can re-sync."
  (interactive)
  (when (and agent-shell-manager--global-buffer
             (buffer-live-p agent-shell-manager--global-buffer))
    (with-current-buffer agent-shell-manager--global-buffer
      (tabulated-list-print t)
      ;; Re-apply hl-line highlight — tabulated-list-print rebuilds the
      ;; buffer contents which destroys the hl-line overlay.
      (when (bound-and-true-p hl-line-mode)
        (hl-line-highlight))
      (run-hooks 'agent-shell-manager-after-refresh-hook))))

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
Walks all windows and returns the one NOT showing the manager buffer.
When in the dedicated manager frame, searches the target frame instead."
  (when agent-shell-manager-preview-active
    (let* ((mgr-buf (get-buffer "*Agent-Shell Buffers*"))
           (search-frame (if (and (agent-shell-manager--in-manager-frame-p)
                                  (agent-shell-manager--target-frame-live-p))
                             agent-shell-manager--target-frame
                           (selected-frame)))
           (result nil))
      ;; First try: find window showing our preview buffer
      (when agent-shell-manager-preview-buffer
        (walk-windows
         (lambda (w)
           (when (and (not result)
                      (not (eq (window-buffer w) mgr-buf))
                      (eq (window-buffer w) agent-shell-manager-preview-buffer))
             (setq result w)))
         nil search-frame))
      ;; Fallback: any non-manager window
      (unless result
        (walk-windows
         (lambda (w)
           (when (and (not result)
                      (not (eq (window-buffer w) mgr-buf)))
             (setq result w)))
         nil search-frame))
      result)))

(defun agent-shell-manager--preview-update ()
  "Update the preview highlight and pane to match cursor position.
Called via `post-command-hook' (user commands) and
`agent-shell-manager-after-refresh-hook' (timer-driven refreshes).
Does nothing if on a group header (nil ID) — the skip-headers hook
will move cursor to a real entry, which re-triggers this via post-command-hook."
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
    ;; Update preview pane (only for real entries, not group headers)
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

(defun agent-shell-manager--preview-after-refresh ()
  "Re-sync preview state after a timer-driven refresh.
Hooked into `agent-shell-manager-after-refresh-hook' while preview is active."
  (when agent-shell-manager-preview-active
    (agent-shell-manager--preview-update)))

(defun agent-shell-manager-enter-preview ()
  "Enter preview mode.
In the dedicated manager frame, previews are shown in the target
frame — the manager stays full-frame.  Otherwise, creates a split
layout with preview on top and manager on bottom (15 lines)."
  (interactive)
  (if agent-shell-manager-preview-active
      (message "Already in preview mode")
    (if (and (agent-shell-manager--in-manager-frame-p)
             (agent-shell-manager--target-frame-live-p))
        ;; Cross-frame preview: no split needed, target frame IS the preview
        (progn
          (setq agent-shell-manager-preview-active t)
          (add-hook 'agent-shell-manager-after-refresh-hook
                    #'agent-shell-manager--preview-after-refresh)
          (agent-shell-manager--preview-update)
          (message "Preview mode (cross-frame): j/k navigate, RET select, q quit"))
      ;; Same-frame preview: split layout
      (setq agent-shell-manager-preview-saved-config (current-window-configuration))
      (let ((mgr-buf (or agent-shell-manager--global-buffer
                         (get-buffer "*Agent-Shell Buffers*"))))
        (unless mgr-buf
          (user-error "No manager buffer. Open it first with agent-shell-manager-toggle"))
        ;; Tear down side windows so we get a clean frame for the preview layout.
        (when (window-parameter (get-buffer-window mgr-buf) 'window-side)
          (window-toggle-side-windows))
        ;; Now select a live window and clean slate
        (when (not (window-live-p (selected-window)))
          (select-window (frame-first-window)))
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
          (add-hook 'agent-shell-manager-after-refresh-hook
                    #'agent-shell-manager--preview-after-refresh)
          (agent-shell-manager--preview-update)
          (message "Preview mode: j/k navigate, RET select, q quit"))))))

(defun agent-shell-manager-exit-preview ()
  "Exit preview mode and restore original window configuration.
In cross-frame mode, just deactivates preview (no config to restore)."
  (setq agent-shell-manager-preview-active nil)
  (setq agent-shell-manager-preview-buffer nil)
  (remove-hook 'agent-shell-manager-after-refresh-hook #'agent-shell-manager--preview-after-refresh)
  (when (and agent-shell-manager-preview-highlight-overlay
             (overlay-buffer agent-shell-manager-preview-highlight-overlay))
    (delete-overlay agent-shell-manager-preview-highlight-overlay))
  (setq agent-shell-manager-preview-highlight-overlay nil)
  ;; Only restore window config for same-frame preview
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

(defun agent-shell-manager--goto-in-target-frame (buffer)
  "Display BUFFER in the target frame and switch focus to it.
Used when the manager is running in its own dedicated frame."
  (let ((target agent-shell-manager--target-frame))
    (raise-frame target)
    (select-frame-set-input-focus target)
    ;; Try to find the buffer already visible in the target frame
    (let ((buffer-window (get-buffer-window buffer target)))
      (if buffer-window
          (select-window buffer-window)
        ;; Try to reuse an existing agent-shell window in the target frame
        (let ((agent-window nil))
          (walk-windows
           (lambda (win)
             (when (and (not agent-window)
                        (with-current-buffer (window-buffer win)
                          (derived-mode-p 'agent-shell-mode)))
               (setq agent-window win)))
           nil target)
          (if agent-window
              (progn
                (set-window-buffer agent-window buffer)
                (select-window agent-window))
            ;; Last resort: use the selected window in the target frame
            (set-window-buffer (frame-selected-window target) buffer)
            (select-window (frame-selected-window target))))))))

(defun agent-shell-manager-goto ()
  "Go to the `agent-shell' buffer at point.
If called from the dedicated manager frame (created by
`agent-shell-manager-toggle-frame'), displays the buffer in the
target frame instead of the current one.
If the buffer belongs to a different perspective (and `persp-mode' is
active), switch to that perspective first — restoring its full window
configuration — then select the buffer's window within it.
If `agent-shell-manager-transient' is non-nil, hide the manager window."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (if (buffer-live-p buffer)
        (if (and (agent-shell-manager--in-manager-frame-p)
                 (agent-shell-manager--target-frame-live-p))
            ;; Cross-frame: display in the target frame
            (agent-shell-manager--goto-in-target-frame buffer)
          ;; Same-frame: original behavior
          (let ((switched-persp nil))
            ;; Perspective-aware: if the buffer lives in another perspective,
            ;; switch to it so its window layout is restored.
            (when (bound-and-true-p persp-mode)
              (let ((other-persp (persp-buffer-in-other-p buffer)))
                (when (and other-persp
                           (eq (car other-persp) (selected-frame)))
                  (agent-shell-manager--hide-window)
                  (persp-switch (cdr other-persp))
                  (setq switched-persp t))))
            ;; After a perspective switch, the buffer's window is likely
            ;; already visible in the restored layout — just select it.
            (let ((buffer-window (get-buffer-window buffer))
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
              (unless switched-persp
                (agent-shell-manager--hide-window)))))
      (user-error "Buffer no longer exists"))))

(defun agent-shell-manager-peek ()
  "Display the agent-shell buffer at point without switching focus.
In frame mode, shows the buffer in the target frame.  Otherwise,
shows it in another window.  Focus remains in the manager.
Scrolls the displayed buffer to the latest output."
  (interactive)
  (when-let* ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "Buffer no longer exists"))
    (let ((win
           (if (and (agent-shell-manager--in-manager-frame-p)
                    (agent-shell-manager--target-frame-live-p))
               ;; Find/reuse a window in the target frame
               (let ((target agent-shell-manager--target-frame))
                 (or (get-buffer-window buffer target)
                     (let ((found nil))
                       (walk-windows
                        (lambda (w)
                          (when (and (not found)
                                     (with-current-buffer (window-buffer w)
                                       (derived-mode-p 'agent-shell-mode)))
                            (setq found w)))
                        nil target)
                       found)
                     (frame-selected-window target)))
             ;; Same-frame: pop in another window
             (display-buffer buffer '((display-buffer-use-some-window)
                                      (inhibit-same-window . t))))))
      (when win
        (set-window-buffer win buffer)
        (with-selected-window win
          (goto-char (point-max))
          (recenter -3))))))

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
  "Create a new `agent-shell'.
When called from the dedicated manager frame, creates the shell
in the target frame and keeps focus in the manager."
  (interactive)
  (if (and (agent-shell-manager--in-manager-frame-p)
           (agent-shell-manager--target-frame-live-p))
      ;; Cross-frame: create in target, stay in manager
      (let ((mgr-frame (selected-frame))
            (target agent-shell-manager--target-frame))
        (select-frame-set-input-focus target)
        (agent-shell t)
        ;; Return focus to the manager frame
        (select-frame-set-input-focus mgr-frame)
        (agent-shell-manager-refresh))
    ;; Same-frame: original behavior
    (agent-shell t)
    (if agent-shell-manager-transient
        (agent-shell-manager--hide-window)
      (agent-shell-manager-refresh))))

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
;; Frame Helpers
;; ============================================================

(defun agent-shell-manager--in-manager-frame-p ()
  "Return non-nil if the current frame is the dedicated manager frame."
  (and agent-shell-manager--frame
       (frame-live-p agent-shell-manager--frame)
       (eq (selected-frame) agent-shell-manager--frame)))

(defun agent-shell-manager--target-frame-live-p ()
  "Return non-nil if a live target frame exists for cross-frame display."
  (and agent-shell-manager--target-frame
       (frame-live-p agent-shell-manager--target-frame)))

(defun agent-shell-manager--frame-cleanup (frame)
  "Clean up tracking variables when the manager FRAME is deleted."
  (when (eq frame agent-shell-manager--frame)
    (setq agent-shell-manager--frame nil)
    (setq agent-shell-manager--target-frame nil)))

;; ============================================================
;; Toggle
;; ============================================================

;;;###autoload
(defun agent-shell-manager-toggle-frame ()
  "Toggle the agent-shell manager in a dedicated frame.
Creates a separate frame showing the manager buffer.  Shells
selected with RET are displayed in the frame that was active
before this frame was created (the \"target\" frame).

If the manager frame already exists, raise it.  If called from
the manager frame itself, delete it."
  (interactive)
  (cond
   ;; Called from the manager frame — close it
   ((agent-shell-manager--in-manager-frame-p)
    (delete-frame agent-shell-manager--frame))
   ;; Manager frame exists elsewhere — raise it
   ((and agent-shell-manager--frame
         (frame-live-p agent-shell-manager--frame))
    (raise-frame agent-shell-manager--frame)
    (select-frame-set-input-focus agent-shell-manager--frame))
   ;; Create new manager frame
   (t
    (setq agent-shell-manager--target-frame (selected-frame))
    (let* ((buffer (get-buffer-create "*Agent-Shell Buffers*"))
           (frame (make-frame agent-shell-manager-frame-alist)))
      (setq agent-shell-manager--frame frame)
      (select-frame-set-input-focus frame)
      (set-window-buffer (frame-selected-window frame) buffer)
      (setq agent-shell-manager--global-buffer buffer)
      (with-current-buffer buffer
        (agent-shell-manager-mode)
        (agent-shell-manager-refresh))
      (add-hook 'delete-frame-functions #'agent-shell-manager--frame-cleanup)))))

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
        (select-window window)))))

;; ============================================================
;; Test Harness
;; ============================================================

;;;###autoload
(defun agent-shell-manager-spawn-test-env ()
  "Spawn test agent-shell buffers across multiple projects.
Creates 2 shells in each of 3 project directories with identifying
messages, then opens the buffer manager for testing preview mode,
project grouping, and status filtering."
  (interactive)
  (let ((projects '(("~/.dotfiles/" . "dotfiles")
                    ("~/roaming/claude/" . "claude")
                    ("~/roaming/projects/blog/" . "blog")))
        (count 0))
    (dolist (proj projects)
      (let ((default-directory (expand-file-name (car proj)))
            (label (cdr proj)))
        (dotimes (i 2)
          (setq count (1+ count))
          (agent-shell t)
          (goto-char (point-max))
          (insert (format "test shell %d — %s (%d)" count label (1+ i))))))
    (agent-shell-manager-toggle)
    (message "Spawned %d test shells across %d projects." count (length projects))))

(provide 'agent-shell-manager)

;;; agent-shell-manager.el ends here
