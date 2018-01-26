(defun kill-buffer-when-exit ()
  "Close assotiated buffer when a process exited"
  (let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
    (when current-process
      (set-process-sentinel current-process
			    (lambda (watch-process change-state)
			      (when (string-match "\\(finished\\|exited\\)" change-state)
				(kill-buffer (process-buffer watch-process))))))))


(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win) (delete-window win)))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(provide 'z-edit-ext)
