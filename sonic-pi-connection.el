(require 'osc)
(require 'eieio)

(defclass sonic-pi--connection ()
  ((daemon :type process)
   (api-client :type process)
   (keep-alive-client :type process)
   (keep-alive-timer :type timer)
   (log-server :type process)
   (token :type number)
   (connected? :initform nil :type boolean)))

(cl-defmethod sonic-pi--connection--connect ((c sonic-pi--connection))
  (sonic-pi-messages-buffer-init)
  (message "Starting Sonic Pi daemon...")
  (with-slots (daemon api-client keep-alive-client keep-alive-timer log-server token connected?) c
    (when (not connected?)
      (setf daemon (start-process-shell-command "sonic-pi-daemon" "*sonic-pi-daemon-output*" sonic-pi-daemon-command))
      (set-process-filter daemon (lambda (_ output)
                                   (cl-destructuring-bind (reported-daemon-keep-alive reported-gui-listen-to-server reported-gui-send-to-server reported-scsynth reported-osc-cues reported-tau-api reported-tau-phx reported-token)
                                       (seq-map #'string-to-number (split-string output " "))
                                     (setf token reported-token)
                                     (setf keep-alive-client (osc-make-client 'local reported-daemon-keep-alive))
                                     (setf keep-alive-timer (run-with-timer 30 30 (lambda ()
                                                                                    (osc-send-message keep-alive-client "/daemon/keep-alive" token))))
                                     (setf api-client (osc-make-client 'local reported-gui-send-to-server))
                                     (setf log-server (osc-make-server 'local reported-gui-listen-to-server (lambda (path &rest args) (sonic-pi-log-message path args))))
                                     (setf connected? t))
                                   (set-process-filter daemon nil))) ; don't need to get more lines
      (while (not connected?) (accept-process-output daemon))) ; wait until we connect
    (message "Sonic Pi connected")))

;; TODO: Bring this back for all processes
;; (set-process-query-on-exit-flag (oref c :api-client) nil)) ; let emacs kill it on exit without confirmation

(cl-defmethod sonic-pi--connection--send ((c sonic-pi--connection) command &rest args)
  (with-slots (connected? api-client token) c
    (if connected?
        (apply #'osc-send-message api-client command token args)
      (message "Not connected! Connect to Sonic Pi first"))))

(cl-defmethod sonic-pi--connection--disconnect ((c sonic-pi--connection))
  (with-slots (keep-alive-timer api-client keep-alive-client log-server daemon connected?) c
    (when connected?
      (cancel-timer keep-alive-timer)
      (delete-process api-client)
      (delete-process keep-alive-client)
      (delete-process log-server)
      (delete-process daemon)
      (sonic-pi-messages-buffer-cleanup)
      (setf connected? nil)))
  (message "Sonic Pi disconnected"))

(provide 'sonic-pi-connection)
