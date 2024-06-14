;;; sonic-pi-connection.el --- Sonic Pi connection -*- lexical-binding: t -*-

;;; Commentary:

;; This file implements the details of starting the Sonic Pi daemon and connecting to it (as a class and a bunch of
;; methods).

;;; Code:

(require 'osc)
(require 'eieio)

(eval-when-compile (defvar sonic-pi-daemon-command)) ; defined in sonic-pi-mode.el

(defclass sonic-pi--connection ()
  ((daemon :type process)
   (api-client :type process)
   (keep-alive-client :type process)
   (keep-alive-timer :type timer)
   (log-server :type process)
   (token :type number)
   (connected? :initform nil :type boolean :accessor sonic-pi--connection--connected?)))

(cl-defmethod sonic-pi--connection--connect ((c sonic-pi--connection))
  "Start the Sonic Pi daemon and connects to it.
It is a relatively involved process: 1) start the daemon, 2) wait
until it prints a list of ports, 3) establish a series of clients
and servers on the reported ports.  All those clients and servers
are stored on the corresponding slots of the connection instance
`C'."
  (sonic-pi-messages-buffer-init)
  (message "Starting Sonic Pi daemon...")
  (with-slots (daemon api-client keep-alive-client keep-alive-timer log-server token connected?) c
    (when (not connected?)
      (setf daemon (start-process-shell-command "sonic-pi-daemon" "*sonic-pi-daemon-output*" sonic-pi-daemon-command))
      (set-process-filter daemon
                          (lambda (_ output)
                            (cl-destructuring-bind
                                (reported-daemon-keep-alive
                                 reported-gui-listen-to-server
                                 reported-gui-send-to-server
                                 _ ; reported-scsynth (unused)
                                 _ ; reported-osc-cues (unused)
                                 _ ; reported-tau-api (unused)
                                 _ ; reported-tau-phx (unused)
                                 reported-token)
                                (seq-map #'string-to-number (split-string output " "))
                              (setf token reported-token)
                              (setf keep-alive-client
                                    (osc-make-client 'local reported-daemon-keep-alive))
                              (setf keep-alive-timer
                                    (run-with-timer 30 30
                                                    (lambda ()
                                                      (osc-send-message keep-alive-client "/daemon/keep-alive" token))))
                              (setf api-client (osc-make-client 'local reported-gui-send-to-server))
                              (setf log-server (osc-make-server 'local reported-gui-listen-to-server
                                                                (lambda
                                                                  (path &rest args) (sonic-pi-log-message path args))))
                              (setf connected? t))
                            (set-process-filter daemon nil))) ; don't need to get more lines
      (while (not connected?) (accept-process-output daemon))) ; wait until we connect
    (message "Sonic Pi connected")))

;; TODO: Bring this back for all processes
;; (set-process-query-on-exit-flag (oref c :api-client) nil)) ; let emacs kill it on exit without confirmation

(cl-defmethod sonic-pi--connection--send ((c sonic-pi--connection) command &rest args)
  "Sends the command `COMMAND' to the Sonic Pi daemon.
It automatically uses the token stored in the connection `C'."
  (with-slots (connected? api-client token) c
    (if connected?
        (apply #'osc-send-message api-client command token args)
      (message "Not connected! Connect to Sonic Pi first"))))

(cl-defmethod sonic-pi--connection--disconnect ((c sonic-pi--connection))
  "Disconnects from the Sonic Pi daemon and stops it.
It uses the clients and servers stored in the connection `C' to
do so."
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

;;; sonic-pi-connection.el ends here
