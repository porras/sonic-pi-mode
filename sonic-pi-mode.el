;;; sonic-pi-mode.el --- Send code to a running instance of Sonic Pi (>= 4.0) -*- lexical-binding: t -*-

;; Copyright (C) 2022 Sergio Gil

;; Author: Sergio Gil <sgilperez@gmail.com>
;; Version: 0.1
;; Keywords: languages
;; URL: https://github.com/porras/sonic-pi-mode
;; Package-Requires: ((emacs "25.1") (osc "0.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Send code to a running instance of Sonic Pi (>= 4.0).
;;
;; See documentation at https://github.com/porras/sonic-pi-mode

;;; Code:
(require 'osc)
(require 'eieio)
; taken from sonic-pi.el
(require 'sonic-pi-console)

(defvar sonic-pi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'sonic-pi-connect)
    (define-key map (kbd "C-c r") 'sonic-pi-send-buffer)
    (define-key map (kbd "C-c s") 'sonic-pi-stop)
    (define-key map (kbd "C-c d") 'sonic-pi-disconnect)
    map))

(defvar sonic-pi-daemon-path "flatpak run --command=\"/app/app/server/ruby/bin/daemon.rb\" net.sonic_pi.SonicPi")
;; (defvar sonic-pi-daemon-path "~/Code/sonic-pi/app/server/ruby/bin/daemon.rb")
(defvar sonic-pi-connection nil)

;;;###autoload
(define-minor-mode sonic-pi-mode
  "Minor mode to send code to a running instance of Sonic Pi"
  :lighter "π)))"
  :keymap sonic-pi-mode-map
  (if sonic-pi-mode
      (message "Sonic Pi mode activated")
    (message "Sonic Pi mode deactivated")))

(defun sonic-pi-connect ()
  (interactive)
  (if sonic-pi-connection
      (message "Already connected")
    (setq sonic-pi-connection (sonic-pi--make-connection))))

(defun sonic-pi-disconnect ()
  (interactive)
  (if sonic-pi-connection
      (progn (sonic-pi--connection--close sonic-pi-connection)
             (setq sonic-pi-connection nil)
             (message "Disconnected"))
    (message "Not connected")))

(defun sonic-pi-send-buffer ()
  (interactive)
  (if sonic-pi-connection
      (progn
        (sonic-pi--flash-mode-line)
        (sonic-pi--connection--send sonic-pi-connection "/run-code" (buffer-string))
        (message (format "Sent %i characters of 🎶" (length (buffer-string)))))
    (message "Not connected to Sonic Pi. Connect first (sonic-pi-connect or C-c c)")))

(defun sonic-pi-stop ()
  (interactive)
  (if sonic-pi-connection
      (progn
        (sonic-pi--connection--send sonic-pi-connection "/stop-all-jobs")
        (message "Stop"))
    (message "Not connected to Sonic Pi. Connect first (sonic-pi-connect or C-c c)")))

(defun sonic-pi--flash-mode-line ()
  "Taken from https://www.emacswiki.org/emacs/AlarmBell#h5o-3"
  (invert-face 'mode-line)
  (run-with-timer 0.2 nil #'invert-face 'mode-line))

(defclass sonic-pi--connection ()
  ((daemon :initarg :daemon :type process)
   (api-client :initarg :api-client :type process)
   (keep-alive-client :initarg :keep-alive-client :type process)
   (keep-alive-timer :initarg :keep-alive-timer :type timer)
   (log-server :initarg :log-server :type process)
   (token :initarg :token :type number)))

;; TODO: This function is a mess, almost everything happens here! Refactor
(defun sonic-pi--make-connection ()
  (sonic-pi-messages-buffer-init)
  (message "Starting Sonic Pi daemon...")
  (let ((c (sonic-pi--connection))
        (daemon (start-process-shell-command "sonic-pi-daemon" "sonic-pi-daemon-output" sonic-pi-daemon-path)))
    (set-process-filter daemon (lambda (_ output)
                                 (cl-destructuring-bind (daemon-keep-alive gui-listen-to-server gui-send-to-server scsynth osc-cues tau-api tau-phx token) ; we don't need them all but better name them
                                     (seq-map #'string-to-number (split-string output " "))
                                   (oset c :token token)
                                   (oset c :keep-alive-client (osc-make-client 'local daemon-keep-alive))
                                   (oset c :keep-alive-timer (run-with-timer 30 30 (lambda ()
                                                                                     (osc-send-message (oref c :keep-alive-client) "/daemon/keep-alive" token))))
                                   (oset c :api-client (osc-make-client 'local gui-send-to-server))
                                   (oset c :log-server (osc-make-server 'local gui-listen-to-server (lambda (path &rest args) (sonic-pi-log-message path args)))))
                                 (set-process-filter daemon nil)
                                 (oset c :daemon daemon)))
    (while (not (slot-boundp c :daemon)) (accept-process-output daemon)) ; wait until we get the output
    (message "Sonic Pi daemon started")
    c))

;; TODO: Bring this back for all processes
;; (set-process-query-on-exit-flag (oref c :api-client) nil)) ; let emacs kill it on exit without confirmation

(cl-defmethod sonic-pi--connection--send ((c sonic-pi--connection) command &rest args)
  (apply #'osc-send-message (oref c :api-client) command (oref c :token) args))

(cl-defmethod sonic-pi--connection--close ((c sonic-pi--connection))
  (cancel-timer (oref c :keep-alive-timer))
  (delete-process (oref c :api-client))
  (delete-process (oref c :keep-alive-client))
  (delete-process (oref c :log-server))
  (delete-process (oref c :daemon))
  (sonic-pi-messages-buffer-cleanup))

(provide 'sonic-pi-mode)
