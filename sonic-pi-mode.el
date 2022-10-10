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

(defvar sonic-pi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'sonic-pi-send-buffer)
    (define-key map (kbd "C-c s") 'sonic-pi-stop)
    (define-key map (kbd "C-c d") 'sonic-pi-disconnect)
    map))

(defvar sonic-pi-log-file "~/.sonic-pi/log/daemon.log")
(defvar sonic-pi-connection nil)

;;;###autoload
(define-minor-mode sonic-pi-mode
  "Minor mode to send code to a running instance of Sonic Pi"
  :lighter "π"
  :keymap sonic-pi-mode-map
  (if sonic-pi-mode
      (message "Sonic Pi mode activated")
    (message "Sonic Pi mode deactivated")))

(defun sonic-pi-send-buffer ()
  (interactive)
  (sonic-pi--ensure-connection)
  (sonic-pi--flash-mode-line)
  (sonic-pi--connection--send sonic-pi-connection "/run-code" (buffer-string))
  (message (format "Sent %i characters of 🎶" (length (buffer-string)))))

(defun sonic-pi-stop ()
  (interactive)
  (sonic-pi--ensure-connection)
  (sonic-pi--connection--send sonic-pi-connection "/stop-all-jobs")
  (message "Stop"))

(defun sonic-pi-disconnect ()
  (interactive)
  (if sonic-pi-connection
      (progn (sonic-pi--connection--close sonic-pi-connection)
             (setq sonic-pi-connection nil)
             (message "Disconnected"))
    (message "Not connected")))

(defun sonic-pi--flash-mode-line ()
  "Taken from https://www.emacswiki.org/emacs/AlarmBell#h5o-3"
  (invert-face 'mode-line)
  (run-with-timer 0.2 nil #'invert-face 'mode-line))

(defun sonic-pi--ensure-connection ()
  (unless sonic-pi-connection
    (setq sonic-pi-connection (sonic-pi--connect))))

(defun sonic-pi--connect ()
  (cl-destructuring-bind (port token) (sonic-pi--get-config)
    (sonic-pi--connection :port port :token token)))

(defun sonic-pi--get-config ()
  (with-temp-buffer
    (insert-file-contents sonic-pi-log-file)
    (goto-char (point-max))
    (search-backward "server_port")
    (re-search-forward "[0-9]")
    (setq-local port (thing-at-point 'number))
    (goto-char (point-max))
    (search-backward "Token:")
    (re-search-forward "-?[0-9]")
    (setq-local token (thing-at-point 'number))
    (list port token)))

(defclass sonic-pi--connection ()
  ((port :initarg :port :type number)
   (token :initarg :token :type number)
   (osc-client :initarg :osc-client :type process)))

(cl-defmethod initialize-instance :after ((c sonic-pi--connection) &rest _)
  (oset c :osc-client (osc-make-client 'local (oref c :port)))
  (set-process-query-on-exit-flag (oref c :osc-client) nil)) ; let emacs kill it on exit without confirmation

(cl-defmethod sonic-pi--connection--send ((c sonic-pi--connection) command &rest args)
  (apply #'osc-send-message (oref c :osc-client) command (oref c :token) args))

(cl-defmethod sonic-pi--connection--close ((c sonic-pi--connection))
  (delete-process (oref c :osc-client)))

(provide 'sonic-pi-mode)
