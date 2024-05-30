;;; sonic-pi-mode.el --- Send code to a running instance of Sonic Pi (>= 4.0) -*- lexical-binding: t -*-

;; Copyright (C) 2022 Sergio Gil

;; Author: Sergio Gil <sgilperez@gmail.com>
;; Version: 0.1
;; Keywords: languages
;; URL: https://github.com/porras/sonic-pi-mode
;; Package-Requires: ((emacs "25.1") (osc "0.3") (f "0.1"))

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
(require 'sonic-pi-connection)
;; taken from sonic-pi.el
(require 'sonic-pi-console)

(defcustom sonic-pi-daemon-command nil
  "Path to the Ruby daemon file inside the Sonic Pi install. For a standard install it should be '<sonic-pi-path>/app/server/ruby/bin/daemon.rb'. If you installed Sonic Pi via flatpak, it should be 'flatpak run --command=\"/app/app/server/ruby/bin/daemon.rb\" net.sonic_pi.SonicPi'."
  :type 'string :group 'sonic-pi)

(defvar-keymap sonic-pi-mode-prefix-map
  "c" #'sonic-pi-connect
  "r" #'sonic-pi-send-buffer
  "s" #'sonic-pi-stop
  "d" #'sonic-pi-disconnect)

(defvar sonic-pi-connection (make-instance sonic-pi--connection))

;;;###autoload
(define-minor-mode sonic-pi-mode
  "Minor mode to send code to a running instance of Sonic Pi"
  :lighter "π)))"
  :keymap (define-keymap "C-c ," sonic-pi-mode-prefix-map)
  (if sonic-pi-mode
      (message "Sonic Pi mode activated")
    (message "Sonic Pi mode deactivated")))

(defun sonic-pi-connect ()
  (interactive)
  (sonic-pi--connection--connect sonic-pi-connection))

(defun sonic-pi-disconnect ()
  (interactive)
  (sonic-pi--connection--disconnect sonic-pi-connection))

(defun sonic-pi-send-buffer ()
  (interactive)
  (sonic-pi--flash-mode-line)
  (sonic-pi--connection--send sonic-pi-connection "/run-code" (buffer-string)))

(defun sonic-pi-stop ()
  (interactive)
  (sonic-pi--connection--send sonic-pi-connection "/stop-all-jobs"))

(defun sonic-pi--flash-mode-line ()
  "Taken from https://www.emacswiki.org/emacs/AlarmBell#h5o-3"
  (invert-face 'mode-line)
  (run-with-timer 0.2 nil #'invert-face 'mode-line))

(provide 'sonic-pi-mode)

;;; sonic-pi-mode.el ends here
