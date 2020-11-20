;;; config-exwm.el --- Configure EXWM                -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2020  Carl Cederberg

;; Author: Carl Cederberg <c@rl.cederberg.info>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure

;;; Code:

(use-package exwm
  :init
;  (setq mouse-autoselect-window t
;	focus-follows-mouse t)
  (require 'exwm-config)
  (require 'exwm-floating)
  :config
  (exwm-config-default)
  (exwm-enable)
  (setenv "EXWM" nil)
  :custom
  (exwm-workspace-number 5)
  :hook
  (
   (exwm-manage-finish
    .
    (lambda ()
      (when (and exwm-title
		 (string= exwm-title "compilare"))
	(exwm-floating-toggle-floating)
	(exwm-floating-move 700 400)
	(exwm-input-toggle-keyboard exwm--id))
      (when (and exwm-class-name
       		 (string= exwm-class-name "Google-chrome"))
	(exwm-workspace-move-window 3)
       	(exwm-input-toggle-keyboard exwm--id))
      (when (and exwm-title
		 (string= exwm-class-name "Emacs"))
	(exwm-workspace-move-window 2)
	(exwm-input-toggle-keyboard exwm--id))
      (when (and exwm-class-name
		 (string= exwm-class-name "Slack"))
	(exwm-workspace-move-window 3))
      (when (and exwm-class-name
		 (string= exwm-class-name "Code"))
	(exwm-input-toggle-keyboard exwm--id))
      ))))

(defun exwm-next-ws()
  "Switch workspace."
  (interactive)
  (let ((curspace exwm-workspace-current-index))
    (exwm-workspace-switch
     (cond ((= curspace 4) 0)
           (t (1+ curspace))))))

(defun local-emacs()
  "."
  (interactive)
  (start-process-shell-command "lemacs" nil "emacs"))

(defun remote-emacs()
  "."
  (interactive)
  (start-process-shell-command "remacs" nil
 			       "ssh -X host emacs -c"))

(exwm-input-set-key [s-f2] (lambda() (interactive)
                             (start-process-shell-command "pwr" nil "sudo sispmctl -t 1")))
(exwm-input-set-key [s-f3] (lambda() (interactive)
                             (start-process-shell-command "pwr" nil "sudo sispmctl -t 3")))
(exwm-input-set-key [s-tab] #'exwm-next-ws)

(exwm-input-set-key [s-t] (lambda() (interactive) (message "hej")
			    (exwm-input-toggle-keyboard)))

(exwm-input-set-key (kbd "<XF86Favorites>") #'remote-emacs)

(exwm-input-set-key (kbd "<XF86Mail>")
                    (lambda () (interactive)
                      (exwm-workspace-switch 4)
		      (notmuch)))

(push 'XF86AudioMute exwm-input-prefix-keys)
(push 'XF86Mail exwm-input-prefix-keys)

(setq server-name "exwm")

;(run-at-time 5 nil 'start-process-shell-command "chrome" nil "google-chrome")
;(run-at-time 10 nil 'remote-emacs)
(run-at-time 15 nil 'start-process-shell-command "nd" nil "/usr/libexec/notification-daemon")

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'config-exwm)
;;; config-exwm.el ends here
