;;; config-nx-ccederberg.el --- Host specific settings   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Carl Cederberg

;; Author: Carl Cederberg <carl.cederberg@gmail.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stufff

;;; Code:

(appt-activate t)

;; Edit in emacs
(use-package atomic-chrome)
(ignore-errors (atomic-chrome-start-server))

(use-package celestial-mode-line
  :custom
  (calendar-longitude 15.569)
  (calendar-latitude 58.394)
  (calendar-location-name "Mjärdevi")
  :init
  (require 'cl-lib)
  (setq global-mode-string
	'("" celestial-mode-line-string " " display-time-string))
  (celestial-mode-line-start-timer))

;; Read mail
(require 'config-notmuch)

(require 'time)
(use-package time
  :custom
  (display-time-24hr-format t)
  :config
  (display-time-mode))


(use-package desktop-environment
  :config
  (desktop-environment-mode))

(load-theme 'tango t)

(provide 'config-nas)
;;; config-nas.el ends here
