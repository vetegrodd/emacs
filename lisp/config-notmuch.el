;;; config-notmuch.el --- Configure notmuch          -*- lexical-binding: t; -*-

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

;; Stuff

;;; Code:

(use-package notmuch
  :custom
  (notmuch-show-all-tags-list t)
  (notmuch-fcc-dirs "\"nn/Sent Items\"")
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unbox" :query "tag:unread and tag:inbox" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "jira" :query "tag:unread and tag:jira" :key "j" :search-type tree)
     (:name "katt" :query "tag:unread and tag:katt" :key "k")
     (:name "devhub" :query "tag:unread and tag:devhub" :key "d")
     (:name "bcm" :query "tag:unread and tag:bcm")
     (:name "vewd" :query "tag:unread and tag:vewd" :key "v" :search-type tree)
     ))
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "")
  (smtpmail-smtp-server "smtp.office365.com")
  (smtpmail-smtp-service 587)
  (smtpmail-smtp-user "c@rl.cederberg.info")
  (smtpmail-stream-type 'starttls)
  (user-full-name "Carl Cederberg")
  (user-mail-address "c@rl.cederberg.info")
  (shr-use-colors t)
  )

(require 'time)
(defun my-notmuch-unread ()
  "Return t if unread important messages."
  (save-window-excursion
    (let* ((default-directory "~/")
           (unread (replace-regexp-in-string
                    "\n\\'" ""
                    (shell-command-to-string
		     "/usr/bin/notmuch count tag:unread and tag:notify"))))
      (not (string= unread "0")))))

(setq display-time-mail-function 'my-notmuch-unread)

(setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))

(provide 'config-notmuch)
;;; config-notmuch.el ends here
