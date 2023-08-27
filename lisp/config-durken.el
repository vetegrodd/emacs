;;; config-durken.el --- host dependant config     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Carl Cederberg

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Theme?

;;; Code:

(scroll-bar-mode 0)
(menu-bar-mode 0)

(use-package multi-term)

(use-package anti-zenburn-theme)

(custom-theme-set-faces
 'anti-zenburn
 '(cursor ((t (:background "DarkRed" :foreground "#232333"))))
 '(cvs-handled ((t (:foreground "pink4"))))
 '(cvs-need-action ((t (:foreground "orange3"))))
 '(cvs-unknown ((t (:foreground "red3"))))
 '(error ((t (:foreground "Red3" :weight normal))))
 '(font-lock-comment-face ((t (:foreground "#735673"))))
 '(gnus-group-mail-1 ((t (:foreground "#0f2050" :weight bold))))
 '(gnus-group-news-1 ((t (:foreground "#0f2050" :weight bold))))
 '(gnus-group-news-1-empty ((t (:foreground "#5f6a8a"))))
 '(gnus-summary-cancelled ((t (:foreground "#205070" :inverse-video t))))
 '(helm-M-x-key ((t (:foreground "LightYellow" :underline t))))
 '(helm-M-x-key ((t (:foreground "mediumblue" :underline t))))
 '(helm-ff-dotted-directory ((t (:background "LightGray" :foreground "black"))))
 '(org-block ((t (:inherit shadow :background "#adadad"))))
 '(org-level-7 ((t (:foreground "#4d7373")))))

(load-theme 'anti-zenburn t)

(purpose-load-window-layout "develop")

;; https://gitlab.com/ideasman42/emacs-undo-fu-session

(provide 'config-durken)
;;; config-durken.el ends here
