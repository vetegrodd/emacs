;;; config-purpose.el --- purpose                    -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Carl Cederberg

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

;; purpose

;;; Code:

;; (use-package perspective
;;   :config
;;   (persp-mode))
;;    :hook
;;    (kill-emacs . #'persp-state-save))

(use-package persp-projectile)

(use-package window-purpose
  :config
  (purpose-mode)
  (add-to-list 'purpose-user-mode-purposes '(term-mode . terminal))
  (add-to-list 'purpose-user-mode-purposes '(vterm-mode . terminal))
  (add-to-list 'purpose-user-mode-purposes '(prog-mode . coding))
  (add-to-list 'purpose-user-mode-purposes '(compilation-mode . messages))
  (add-to-list 'purpose-user-mode-purposes '(logview-mode . log))
  (add-to-list 'purpose-user-mode-purposes '(makefile-mode . make))

  (purpose-compile-user-configuration)

  (require 'window-purpose-x)
  (purpose-x-kill-setup)
  (purpose-x-magit-single-on)
  (purpose-x-persp-setup)
  )


;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits


(provide 'config-purpose)
;;; config-purpose.el ends here
