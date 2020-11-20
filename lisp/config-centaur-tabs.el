;;; config-centaur-tabs.el --- Configure centaur tabs  -*- lexical-binding: t; -*-

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

;; Setup

;;; Code:


(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)

  (centaur-tabs-enable-buffer-reordering)

  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (and (string-prefix-p "magit" name)
	    (not (file-name-extension name)))
       )))

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((derived-mode-p 'emacs-lisp-mode) "Config")
      ;; ((derived-mode-p 'prog-mode) "Editing")
      ((derived-mode-p 'dired-mode) "Dired")
      ((string-prefix-p "*compilation" (buffer-name)) "Build")
      ((memq major-mode '(helpful-mode
			  help-mode)) "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode)) "OrgMode")
      ((derived-mode-p 'term-mode) "Terminal")
      ((or (string-prefix-p "*" (buffer-name))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      ))) "Emacs")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  ("C-S-<iso-lefttab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-pgrojectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  :custom
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-adjust-buffer-order 'left)
  (centaur-tabs-cycle-scope 'tabs))

(provide 'config-centaur-tabs)
;;; config-centaur-tabs.el ends here
