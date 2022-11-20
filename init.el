;;; init.el --- ini file                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2020  Carl Cederberg

;; Author: Carl Cederberg <c@rl.cederberg.info>
;; Keywords: internal

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

;;

;;; Code:

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(tool-bar-mode -1)

(use-package async)

(use-package paradox
  :custom
  (paradox-execute-asynchronously t)
  (paradox-spinner-type 'moon)
  (paradox-github-token t))

(add-to-list 'load-path
	     (concat (expand-file-name user-emacs-directory)
		     (convert-standard-filename "lisp/")))

;;; fira
(use-package fira-code-mode
  :config (global-fira-code-mode))

(set-face-attribute 'default nil
                     :family "Fira Code Retina"
                     :height 80
                     :weight 'normal
                     :width 'normal
   		    :slant 'normal)
(set-frame-font "Fira Code Retina" nil t)

;; EXWM
(if (string= (getenv "EXWM") "true")
    (require 'config-exwm))

;; Host config
(let* ((host-config (concat "config-" (system-name)))
       (file-name (concat
		   user-emacs-directory
		   (convert-standard-filename "lisp/")
		   host-config
		   ".el")))
  (if (file-exists-p file-name)
      (require (intern host-config))))

(add-hook 'c-mode-common-hook
          #'(lambda ()
             (setq indent-tabs-mode nil)
             (define-key c-mode-base-map "\C-m" 'c-context-line-break)
             (define-key c-mode-base-map [M-up] 'c-beginning-of-statement)
             (define-key c-mode-base-map [M-down] 'c-end-of-statement)
             (auto-insert-mode t)
             (c-toggle-auto-hungry-state t)
             (setq c-electric-pound-behavior '(alignleft))))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(with-eval-after-load 'projectile
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".ccls")
                projectile-project-root-files-top-down-recurring)))

;; programming
(use-package flycheck
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :hook
  ((elisp-mode . flycheck-mode)))

(use-package yasnippet
  :config
  (yas-global-mode +1)
  )

(require 'config-lsp)

;;; company elisp
(use-package company
  :hook
  ((emacs-lisp-mode . company-mode)))

;;; git
(use-package magit
  :bind
  (("C-x v t" . magit-status)))

(use-package git-messenger
  :bind
  (("C-x v p" . git-messenger:popup-message)))

(use-package git-timemachine)

(use-package git-walktree)

;;; eshell
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;;; term
(use-package bifocal
  :hook (term-mode . bifocal-mode))

(use-package vterm)

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package clean-aindent-mode
  :hook
  (prog-mode . clean-aindent-mode))

(use-package anzu
  :config
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

(auto-insert-mode)

;;; selectrum
(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;;; auto-compile
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

;; Name compilation buffer after the buffer name

(setq compilation-buffer-name-function
      (lambda (mode) (concat "*" (downcase mode) ": " default-directory "*")))

(customize-save-variable 'compilation-scroll-output t)
(customize-save-variable 'ring-bell-function 'ignore)

(use-package bury-successful-compilation
  :ensure t
  :bind ("C-c C-m" . recompile))

;; Bury the compilation buffer when compilation is finished and successful.
;; (add-to-list 'compilation-finish-functions
;;              (lambda (buffer msg)
;;                (when
;;                    (bury-buffer buffer)
;;                  (replace-buffer-in-windows buffer))))


(use-package column-enforce-mode
  :hook (prog-mode . column-enforce-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

;;; web-mode
(use-package web-mode
  :defer t
  :bind (("C-c C-v" . browse-url-of-buffer)
         ("C-c w t" . web-mode-element-wrap))
  :config
  (add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package terminal-toggle
  :custom
  (terminal-toggle--term-shell "/usr/bin/fish")
  (terminal-toggle--term-title "fishterm"))

(use-package ag)

(use-package lolcat)

(use-package howdoyou)

(use-package pyvenv)

;; Pull from PRIMARY (same as middle mouse click)
(defun paste-primary-selection ()
  (interactive)
  (insert
   (gui-get-selection 'PRIMARY)))
(global-set-key (kbd "s-<insert>") 'paste-primary-selection)

(windmove-default-keybindings)

(require 'server)
(unless (server-running-p) (server-start))

(require 'config-purpose)

(use-package demap)

(require 'gyp-mode)

;;; init.el ends here
