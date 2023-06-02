;; No decorations
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)
(hl-line-mode t)
(icomplete-mode t)
(icomplete-vertical-mode t)

;; No annoying undo and backup files everywhere
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq backup-by-copying t)
(setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "tmp/undo" user-emacs-directory))))

;; Enable when lockfiles become annoying
;; (setq create-lockfiles nil

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(column-number-mode t)
(show-paren-mode t)

;; Pretty font
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 150)

(load-theme 'gruber-darker t)

;; Probably something good
(setq frame-resize-pixelwise t)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Packages
(use-package diminish)

(use-package gruber-darker-theme)

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode))

(use-package git-gutter
  :diminish
  :config (global-git-gutter-mode t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)
