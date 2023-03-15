;; No decorations
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; No annoying undo and backup files everywhere
(setq auto-save-file-name-transforms `(("." "~/.emacs.d/saves/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/saves/")))
(setq backup-by-copying t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(column-number-mode t)
(show-paren-mode t)

;; Pretty font
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 150)

;; Probably something good
(setq frame-resize-pixelwise t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ido

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Packages
(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode))

(use-package magit
  :diminish auto-revert mode)

(use-package git-gutter
  :diminish
  :config (global-git-gutter-mode t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("738c4838957c1884dfacbb6f4f783c54e87c4a6b31c336d6279fc1c2b2ee56c5" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(package-selected-packages
   '(danneskjold-theme gruber-darker-theme which-key git-gutter magit undo-tree use-package)))
