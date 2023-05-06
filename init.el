;; No decorations
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)

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

;; Yes
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(defun ds/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (evil-mode 1)
  :hook (evil-mode . ds/evil-hook)
  :after counsel
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'ivy-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>/") 'counsel-grep-or-swiper)
  (evil-define-key 'normal 'global (kbd "<leader>hp") 'git-gutter:popup-hunk))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package counsel
  :diminish ivy-mode
  :bind (("M-x" . counsel-M-x))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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
