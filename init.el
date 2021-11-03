(setq inhibit-startup-message t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)

;; time in modeline
; (setq display-time-24hr-format 1)
; (setq display-time-default-load-average nil)
; (display-time-mode 1)

;; macOS
(exec-path-from-shell-initialize)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(column-number-mode 1)
(show-paren-mode 1)

(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 280)

;; cool themes: doom-miramare, doom-one, doom-nord, doom-zenburn, gruber-darker, doom-gruvbox
(load-theme 'doom-miramare t)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c C-e") 'compile-goto-error)

(setq frame-resize-pixelwise t)

;; PACKAGES

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; HASKELL
(use-package haskell-mode) ; is this neccesary, idk
; (add-hook 'haskell-mode-hook 'haskell-indent-mode) ;; broken for some reason
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode) ;; remember to install that package
(setq hindent-reformat-buffer-on-save t)

;; EVIL MODE SUPREMACY YAAAAAAS
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

(use-package undo-tree
  :after evil ; probably needs this, idk
  :diminish
  :config
    (global-undo-tree-mode)
    (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after magit
  :config
  (evil-collection-init))

(use-package magit
  :diminish auto-revert-mode
  :after evil
  )

(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode 1))

;; collection of themes
(use-package doom-themes)

;; use smex for ido style M-x
;; (require 'ido)
;; (ido-mode t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ;("C-k" . ivy-previous-line)
         ;("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
	 ;("C-d" . ivy-reverse-i-search-kill)
	 )
  :custom
  (ivy-mode t))

; M-x man is slow with ivy on macOS
(add-to-list 'ivy-completing-read-handlers-alist '(man . completing-read-default))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-M-b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  :custom
  (counsel-mode t))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; kind of ugly ngl
; (use-package rainbow-delimiters
;   :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  ;; uncomment for longer delay before the help shows up
  ;:config
  ;(setq which-key-idle-delay 0.3))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "03e26cd42c3225e6376d7808c946f7bed6382d795618a82c8f3838cd2097a9cc" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(hindent interactive-haskell-mode haskell-mode git-gutter evil-collection undo-tree evil helpful gruber-darker-theme ivy-rich swiper diminish markdown-mode which-key rainbow-delimiters raninbow-delimiters doom-themes exec-path-from-shell latex-preview-pane magit use-package ivy))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-variable-name-face ((t (:foreground "#87c095" :slant normal)))))
