;; -*- lexical-binding: t; -*-
;; This config requires emacs >= 31 for transpose-window-layout

(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(setq tab-bar-show 1)
(when (display-graphic-p)
  (progn
    (tool-bar-mode 0)
    (scroll-bar-mode 0)))
(setq visible-cursor nil) ; no blinking cursor in terminal
(setq use-short-answers t)

(unless (display-graphic-p)
  (xterm-mouse-mode))

;; make transparency work in the terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

(load-theme 'modus-vivendi t)

(setq completions-format 'one-column
      completion-show-help nil
      completion-auto-help t
      completion-auto-select 'second-tab
      completions-detailed t
      completion-show-inline-help nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-display-summary nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Code" (or (derived-mode . prog-mode) (mode . ess-mode)
                           (mode . compilation-mode)))
               ("LaTeX" (mode . latex-mode))
               ("Dired" (mode . dired-mode))
               ("Org" (mode . org-mode))
               ("Pdf" (mode . pdf-view-mode))
               ("Help" (or (mode . help-mode) (mode . Man-mode)))
               ("Git" (name . "^magit"))
               ("Misc" (name . "^\\**.*\\*$"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-auto-mode t)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(column-number-mode 1)
(show-paren-mode 1)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq backup-by-copying t)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-M-o") 'window-layout-transpose)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-D") 'duplicate-dwim)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(setq suggest-key-bindings nil
      echo-keystrokes 0.01)

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

(setq comint-prompt-read-only t)

(setq sentence-end-double-space nil
      sentence-end "[.\",;!?*:'] ")

(setq isearch-wrap-pause 'no-ding
      isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      search-whitespace-regexp ".*?")

(setq indent-tabs-mode nil)

(electric-pair-mode t)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)
