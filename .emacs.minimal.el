(menu-bar-mode 0)
(setq tab-bar-show 1)

(icomplete-vertical-mode)
(fido-mode)

(display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq backup-by-copying t)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-z") 'grep-find)

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
