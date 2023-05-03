;; No decorations
(setq inhibit-startup-message t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; No annoying undo and backup files everywhere
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq backup-by-copying t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Enable when lockfiles become annoying
;; (setq create-lockfiles nil

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(column-number-mode t)
(show-paren-mode t)

;; Pretty font
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 150)

;; Colorscheme
(load-theme 'base16-default-dark t)

;; Probably something good
(setq frame-resize-pixelwise t)

;; Yes
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package counsel
  :diminish ivy-mode
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)
	 ("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("b19d77aca9b695b1b6219e949917160cecfa009e9901649c6b98b5d919492fee" "8de91e43d21b9d5cb79a146b267d29d3e377ed73888324912c7f6e514a76e5d7" "0430ba14a3cc147f924a5e0d24c4fd21b377c6ebe2924855969023430705500c" "1cfbec19edafb831c7729be2f6454ec019c21b9a54b39b3bb5ec276a6b21d484" "f700bc979515153bef7a52ca46a62c0aa519950cc06d539df4f3d38828944a2c" "882001d4237dfe3e84ceeeee697a0da77719c9ab4e85eca7bb43541815d458fc" "308fc0c8cee43c5fccf3efa360c9cdf7d6bbbebc5c2f76850f1b1c8ac8fbaca0" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" "eff66f28da7d1590650ebcc602fc1752fec11fe03ad0f01cc9696c31d7f27e7b" "dea106ab256a8017a325f51f01b1131915989fa25db48eb831ffb18dac8ecd39" "47610f9d6af7e30fbfb52fffe6de4c7de299792a7f0d09192a5b2b593c18931b" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "3a9f65e0004068ecf4cf31f4e68ba49af56993c20258f3a49e06638c825fbfb6" "75e027e3ab2892c5c1f152e3d9fae03718f75bee50d259040e56e7e7672a4872" "e2337309361eef29e91656c5e511a6cb8c54ce26231e11424a8873ea88d9093e" "738c4838957c1884dfacbb6f4f783c54e87c4a6b31c336d6279fc1c2b2ee56c5" default))
 '(package-selected-packages '(ivy-rich counsel diminish base16-theme undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
