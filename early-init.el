(setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

(defvar darius/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

(defvar darius/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar darius/vc-handled-backends vc-handled-backends)
(setq vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold darius/gc-cons-threshold
                  file-name-handler-alist darius/file-name-handler-alist
                  vc-handled-backends darius/vc-handled-backends)))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq server-client-instructions nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(blink-cursor-mode 0)
(setq frame-resize-pixelwise t)
(setq use-dialog-box nil)
