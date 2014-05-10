(prelude-require-package 'multiple-cursors)

; no scroll bars
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

; be quiet
(setq ring-bell-function 'ignore)

;;; ISSUE fish Shell separates PATH with whitespace instead of :
;;; https://github.com/purcell/exec-path-from-shell/issues/9
(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize)
  (when (equal (file-name-nondirectory (getenv "SHELL")) "fish")
    (setq exec-path (split-string (car exec-path) " "))
    (let ((fixed-path (mapconcat 'identity (split-string (getenv "PATH") " ") ":")))
         (setenv "PATH" fixed-path)
         (setq eshell-path-env fixed-path))))

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; mac friendly font
(when window-system
  (setq
  default-font "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font default-font))

(if window-system
  (load-theme 'solarized-dark t)
  (load-theme 'solarized-light t))


(require 'setup-evil)
(require 'setup-cider)
