(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
;;
;; follow Steve Yegge's suggestion
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "C-c <RET>") 'smex)

;; eval like Light Table
(global-set-key (kbd "M-<RET>") 'eval-last-sexp)

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
(setq ns-use-srgb-colorspace t)


(when window-system
  (setq
   menlo-font "-apple-Menlo-regular-normal-normal-*-12-*-*-*-m-0-iso10646-1")
  (setq
   monaco-font "-apple-Monaco-normal-normal-*-12-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font menlo-font)
  (set-face-attribute 'fixed-pitch nil :font menlo-font)
  (set-face-attribute 'variable-pitch nil :font menlo-font))

(defun pick-color-theme (frame)
  (if (window-system frame)
      (load-theme 'solarized-dark t)
    (load-theme 'solarized-light t)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (pick-color-theme frame)))

;; for direct loading
(pick-color-theme nil)

(setq themes-options (list
       'solarized-dark
       'solarized-light
       'zenburn
))

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

(setq theme-current themes-options)
(defun theme-cycle ()
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current themes-options))
  (load-theme (car theme-current))
  (message "%S" (car theme-current)))

(global-set-key [f4] 'theme-cycle)

(require 'setup-evil)
(require 'setup-window)
(require 'setup-org)
(require 'setup-cider)
