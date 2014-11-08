;; Emacs IRC client
(require 'prelude-erc)
(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-helm) ;; Interface for narrowing and search
;; (require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations
;; (require 'prelude-mediawiki)
;; (require 'prelude-evil)

;;; Programming languages support
(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
;; (require 'prelude-css)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-go)
;; (require 'prelude-haskell)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp)
;; (require 'prelude-ocaml)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
;; (require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-shell)
;; (require 'prelude-scss)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)

(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode 1)

(setq projectile-switch-project-action 'projectile-dired)
(prelude-require-package 'ag)
(prelude-require-package 'perspective)
(prelude-require-package 'persp-projectile)
(require 'persp-projectile)
(persp-mode)
(define-key projectile-mode-map [?\s-p] 'projectile-persp-switch-project)

(prelude-require-package 'discover)
(global-discover-mode 1)

(prelude-require-package 'keyfreq)
(setq keyfreq-file (expand-file-name "keyfreq" prelude-savefile-dir))
(setq keyfreq-file-lock (expand-file-name "keyfreq-lock" prelude-savefile-dir))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(setq find-function-C-source-directory "~/vendor/emacs/src")

(sp-use-smartparens-bindings)

;; as rgrep.el doesn't working in fish-shell
(define-key prelude-mode-map [?\s-g] 'projectile-ag)

(prelude-require-package 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; extempore setup
(require 'extempore)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
(defun extempore-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))
(add-hook 'extempore-mode-hook 'extempore-coding-defaults)
(setq user-extempore-directory "/usr/local/Cellar/extempore/0.53/")

(require 'powerline)
(powerline-evil-theme)

(if (equal emacs-version "24.4.1")
    (prettify-symbols-mode))

(prelude-require-package 'skewer-mode)
(skewer-setup)

(prelude-require-package 'e2wm)
(require 'e2wm)

(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-engines-alist  '(("underscorejs"    . "\\.ejs\\'")))

;; follow Steve Yegge's suggestion
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "C-c <RET>") 'smex)

;; eval like Light Table
(define-key key-translation-map (kbd "M-<RET>") (kbd "C-x C-e"))
;; (global-set-key (kbd "M-<RET>") 'eval-last-sex)

; no scroll bars
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

; be quiet
(setq ring-bell-function 'ignore)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; mac friendly font
(setq ns-use-srgb-colorspace t)

(defconst preferred-monospace-fonts
  `(
    ("DejaVu Sans Mono" . 120)
    ("Source Code Pro" . ,(if (eq system-type 'darwin) 130 100))
    ("Anonymous Pro" . ,(if (eq system-type 'darwin) 135 110))
    ("Anonymous Pro Minus" . ,(if (eq system-type 'darwin) 135 110))
    ("Monaco For Powerline" . ,(if (eq system-type 'darwin) 130 110))
    ("Menlo" . 120)
    ("Consolas" . 130)
    ("Courier New" . 130))
  "Preferred monospace fonts
The `car' of each item is the font family, the `cdr' the preferred font size.")

(defconst preferred-proportional-fonts
  '(("Lucida Grande" . 120)
    ("DejaVu Sans" . 110))
  "Preferred proportional fonts
The `car' of each item is the font family, the `cdr' the preferred font size.")

(defun first-existing-font (fonts)
  "Get the first existing font from FONTS."
  (--first (x-family-fonts (car it)) fonts))

(setq current-monospace-font preferred-monospace-fonts)
(defun cycle-fonts ()
  "Cycle through the monospace fonts"
  (interactive)
  (if (null current-monospace-font)
      (setq current-monospace-font preferred-monospace-fonts)
    (setq current-monospace-font (cdr current-monospace-font)))
  (-when-let (font (first-existing-font current-monospace-font))
    (--each '(default fixed-pitch)
      (set-face-attribute it nil
                          :family (car font) :height (cdr font))
      (message "%S" font))))
(global-set-key [f5] 'cycle-fonts)

(defun choose-best-fonts ()
  "Choose the best fonts."
  (interactive)
  (-when-let (font  (first-existing-font preferred-monospace-fonts))
    (--each '(default fixed-pitch)
      (set-face-attribute it nil
                          :family (car font) :height (cdr font))))
  (-when-let (font (first-existing-font preferred-proportional-fonts))
    (set-face-attribute 'variable-pitch nil
                        :family (car font) :height (cdr font))))

(choose-best-fonts)

(prelude-require-package 'solarized-theme)
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
(setq solarized-high-contrast-mode-line nil)

(setq theme-current themes-options)

(defun theme-cycle ()
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current themes-options))
  (load-theme (car theme-current) 'no-confirm)
  (message "%S" (car theme-current)))

(global-set-key [f4] 'theme-cycle)

(require 'setup-evil)
(require 'setup-window)
(require 'setup-org)
(require 'setup-cider)

;; load google-configuration
(require 'setup-google)

;; start emacs server so that you can use emacsclient to open new files
;; quickly in your one emacs session (which you start after a reboot and
;; keep open until your next reboot)
(server-start)
