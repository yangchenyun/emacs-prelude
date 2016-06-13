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
(require 'prelude-latex)
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

(prelude-require-package 'guide-key)
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-h"))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/idle-delay 0.5)
(guide-key-mode 1)  ; Enable guide-key-mode

;; AucTeX,
;; http://www.stefanom.org/setting-up-a-nice-auctex-environment-on-mac-os-x/
(prelude-require-package 'auctex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer"
         "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(prelude-require-packages '(company-web company-tern))
(require 'company-web-html)
(require 'company-tern)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))


(prelude-require-package 'hydra)
(require 'hydra)
(require 'setup-hydra)

(prelude-require-package 'nasm-mode)
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(nasm\\|s\\)$" . nasm-mode))

;; convert between under_score and CamelCase
(prelude-require-package 'string-inflection)
(require 'string-inflection)

(add-to-list 'recentf-exclude "/tmp/buildifier.*")

(prelude-require-package 'prodigy)
(prelude-require-package 'js-doc)
(prelude-require-package 'emmet-mode)
(prelude-require-package 'tern)
(prelude-require-package 'editorconfig)
(prelude-require-package 'yasnippet)
(setq yas/root-directory (list "~/.emacs.d/personal/yasnippets"))
;; (yas-global-mode)
;; (yas-reload-all)
(prelude-require-package 'programmer-dvorak)
(require 'programmer-dvorak)

(setq whitespace-global-modes '(not org-mode))

(setq tab-width 4)

;; Typescript integration
(prelude-require-package 'tss)
(require 'typescript)
(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")
(tss-config-default)

;; (add-hook 'typescript-mode-hook
;;           (lambda ()
;;             (tide-setup)
;;             (flycheck-mode +1)
;;             (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;             (eldoc-mode +1)
;;             (company-mode-on)))

;; ycmd configuration
(prelude-require-package 'ycmd)
(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-server-command
              (list "python" (expand-file-name "~/.ghq/github.com/Valloric/ycmd/ycmd")))

(prelude-require-package 'virtualenvwrapper)
(require 'virtualenvwrapper)

;; http documentation
(prelude-require-package 'know-your-http-well)

(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

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

(if (version< "24.4.1" emacs-version)
    (global-prettify-symbols-mode))

(prelude-require-package 'skewer-mode)
(skewer-setup)

(prelude-require-package 'e2wm)

(setq speedbar-use-images nil)
(global-set-key "\M-`" 'speedbar-get-focus)

;;; web-mode setup
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Enable JavaScript completion between <script>...</script> etc.
(defadvice company-tern (before web-mode-set-up-ac-sources activate)
  "Set `tern-mode' based on current language before running company-tern."
  (if (equal major-mode 'web-mode)
      (let ((web-mode-cur-language
             (web-mode-language-at-pos)))
        (if (or (string= web-mode-cur-language "javascript")
                (string= web-mode-cur-language "jsx"))
            (unless tern-mode (tern-mode))
          (if tern-mode (tern-mode -1))))))
(defun custom-web-mode-hook ()
  "Hooks for web mode."
  (whitespace-mode -1)  ; disable whitespace-mode
  (emmet-mode) ; enable emmet-mode for web-mode
  (setq web-mode-engines-alist  '(("underscorejs"    . "\\.ejs\\'")))
  (setq web-mode-markup-indent-offset 2)  ; html indent
  (setq web-mode-css-indent-offset 2)  ; css indent
  (setq web-mode-code-indent-offset 2)  ; script / code indent
  )
(add-hook 'prelude-web-mode-hook 'custom-web-mode-hook)

(defun text-mode-hook-setup ()
  ;; make `company-backends' local in text-mode only
  (make-local-variable 'company-backends)
  ;; company-ispell is the plugin to complete words
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'text-mode-hook 'text-mode-hook-setup)

;; follow Steve Yegge's suggestion
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "C-c <RET>") 'smex)

;; eval like Light Table
(define-key key-translation-map (kbd "M-<RET>") (kbd "C-x C-e"))
;; (global-set-key (kbd "M-<RET>") 'eval-last-sex)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

;; terminal mode settings
(unless window-system
  (xterm-mouse-mode 1)
  (menu-bar-mode 1)
  (setq select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))

; be quiet
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)

(when (eq system-type 'darwin)
  (prelude-require-package 'osx-dictionary)
  (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
  (prelude-swap-meta-and-super))


;; mac friendly font
(setq ns-use-srgb-colorspace t)

(defconst preferred-monospace-fonts
  `(
    ("Menlo For Powerline" . ,(if (eq system-type 'darwin) 120 100))
    ("Monaco For Powerline" . ,(if (eq system-type 'darwin) 115 100))
    ("Hack" . 110)
    ("DejaVu Sans Mono" . 105)
    ("Source Code Pro" . ,(if (eq system-type 'darwin) 130 100))
    ("Anonymous Pro" . ,(if (eq system-type 'darwin) 135 110))
    ("Anonymous Pro Minus" . ,(if (eq system-type 'darwin) 135 110))
    ("Consolas" . 130)
    ("Courier New" . 130))
  "Preferred monospace fonts.
The `car' of each item is the font family, the `cdr' the preferred font size.")

(defconst preferred-proportional-fonts
  '(("Lucida Grande" . 120)
    ("DejaVu Sans" . 110))
  "Preferred proportional fonts.
The `car' of each item is the font family, the `cdr' the preferred font size.")

(defun first-existing-font (fonts)
  "Get the first existing font from FONTS."
  (--first (x-family-fonts (car it)) fonts))

(setq current-monospace-font preferred-monospace-fonts)
(defun cycle-fonts ()
  "Cycle through the monospace fonts."
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

(add-hook 'prelude-ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "ruby " "\"" buffer-file-name "\""))))

(add-hook 'prelude-python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python " "'" buffer-file-name "'"))))

(add-hook 'prelude-go-mode-hook
          (lambda ()
            ;; Call Gofmt before saving
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; Customize compile command to run go build
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go generate && go build -v && go test -v && go vet"))
            ;; godef jump
            (local-set-key (kbd "M-.") 'godef-jump)))

;; load pomello script
(require 'pomello-org)

;; load google-configuration
(require 'setup-google)

;; start emacs server so that you can use emacsclient to open new files
;; quickly in your one emacs session (which you start after a reboot and
;; keep open until your next reboot)
(server-start)
