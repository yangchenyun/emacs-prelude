;;; setup-evil.el --- user evil configuration entry point.

;;; Commentary:

;; This file configures the behavior of evil in emacs
;; here is some resources it refers to
;; https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el

(require 'yangchenyun-util)
(prelude-require-package 'undo-tree)
(prelude-require-package 'evil-leader)
(prelude-require-package 'evil-nerd-commenter)
(prelude-require-package 'evil)
(prelude-require-package 'evil-surround)
(prelude-require-package 'undo-tree)
(prelude-require-package 'evil-numbers)
(require 'evil-rails)

(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil
      evil-cross-lines t)
(setq evil-default-cursor t)
(setq evil-mode-line-format nil)
(setq evil-leader/leader ",")
(setq evil-want-C-w-in-emacs-state t)

(setq evil-search-module 'evil-search)

(global-evil-leader-mode)

(defun evilnc-default-hotkeys ()
  "Set the hotkeys of evil-nerd-comment"
  (interactive)
  (global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
)
(evilnc-default-hotkeys)

;; FIXME a little overlap with powerline evil configuration
(setq evil-normal-state-tag
      (propertize " N " 'face '((:background "red" :foreground "black")))
      evil-emacs-state-tag
      (propertize " E " 'face '((:background "blue violet" :foreground "black")))
      evil-insert-state-tag
      (propertize " I " 'face '((:background "green")))
      evil-motion-state-tag
      (propertize " M " 'face '((:background "blue")))
      evil-visual-state-tag
      (propertize " V " 'face '((:background "orange" :foreground "black")))
      evil-operator-state-tag
      (propertize " O " 'face '((:background "sky blue"))))



(require-and-exec 'evil-surround
  (setq-default evil-surround-pairs-alist '((?\( . ("(" . ")"))
                                       (?\[ . ("[" . "]"))
                                       (?\{ . ("{" . "}"))

                                       (?\) . ("( " . " )"))
                                       (?\] . ("[ " . " ]"))
                                       (?\} . ("{ " . " }"))
                                       (?>  . ("< " . " >"))

                                       (?# . ("#{" . "}"))
                                       (?p . ("(" . ")"))
                                       (?b . ("[" . "]"))
                                       (?B . ("{" . "}"))
                                       (?< . ("<" . ">"))
                                       (?t . evil-surround-read-tag)))

  (defun yangchenyun/surround-add-pair (trigger begin-or-fun &optional end)
    "Add a surround pair.
     If `end' is nil `begin-or-fun' will be treated as a fun."
    (push (cons (if (stringp trigger)
                    (string-to-char trigger)
                  trigger)
                (if end
                    (cons begin-or-fun end)
                  begin-or-fun))
          evil-surround-pairs-alist))

  (global-evil-surround-mode 1)
  (add-to-hooks (lambda ()
                  (yangchenyun/surround-add-pair "`" "`"  "'"))
                '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-to-hooks (lambda ()
                  (yangchenyun/surround-add-pair "~" "``"  "``"))
                '(markdown-mode-hook rst-mode-hook python-mode-hook))
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (yangchenyun/surround-add-pair "~" "\\texttt{" "}")
                               (yangchenyun/surround-add-pair "=" "\\verb=" "=")
                               (yangchenyun/surround-add-pair "/" "\\emph{" "}")
                               (yangchenyun/surround-add-pair "*" "\\textbf{" "}")
                               (yangchenyun/surround-add-pair "P" "\\(" "\\)")))
  (add-to-hooks (lambda ()
                  (yangchenyun/surround-add-pair "c" ":class:`" "`")
                  (yangchenyun/surround-add-pair "f" ":func:`" "`")
                  (yangchenyun/surround-add-pair "m" ":meth:`" "`")
                  (yangchenyun/surround-add-pair "a" ":attr:`" "`")
                  (yangchenyun/surround-add-pair "e" ":exc:`" "`"))
                '(rst-mode-hook python-mode-hook)))

(evil-mode 1)

(evil-define-command cofi/evil-maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p))
        (entry-key ?j)
        (exit-key ?j))
    (insert entry-key)
    (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(fill-keymap evil-normal-state-map
             "!"     'string-inflection-cycle
             "Y"     (kbd "y$")
             "+"     'evil-numbers/inc-at-pt
             "-"     'evil-numbers/dec-at-pt
             "SPC"   'evil-scroll-down
             "DEL"   'evil-scroll-up
             "C-SPC" 'evil-ace-jump-word-mode
             "go"    'evil-ace-jump-line-mode
             "C-t"   'transpose-chars
             "gH"    'evil-window-top
             "gL"    'evil-window-bottom
             "gM"    'evil-window-middle
             "H"     'beginning-of-line
             "L"     'end-of-line
             "M-p"   'helm-projectile
             "M-P"   'projectile-ag
             "M-."   'find-tag

             ;; smartparens

             ;; Finds opening '(' of the current list.
             "(" 'sp-backward-up-sexp
             ;; Finds closing ')' of the current list.
             ")" 'sp-up-sexp
             ;; Go to the start of current/previous sexp
             "[[" 'sp-backward-sexp
             ;; Go to the start of next sexp.
             "]]" 'sp-forward-sexp

             "}"  'sp-forward-barf-sexp
             "{"  'sp-backward-barf-sexp

             "gn" 'sp-next-sexp ;; in a AST sence
             "gp" 'sp-previous-sexp
             "gk" 'sp-kill-sexp
             "gu" 'sp-raise-sexp
             "gy" 'sp-copy-sexp
             )

(fill-keymap evil-motion-state-map
             "y"     'evil-yank
             "Y"     (kbd "y$")
             "_" 'evil-first-non-blank
             "C-e"   'end-of-line
             "C-S-d" 'evil-scroll-up
             "C-S-f" 'evil-scroll-page-up
             "_"     'evil-first-non-blank
             "C-y"   nil)

(fill-keymap evil-insert-state-map
             "j"   'cofi/evil-maybe-exit
             "C-h" 'backward-delete-char
             "C-k" 'kill-line
             "C-y" 'yank
             "M-p" 'helm-projectile
             "C-e" 'end-of-line)

(fill-keymaps (list evil-operator-state-map
                    evil-visual-state-map)
             "SPC"   'evil-scroll-down
             "DEL"   'evil-scroll-up
             "C-SPC" 'evil-ace-jump-word-mode)

(defun cofi/clear-empty-lines ()
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "^ +$" line)
      (delete-region (point-at-bol) (point-at-eol)))))

(add-hook 'evil-insert-state-exit-hook #'cofi/clear-empty-lines)

(evil-leader/set-key
  "W" 'save-some-buffers
  "k" 'kill-current-buffer
  "K" 'kill-buffer-and-window
  "f" 'ido-find-file
  "d" 'dired-jump
  "b" 'ibuffer
  "m" 'compile
  "/" 'evil-ex-nohighlight
  "r" 'prelude-recentf-ido-find-file
  "i" 'imenu
  "s" 'helm-occur
  "S" 'eshell
  "." 'smex
  "p" 'projectile-switch-project

  ;; nerd commenter
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region

  ;; smartparens
  ;; https://github.com/emezeske/paredit.vim/blob/master/doc/paredit.txt
  "<" 'sp-backward-slurp-sexp
  ">" 'sp-forward-slurp-sexp
  "J" 'sp-join-sexp
  "O" 'sp-split-sexp
  "S" 'sp-splice-sexp
  "w(" (prelude-wrap-with "(")
  "w{" (prelude-wrap-with "{")
  "w[" (prelude-wrap-with "[")
)

;; Integration with other mode

;; default initial state in modes
(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode     . emacs)
                                 (pylookup-mode                . emacs)
                                 (comint-mode                  . emacs)
                                 (ebib-entry-mode              . emacs)
                                 (ebib-index-mode              . emacs)
                                 (ebib-log-mode                . emacs)
                                 (elfeed-show-mode             . emacs)
                                 (elfeed-search-mode           . emacs)
                                 (gtags-select-mode            . emacs)
                                 (shell-mode                   . emacs)
                                 (term-mode                    . emacs)
                                 (bc-menu-mode                 . emacs)
                                 (magit-branch-manager-mode    . emacs)
                                 (makey-key-mode               . emacs)
                                 (semantic-symref-results-mode . emacs)
                                 (rdictcc-buffer-mode          . emacs)
                                 (gtags-browse-tags-mode       . emacs)
                                 (finder-mode                  . emacs)
                                 (project-explorer-mode        . motion)
                                 (prodigy-mode                 . motion)
                                 (ibuffer-mode                 . normal)
                                 (org-agenda-mode              . motion)
                                 (erc-mode                     . normal))
         do (evil-set-initial-state mode state))

(evil-add-hjkl-bindings gtags-browse-tags-mode-map 'emacs)
(evil-add-hjkl-bindings git-rebase-mode-map 'emacs)
(evil-add-hjkl-bindings occur-key-map 'emacs)
(evil-add-hjkl-bindings package-menu-mode-map 'emacs
  "H" 'package-menu-quick-help
)

(setq ;; C-<leader> to access from all buffers
      evil-leader/in-all-states t
      ;; enable <leader> anyway
      evil-leader/no-prefix-mode-rx (list
                                     "occur-mode"
                                     "package-menu-mode"
                                     "ibuffer-mode"
                                     "magit-.*-mode"
))

(eval-after-load 'org-agenda
  '(progn
     ;; use the standard Dired bindings as a base
     (defvar org-agenda-mode-map)
     (evil-make-overriding-map org-agenda-mode-map 'motion)
     (evil-add-hjkl-bindings org-agenda-mode-map 'motion
       "j" 'org-agenda-next-line
       "k" 'org-agenda-previous-line
       "J" 'org-agenda-goto-date
       "L" 'org-agenda-log-mode
       "K" 'org-agenda-capture
       "S" 'org-agenda-schedule
)))

(eval-after-load 'elisp-slime-nav-mode
  '(progn
     (evil-define-key 'insert smartparens-mode-map [(shift return)] 'sp-newline)))

(eval-after-load 'elisp-slime-nav
  '(progn
     (defvar elisp-slime-nav-mode-map)
     (evil-make-overriding-map elisp-slime-nav-mode-map 'normal)
     (evil-add-hjkl-bindings elisp-slime-nav-mode-map 'normal)))

(eval-after-load 'smartparens
  '(progn
     (evil-define-key 'insert smartparens-mode-map [(shift return)] 'sp-newline)))

(eval-after-load 'e2wm
  '(progn
     (defvar e2wm:def-plugin-files-mode-map)
     (evil-make-overriding-map e2wm:def-plugin-files-mode-map 'normal)

     (defvar e2wm:def-plugin-imenu-mode-map)
     (evil-make-overriding-map e2wm:def-plugin-imenu-mode-map 'normal)

     (defvar e2wm:def-plugin-history-list-mode-map)
     (evil-make-overriding-map e2wm:def-plugin-history-list-mode-map 'normal)
))

(provide 'setup-evil)
;;; setup-evil.el ends here
