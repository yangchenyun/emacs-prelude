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
(prelude-require-package 'surround)
(prelude-require-package 'undo-tree)
(prelude-require-package 'evil-numbers)

(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil
      evil-cross-lines t)
(setq evil-default-cursor t)
(setq evil-mode-line-format nil)
(setq evil-leader/leader ","
      ;; C-<leader> to access from all buffers
      evil-leader/in-all-states t
      ;; enable <leader> anyway
      evil-leader/no-prefix-mode-rx (list
                                     "occur-mode"
                                     "ibuffer-mode"
                                     "magit-.*-mode"
))

(setq evil-search-module 'evil-search)

(global-evil-leader-mode)
(evilnc-default-hotkeys)

(setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

(require-and-exec 'surround
  (setq-default surround-pairs-alist '((?\( . ("(" . ")"))
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
                                       (?t . surround-read-tag)))

  (defun yangchenyun/surround-add-pair (trigger begin-or-fun &optional end)
    "Add a surround pair.
     If `end' is nil `begin-or-fun' will be treated as a fun."
    (push (cons (if (stringp trigger)
                    (string-to-char trigger)
                  trigger)
                (if end
                    (cons begin-or-fun end)
                  begin-or-fun))
          surround-pairs-alist))

  (global-surround-mode 1)
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

(evil-set-toggle-key "<pause>")
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
                                 (semantic-symref-results-mode . emacs)
                                 (rdictcc-buffer-mode          . emacs)
                                 (erc-mode                     . normal))
         do (evil-set-initial-state mode state))

(fill-keymap evil-normal-state-map
             "Y"     (kbd "y$")
             "+"     'evil-numbers/inc-at-pt
             "-"     'evil-numbers/dec-at-pt
             "SPC"   'evil-ace-jump-char-mode
             "C-SPC" 'evil-ace-jump-word-mode
             "go"    'evil-ace-jump-line-mode
             "C-t"   'transpose-chars
             "gH"    'evil-window-top
             "gL"    'evil-window-bottom
             "gM"    'evil-window-middle
             "H"     'beginning-of-line
             "L"     'end-of-line
             "C-u"   'evil-scroll-up
             "C-;"   'eval-expression
             "M-p"   'helm-projectile
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
             "SPC"   'evil-ace-jump-char-mode
             "C-SPC" 'evil-ace-jump-word-mode)

;; Make HJKL keys work in special buffers
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-process-mode-map 'emacs)
(evil-add-hjkl-bindings git-rebase-mode-map 'emacs)
(evil-add-hjkl-bindings ibuffer-mode-map 'emacs)
(evil-add-hjkl-bindings occur-mode 'emacs)

(defun cofi/clear-empty-lines ()
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "^ +$" line)
      (delete-region (point-at-bol) (point-at-eol)))))

(add-hook 'evil-insert-state-exit-hook #'cofi/clear-empty-lines)

(evil-leader/set-key
  "w" 'save-buffer
  "W" 'save-some-buffers
  "k" 'kill-current-buffer
  "K" 'kill-buffer-and-window
  "f" 'ido-find-file
  "d" 'dired-jump
  "b" 'ibuffer
  "m" 'compile
  "/" 'evil-ex-nohighlight
  "a" 'org-agenda
  "r" 'prelude-recentf-ido-find-file
  "s" 'cofi/split-shell
  "S" 'eshell
  "." 'smex)

(provide 'setup-evil)
;;; setup-evil.el ends here
