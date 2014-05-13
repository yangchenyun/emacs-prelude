(prelude-require-package 'org-mac-link)
(require 'org-protocol)
;; Register a org-protocol:// scheme onto the system
;; http://jcardente.blogspot.com/2010/09/saving-weblinks-to-org-mode-from-safari.html

;; adapted from evil-org-mode with customized settings for evil keymaps
(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode)

(defun always-insert-item ()
  "Force insertion of org item"
  (if (not (org-in-item-p))
      (insert "\n- ")
    (org-insert-item))
  )

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil)
  )

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "gu" 'outline-up-heading
  "gj" 'org-forward-heading-same-level
  "gk" 'org-backward-heading-same-level
  "gh" 'outline-previous-visible-heading
  "gl" 'outline-next-visible-heading
  "H" 'org-shiftleft
  "L" 'org-shiftright
  "*" 'org-toggle-heading
  "t" 'org-todo
  "T" 'org-set-tags-command
  ";t" 'org-show-todo-tree
  "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))

(evil-leader/set-key-for-mode 'org-mode
  "^" 'org-sort
  ;; clock related keymaps
  "ci" 'org-clock-in
  "cl" 'org-clock-in-last
  "co" 'org-clock-out
  "cq" 'org-clock-cancel
  "cd" 'org-clock-display
  "cj" 'org-clock-goto
  "ce" 'org-clock-modify-effort-estimate
)

(defun config-org-mode ()
  (interactive)
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("STARTED" . "yellow")
          ("CANCELED" . (:foreground "blue" :weight bold))))
  (setq org-enforce-todo-dependencies t)
  (setq org-M-RET-may-split-line nil)
  (add-to-list 'org-modules "org-habit")
  (org-agenda-files "~/.org")
  (org-indent-mode t)
  (lambda ()
    (local-unset-key (kbd "C-u")))
)

;; only load with org-mode is activated
(add-hook 'org-mode-hook 'config-org-mode)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(provide 'setup-org)
