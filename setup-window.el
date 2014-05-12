(require 'evil)
(require 'cofi-windowing)
(require 'yangchenyun-util)

;; allow C-w to be shadowed in emacs-state -- `evil-want-C-w-in-emacs-state'
;; doesn't allow this
(global-set-key (kbd "C-w") evil-window-map)

;; override binding of C-w for other mode which overrides it
(add-to-hooks (lambda () (local-set-key (kbd "C-w") evil-window-map))
              '(magit-mode-hook))

(fill-keymap evil-window-map
    "C-h" nil
    "d" 'cofi/window-toggle-dedicate
    ;; Splitting
    "s" 'cofi/smart-split
    "\\" 'split-window-vertically
    "|" 'split-window-horizontally
    "/" 'cofi/multi-split

    ;; Deleting
    "D"   'delete-window
    "C-d" 'delete-window
    "1"   'delete-other-windows

    ;; Sizing
    "RET" 'enlarge-window
    "-"   'shrink-window-horizontally
    "="   'enlarge-window-horizontally

    ;; Moving
    "<left>"  'evil-window-left
    "<down>"  'evil-window-down
    "<up>"    'evil-window-up
    "<right>" 'evil-window-right

    ;; Swapping
    "M-h"       'swap-with-left
    "M-j"       'swap-with-down
    "M-k"       'swap-with-up
    "M-l"       'swap-with-right
    "S-<left>"  'swap-with-left
    "S-<down>"  'swap-with-down
    "S-<up>"    'swap-with-up
    "S-<right>" 'swap-with-right
    "SPC"       'swap-window

    "g" 'cofi/goto-window

    ;; winner-mode
    "u" 'winner-undo
    "C-r" 'winner-redo
    ;; shadow rotating in evil-window-map
    "C-R" 'winner-redo)

(provide 'setup-window)
