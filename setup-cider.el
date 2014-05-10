(eval-after-load 'cider-mode
  '(fill-keymaps '(cider-mode-map
                   evil-normal-state-map)
     "M-." 'cider-jump
     "M-," 'cider-jump-back
     "C-<RET>"     'cider-eval-last-sexp
     "M-<RET>"     'cider-eval-defun-at-point))

(provide 'setup-cider)
;;; setup-cider.el ends here
