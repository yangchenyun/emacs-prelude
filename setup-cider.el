(fill-keymap cider-mode-map
             "M-." 'cider-jump
             "M-," 'cider-jump-back
             "C-<RET>"     'cider-eval-last-sexp
             "M-<RET>"     'cider-eval-defun-at-point)

(provide 'setup-cider)
;;; setup-cider.el ends here
