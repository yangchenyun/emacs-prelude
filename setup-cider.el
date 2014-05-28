(eval-after-load 'cider-mode
  '(progn
     (defvar cider-mode-map)
     (evil-make-overriding-map cider-mode-map 'normal)
     (evil-define-key 'normal cider-mode-map
       (kbd "M-<RET>") 'cider-eval-last-sexp
       (kbd "M-S-<RET>") 'cider-eval-defun-at-point)))

(provide 'setup-cider)
;;; setup-cider.el ends here
