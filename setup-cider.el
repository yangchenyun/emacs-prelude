(defun cider-eval-defun-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(defun cider-eval-last-sexp-in-repl ()
  (interactive)
  (let ((form (cider-last-sexp)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(eval-after-load 'cider-mode
  '(progn
     (defvar cider-mode-map)
     (evil-make-overriding-map cider-mode-map 'normal)
     (evil-define-key 'normal cider-mode-map
       (kbd "M-<RET>") 'cider-eval-last-sexp-in-repl
       [(control return)] 'cider-eval-defun-at-point-in-repl)))

(provide 'setup-cider)
;;; setup-cider.el ends here
