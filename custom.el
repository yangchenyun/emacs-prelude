(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("/Users/steveyang/infotxt")))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(evil-symbol-word-search t)
 '(geiser-active-implementations (quote (guile racket chicken scsh)))
 '(geiser-default-implementation (quote scsh))
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-refine-hunk t)
 '(magit-status-headers-hook
   (quote
    (magit-insert-repo-header magit-insert-remote-header magit-insert-head-header magit-insert-upstream-header magit-insert-tags-header)))
 '(magit-use-overlays nil)
 '(sh-imenu-generic-expression
   (quote
    ((sh
      ("Func" "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?" 1)
      ("Func" "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()" 1))
     (bash
      ("Func" "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_:]*\\)\\s-*\\(?:()\\)?" 1)
      ("Func" "^\\s-*\\([[:alpha:]_][[:alnum:]_:]*\\)\\s-*()" 1)
      ("Vars" "\\(?:^[[:blank:]]*declare[[:blank:]]*\\(?:-[[:alpha:]][[:blank:]]*\\)?\\([_[:alpha:]]+\\)\\(?:=.+\\)?$\\)" 1)))))
 '(yas-prompt-functions (quote (yas-ido-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "#839496" :strike-through t)))))
