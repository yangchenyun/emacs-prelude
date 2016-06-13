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
 '(js-indent-level 2)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-refine-hunk t)
 '(magit-status-headers-hook
   (quote
    (magit-insert-repo-header magit-insert-remote-header magit-insert-head-header magit-insert-upstream-header magit-insert-tags-header)))
 '(magit-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/.org/plans.org" "~/Dropbox/.org/work.org" "~/Dropbox/.org/work.trello" "~/Dropbox/.org/diary.org" "~/Dropbox/.org/meetings.org" "~/Dropbox/.org/inbox.org")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (wordsmith-mode company-auctex anaconda-mode avy company dash helm ido-completing-read+ inf-ruby jedi-core js2-mode json-reformat list-utils magit-popup org perspective popup projectile pythonic request request-deferred s smartparens tablist web-completion-data with-editor zenburn-theme circe flyspell-lazy helm-gtags ggtags xcscope 0blayout ace-link jinja2-mode wc-mode auctex texmathp cdlatex-mode cdlatex virtualenvwrapper company-tern company-web php-mode evil-leader ycmd zop-to-char yasnippet yari yaml-mode web-mode w3m volatile-highlights vkill tern sublimity string-inflection sr-speedbar sotlisp solarized-theme smex smart-mode-line skewer-mode scss-mode ruby-tools restclient rainbow-mode rainbow-delimiters projectile-rails programmer-dvorak prodigy persp-projectile pdf-tools ox-reveal ov osx-dictionary org-trello operate-on-number nasm-mode move-text minitest markdown-mode magit macrostep know-your-http-well keyfreq json-rpc json-mode js-doc jedi impatient-mode ido-vertical-mode ido-ubiquitous hydra helm-projectile helm-ghq helm-ag guru-mode guide-key grizzl god-mode go-mode gitignore-mode gitconfig-mode git-timemachine gist geiser flycheck flx-ido fixmee expand-region exec-path-from-shell evil-surround evil-numbers evil-nerd-commenter evil-lisp-state evil-escape epresent emmet-mode elisp-slime-nav editorconfig easy-kill e2wm discover-my-major discover diminish diff-hl debbugs csv-mode company-anaconda command-log-mode coffee-mode chinese-word-at-point browse-kill-ring bison-mode anzu ag ack-and-a-half ace-window ace-jump-buffer)))
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
