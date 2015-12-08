;;; setup-google.el --- user google configuration entry point.
;;; Commentary:

;; This file configures google specific settings in emacs
;; It follows structure defined in /google/src/head/depot/eng/elisp/google.el
;; and cherrypicks modules to fit my personal preference

;;; Code:
(unless (not (file-exists-p "/google"))
  ;; Add gmacs's site-lisp load-path
  ;; NOTE: in order to use ELPA's version of third_party packages,
  ;;   some packages are commented out in the google.el loaded below
  (setq ami-emacs-version
        (concat "gmacs-" (replace-regexp-in-string "\\.[0-9]+$" ""
                                                   emacs-version)))
  (setq debian-emacs-flavor (make-symbol ami-emacs-version))
  (add-to-list 'load-path (concat "/usr/share/" ami-emacs-version
                                  "/site-lisp/emacs-google-config/"
                                  "devtools/editors/emacs/"))
  (require 'google)
  (require 'google-ycmd))

(provide 'setup-google)
;;; setup-google.el ends here
