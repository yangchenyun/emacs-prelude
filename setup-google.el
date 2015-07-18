;;; setup-google.el --- user google configuration entry point.
;;; Commentary:

;; This file configures google specific settings in emacs
;; It follows structure defined in /google/src/head/depot/eng/elisp/google.el
;; and cherrypicks modules to fit my personal preference

;;; Code:
(unless (not (file-exists-p "/google"))
  (require 'google))

(provide 'setup-google)
;;; setup-google.el ends here
