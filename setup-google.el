;;; setup-google.el --- user google configuration entry point.
;;; Commentary:

;; This file configures google specific settings in emacs
;; It follows structure defined in /google/src/head/depot/eng/elisp/google.el
;; and cherrypicks modules to fit my personal preference

(defgroup google-tools nil
  "Google.Tools"
  :prefix "google-"
  :group 'local)

;;;;;;;;;;;;;;;;;;;;;;;; About load-path ;;;;;;;;;;;;;;;;;;;;;;;

;; In Debian, Emacs startup file
;; /etc/emacs/site-start.d/50emacs-google-config.el is loaded and
;; /usr/share/emacs24/site-lisp/google is added to `load-path' where
;; all google emacs packages have been compiled.

;; Update the local emacs files with "ineptitude install emacs-google-config"

;; So there is no need to manipulate the `load-path' to load packages

;; To internal ELPA site http://internal-elpa.appspot.com/
(eval-after-load 'package
  '(add-to-list 'package-archives
                '("GELPA" . "http://internal-elpa.appspot.com/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;; File mode selection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Piccolo has its own mode, which is just a copy of python-mode for now.
(define-derived-mode piccolo-mode python-mode "Piccolo"
  "python-mode variant for Piccolo")

;; Spitfire Template
(require 'cheetah-mode "cheetah")

;; Let emacs know which file extensions belong to which mode.
;; We intentionally simply prepend our list to the pre-existing
;; auto-mode-alist to clobber any values that may have been added by debian's
;; crazy /etc/emacs/site-start.d setup.
(setq auto-mode-alist
      (nconc
       (list
        ;; GSLB configuration files
        (cons "/gslb\\.part\\.cfg$" 'gslb-mode)
        ;; .py files belong in Python mode
        (cons "\\.py$" 'python-mode)
        (cons "\\.pi$" 'piccolo-mode)
        ;; MPM package definition files are Python syntax.
        (cons "/pkgdef$" 'python-mode)
        (cons "/BUILD$" 'google3-build-mode)
        (cons "\\.proto$" 'protobuf-mode)
        (cons "\\.protodevel$" 'protobuf-mode)
        (cons "\\.ncl$" 'ncl-mode)
        ;; .blueprint and .itcnf (Guitar config) files are NCL files.
        (cons "\\.blueprint$" 'ncl-mode)
        (cons "\\.itcnf$" 'ncl-mode)
        ;; SWIG files are made mostly of C++.
        (cons "\\.swig$" 'c++-mode)
        ;; gxp files are just xml files with largely html tags. Some
        ;; people find html-mode annoying (e.g. typing < gives &lt),
        ;; so we map .gxp to whatever mode is used by .html files
        (cons "\\.gxp$"
              (or (cdr (assoc "\\.html$" auto-mode-alist))
                  'html-mode))
        ;; We give our makefiles names like Makefile.foo
        (cons "/Makefile" 'makefile-mode)
        ;; Edit XML files in XML mode (rather than the default SGML
        ;; mode)
        (cons "\\.xml$" 'xml-mode)
        ;; Edit CTemplate files with tpl-mode.
        (cons "\\.tpl$" 'tpl-mode)
        ;; Mendel files
        (cons "mendel/.*\\.gcl$" 'mendel-mode)
        (cons "gws.*\\.gcl$" 'mendel-mode)
        ;; Various Borg-related files
        (cons "\\.bcl$" 'borg-mode)
        (cons "\\.borg$" 'borg-mode)
        (cons "\\.btcfg$" 'borg-mode)
        (cons "\\.gcl$" 'borg-mode)
        (cons "\\.gclx$" 'borg-mode)
        (cons "\\.pp$" 'borg-patchpanel-mode)
        (cons "\\.cfg$" 'borgmon-mode)
        (cons "\\.rules$" 'borgmon-mode)
        (cons "\\.probe$" 'borgmon-mode)
        (cons "\\.alert$" 'borgmon-mode)
        ;; borgmon test just for files under testdata dir
        (cons "google3/production/monitoring/.*/testdata/.*\\.data$"
              'borgmon-test-mode)
        ;; Sawzall
        (cons "\\.szl$" 'sawzall-mode)
        ;; Scraper
        (cons "\\.scraper$" 'scraper-mode)
        ;; ActionScript
        (cons "\\.as$" 'actionscript-mode)
        ;; Go language
        (cons "\\.go$" 'go-mode)
        ;; PathQuery
        (cons "\\.pq$" 'pathquery-mode)
        ;; Megastore Definition Language
        (cons "\\.mdl$" 'sql-mode)
        ;; GYP
        (cons "\\.gypi?$" 'gyp-mode)
        ;; Text protobuf data, traditionally called ASCII before Proto2,
        ;; including Bigtable schemas. (Not to be confused with protobuf-mode
        ;; for protocol buffer definitions.)
        (cons "\\.ascii[-_]?pb$" 'protobuffer-mode)
        (cons "\\.ascii[-_]?proto$" 'protobuffer-mode)
        (cons "\\.pb[-_]?ascii$" 'protobuffer-mode)
        (cons "\\.pb[-_]?te?xt$" 'protobuffer-mode)
        (cons "\\.proto[-_]?ascii$" 'protobuffer-mode)
        (cons "\\.proto[-_]?te?xt$" 'protobuffer-mode)
        (cons "\\.te?xt[-_]?pb$" 'protobuffer-mode)
        (cons "\\.te?xt[-_]?proto$" 'protobuffer-mode)
        (cons "\\.btschema$" 'protobuffer-mode)
        (cons "\\.repoconfig$" 'protobuffer-mode)
        (cons "/METADATA$" 'protobuffer-mode)
        ;; Rosy service definitions.
        (cons "\\.rosy$" 'protobuf-mode)
        ;; Dabba configurations
        (cons "\\.dabba$" 'dabba-mode)
        ;; Spanner SDL files
        (cons "\\.sdl$" 'spansdl-mode)
        ;; GFE urlmap files
        (cons "/urlmap\\.production.*" 'urlmap-mode)
        (cons "/urlmap\\.any.*" 'urlmap-mode)
        (cons "/urlmap\\.part.*" 'urlmap-mode)
        (cons "/urlmap\\.test.*" 'urlmap-mode)
        ;; RVL (shastax language)
        (cons "\\.rvl$" 'rvl-mode)
        )
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Googley features ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google specific autoloads definitions
(require 'google-autoloads)
(require 'google-autogen)
(add-hook 'find-file-not-found-hooks 'autogen)
(require 'google-coding-style)
(require 'google-comint) ;; For password prompts matching

;; Hyperlink Knowledge Graph entity mids in Comint output.
(require 'google-kg)
(google-kg-mode 1)

;; integrate with blaze
(require 'google3-build)
(setq google-build-system "blaze")

;; Googley git / git5.
(eval-after-load "vc-git"
  '(progn
     (require 'google-git)
     (google-git-prevent-index-abuse-in-vc-git)))

;; Grok
(grok-init)

;; Code Search
(require 'csearch)
(require 'csearch-ui)
(global-set-key [f6] 'csearch)

;; Google Gtags
(global-set-key [f7] 'google-show-tag-locations-regexp)
(global-set-key [f8] 'google-show-callers)

(fill-keymap evil-normal-state-map
             "C-]"   'gtags-show-tag-locations-under-point
             "C-o"   'gtags-pop-tag)

;; some evil goodies
(evil-leader/set-key
  "m" 'google3-build
  "g" 'gtags-show-tag-locations-regexp
  "j" 'grok-jump-to-definition-at-point)

;; projectile configuration
(add-to-list 'projectile-project-root-files-bottom-up "BUILD")

;; Store backup and autosave files under /google on local disk
;; more about https://snarfed.org/gnu_emacs_backup_files

(defcustom google-backup-directory "/usr/local/google/tmp/emacs/"
  "Path to a directory for storing auto-saves for files in /google."
  :type 'directory
  :group 'google-tools)

(defun google-create-backup-directory ()
  "Recreate the directory `google-backup-directory' if needed.
This may be needed in case it was deleted by tmpreaper during a
long-running Emacs session."
  (condition-case e
      (progn
        (make-directory google-backup-directory t)
        (unless (eq (file-modes google-backup-directory) #o1777)
          (set-file-modes google-backup-directory #o1777)))
    (error
     (display-warning 'google (format "Problem setting up CITC backup directory: %s" e)))))

(condition-case e
    (progn
      (google-create-backup-directory)
      ;; Ensure we can create a new file before committing to using this
      ;; directory; immediately delete the new file as it's not useful.
      (let ((temporary-file-directory google-backup-directory))
        (delete-file (make-temp-file "test")))
      (add-to-list 'backup-directory-alist
                   `("^/google/" . ,google-backup-directory))
      (add-to-list 'auto-save-file-name-transforms
                   `("^/google/.*/" ,google-backup-directory t))
      (add-hook 'auto-save-hook #'google-create-backup-directory))
  (error
   ;; Be loud about a problem creating the directory; we should just change
   ;; the path if the default is no good.
   (display-warning
    'google
    (format "Problem setting up CITC backup directory: %s" e))))

;; The following prevents the Python-mode comment (ie "-*- mode: python -*-")
;; at the top of BUILD files from forcing python-mode:
(let ((init-build-mode
       '(progn
          (require 'google3-build-mode)
          (google3-build-mode-init))))
  (eval-after-load "python-mode" init-build-mode)
  (eval-after-load "python" init-build-mode))

;; Prevents Emacs from pounding the nfs filers (/home, /google/src/**/home)
;; eg when looking for version control metadata:
(setq locate-dominating-stop-dir-regexp
      (concat "\\(?:" locate-dominating-stop-dir-regexp "\\)"
              "\\|\\`\\(?:/home\\|"
              "/google/src/\\(?:baseline\\|union\\)\\(?:/[0-9]+\\)?/home\\|"
              "~\\)/\\'"))
(setq vc-ignore-dir-regexp locate-dominating-stop-dir-regexp)

;; Likewise, don't let GUD open every source file in sight on SrcFS
(setq gdb-create-source-file-list nil)

(provide 'setup-google)
;;; setup-google.el ends here
