(require 'org-protocol)
;; Register a org-protocol:// scheme onto the system
;; http://jcardente.blogspot.com/2010/09/saving-weblinks-to-org-mode-from-safari.html

;; org-trello setup
(prelude-require-package 'org-trello)
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
              (org-trello-mode)))))

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(define-key org-mode-map "<"
  (lambda () (interactive)
     (if (looking-back "^")
         (hydra-org-template/body)
       (self-insert-command 1))))

;; normal state shortcuts
(evil-define-key 'normal org-mode-map
  "gu" 'outline-up-heading
  "gj" 'org-forward-heading-same-level
  "gk" 'org-backward-heading-same-level
  "gh" 'outline-previous-visible-heading
  "gl" 'outline-next-visible-heading
  "H" 'org-shiftleft
  "L" 'org-shiftright
  "*" 'org-toggle-heading
  "t" 'org-todo
  "T" 'org-set-tags-command
  ";t" 'org-show-todo-tree
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  "-" 'org-cycle-list-bullet
  "W" 'org-refile
  (kbd "TAB") 'org-cycle)

(evil-leader/set-key-for-mode 'org-mode
  "oa" 'org-agenda
  "oc" 'org-capture
  "oj" 'org-clock-goto
  "n" 'org-narrow-to-subtree
  "w" 'widen

  ;; awesome plan
  "Y" 'org-awesome-plan/total-hours-for-year
  "M" 'org-awesome-plan/total-hours-for-month
  "W" 'org-awesome-plan/total-hours-for-week
  "Py" 'org-awesome-plan/create-or-update-year-plan-property
  "Pm" 'org-awesome-plan/create-or-update-month-plan-property
  "Pw" 'org-awesome-plan/create-or-update-week-plan-property
  "Psy" 'org-awesome-plan/only-show-plan-for-year
  "Psm" 'org-awesome-plan/only-show-plan-for-month
  "Psw" 'org-awesome-plan/only-show-plan-for-week

  "^" 'org-sort
  ;; clock related keymaps
  "c"  'hydra-org-clock/body
  "t"  'hydra-org-trello/body

  "e" 'org-set-effort
  "a" 'org-archive-subtree-default
)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))

(defvar org-dir "~/Dropbox/.org")
(setq org-directory (expand-file-name org-dir)
      org-agenda-files (list (concat org-dir "/plans.org")
                             (concat org-dir "/work.org")
                             (concat org-dir "/work.trello")
                             (concat org-dir "/meetings.org")
                             (concat org-dir "/expr/newformat.org")
                             (concat org-dir "/inbox.org"))
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-completion-use-ido t        ; Complete with IDO in Org
      org-yank-adjusted-subtrees t    ; Adjust level when yanking entire trees
      org-enforce-todo-dependencies t
      org-clock-into-drawer t
      org-M-RET-may-split-line nil
      org-pretty-entities t
      org-refile-targets '((org-agenda-files :maxlevel . 4))
      org-archive-default-command 'org-archive-subtree-default-with-confirmation
      org-clock-persist 'history
      org-todo-keywords '((sequence
                           "TODO(t)"
                           "STARTED(s)"
                           "WAITING(w@/!)" ;; note for enter, timestamp for leaving
                           "SOMEDAY(.)"
                           "|" "DONE(x)" "CANCELLED(c@)")
                          (sequence "TODELEGATE(-@)" "DELEGATED(d!)" "COMPLETE(x!)"))
      org-todo-keyword-faces
      '(("TODO" . (:foreground "#eee8d5" :weight bold))
        ("TODELEGATE" . (:foreground "#eee8d5" :weight bold))
        ("STARTED" . (:foreground "#dc322f" :weight bold))
        ("WAITING" . (:foreground "#9EA0E5" :weight bold))
        ("DELEGATED" . (:foreground "#9EA0E5" :weight bold))
        ("SOMEDAY" . (:foreground "#F771AC" :weight bold))

        ("CANCELLED" . (:foreground "#839496" :weight normal))
        ("COMPLETE" . (:foreground "#839496" :weight normal :strike-through t))
        ("DONE" . (:foreground "#B4C342" :weight normal :strike-through t))
        )
      org-fontify-done-headline t

      org-tag-alist '(
                      ;; For the type system
                      (:startgroup . nil)
                      ("STUDY" . ?s)
                      (:grouptags . nil)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("WORK" . ?w)
                      (:grouptags . nil)
                      ("PAID WORK" . ?p)
                      ("UNPAID WORK" . ?u)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("LEISURE" . ?l)
                      (:grouptags . nil)
                      ("EXERCISE" . ?e)
                      ("TRAVEL" . ?t)
                      ("PLAY" . ?p)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("ERRANDS" . ?r)
                      (:grouptags . nil)
                      ("SHOPPING" . nil)
                      ("COMMUTE" . ?c)
                      ("EAT" . nil)
                      (:endgroup . nil)

                      ;; for context
                      ("@home" . ?h)
                      ("@away" . ?a)

                      )

      org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))
      )

;; strike through headline
(custom-set-faces
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "#839496" :strike-through t)))))

(defvar sacha/org-basic-task-template "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:" "Basic task data")

(setq org-capture-templates
      `(
        ("t" "Tasks" entry
         (file+headline (expand-file-name "inbox.org" org-directory) "Tasks")
         ,sacha/org-basic-task-template)
        ;; for insert tasks quickly
        ("d" "Done - Task" entry
         (file+headline (expand-file-name "inbox.org" org-directory) "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("i" "Ideas" entry
         (file+headline (expand-file-name "inbox.org" org-directory) "Ideas")
         "* %^{Idea} %u\n%?\n\n%i")
        ("q" "Quick note" item
         (file+headline (expand-file-name "inbox.org" org-directory) "Quick Notes"))
        ("j" "Daily Journal" entry
         (file+datetree (expand-file-name "inbox.org" org-directory))
         "* %?\n\n%i\n")
        ("m" "Meeting Journal" entry
         (file+datetree (expand-file-name "meetings.org" org-directory))
         "* %?\n\n%i\n")))

(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports."
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
        (insert "\n\n"
                (format-time-string (car org-time-stamp-formats)
                                    (seconds-to-time start))
                "----------------\n")
        (org-dblock-write:clocktable
         (plist-put
          (plist-put
           params
           :tstart
           (format-time-string (car org-time-stamp-formats)
                               (seconds-to-time start)))
          :tend
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time end))))
        (setq start (+ 86400 start))))))

;; FIXME, the org-make-tags-matcher reports error
(defun wicked/org-calculate-tag-time (matcher &optional ts te)
  "Return the total minutes clocked in headlines matching MATCHER.
MATCHER is a string or a Lisp form to be evaluated, testing if a
given set of tags qualifies a headline for inclusion. TS and TE
are time start (inclusive) and time end (exclusive). Call with a
prefix to be prompted for TS and TE.

For example, to see how much time you spent on tasks tagged as
URGENT, call M-x wicked/org-calculate-tag-time RET URGENT RET. To
see how much time you spent on tasks tagged as URGENT today, call
C-u M-x wicked/org-calculate-tag-time RET URGENT RET . RET +1 RET."
  (interactive (list
		(read-string "Tag query: ")
		(if current-prefix-arg (org-read-date))
		(if current-prefix-arg (org-read-date))))
  ;; Convert strings to proper arguments
  (if (stringp matcher) (setq matcher (cdr (org-make-tags-matcher matcher))))
  (if (stringp ts)
      (setq ts (time-to-seconds (apply 'encode-time (org-parse-time-string ts)))))
  (if (stringp te)
      (setq te (time-to-seconds (apply 'encode-time (org-parse-time-string te)))))
  (let* ((re (concat "[\n\r]" outline-regexp " *\\(\\<\\("
		     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		     (org-re
		      "\\>\\)\\)? *\\(.*?\\)\\(:[[:alnum:]_@:]+:\\)?[ \t]*$")))
	 (case-fold-search nil)
         lspos
	 tags tags-list tags-alist (llast 0) rtn level category i txt p
	 marker entry priority (total 0))
    (save-excursion
      (org-clock-sum ts te)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(catch :skip
	  (setq tags (if (match-end 4) (match-string 4)))
	  (goto-char (setq lspos (1+ (match-beginning 0))))
	  (setq level (org-reduced-level (funcall outline-level))
		category (org-get-category))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the nex tags
	  (when tags
	    (setq tags (mapcar 'downcase (org-split-string tags ":"))
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr tags-alist))
		  tags))
	  (when (and (eval matcher)
		     (or (not org-agenda-skip-archived-trees)
			 (not (member org-archive-tag tags-list))))
	    ;; Get the time for the headline at point
	    (goto-char (line-beginning-position))
	    (setq total (+ total (or (get-text-property (1+ (point)) :org-clock-minutes) 0)))
	    ;; if we are to skip sublevels, jump to end of subtree
	    (org-end-of-subtree t)))))
    (if (called-interactively-p `interactive)
	(let* ((h (/ total 60))
	       (m (- total (* 60 h))))
	  (message "Time: %d:%02d (%d hours and %d minutes)" h m h m)))
    total))

(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
(defun sacha/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))

(defun sacha/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(defun sacha/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
   (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

;; Override the key definition in org-agenda
(org-add-hook
 'org-agenda-mode-hook
 '(lambda ()
   (define-key org-agenda-mode-map "x" 'sacha/org-agenda-done)
   (define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup)
   (define-key org-agenda-mode-map "N" 'sacha/org-agenda-new)))

(defun guide-key/org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/org-mode)

(org-add-hook
 'org-mode-hook
 '(lambda ()
    (org-indent-mode)
    (org-clock-persistence-insinuate)
    (add-to-list 'org-modules "org-habit")))

;;; For latex support in org-mode
(prelude-require-package 'cdlatex)
(require 'texmathp)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;;; Patch to make Latex and Biber work for org-mode
(setq org-latex-pdf-process (list "latexmk -f -pdf %f"))

;; 1. hook flyspell into org-mode
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)

;; 2. ignore message flags
(setq flyspell-issue-message-flag nil)

;; 3. ignore tex commands
(add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))

;; TODO(steveyang): only when using latex and make it asynchronous
;; (add-hook 'after-save-hook
;;           (lambda () (when (and
;;                        (eq major-mode 'org-mode)
;;                        t
;;                   (org-latex-export-to-pdf))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)))

;; Allow multiple line emphasis, see http://goo.gl/kSk18e
(setcar (nthcdr 4 org-emphasis-regexp-components) 10)

(setq org-src-fontify-natively t)

;; Disable whitespace-mode highlighting
(add-hook 'org-mode-hook
          (lambda () (whitespace-mode -1)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (awk . t)
   ))
(setq org-babel-sh-command "bash")

(require 'org-awesome-plan)
(provide 'setup-org)
