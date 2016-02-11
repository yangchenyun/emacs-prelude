;;;  org-awesome-plan.el --- Make org-mode more awesome with plans.

;;; Commentary:

;; This file contains the code to implement awesome plan system
;; developed by myself back into 2010. It contains periodical plans
;; and related views.

;; Related models:
;; Category: id, no property, act as a label holder
;; Goal: id, hold plans
;; Project: same as goal, for temporary project
;; Action: a normal TODO item, logging time on

;; Plan has a covering period and available hours, in plain English, I
;; plan to "spend * hours in this period"; currently available period
;; includes yearly / monthly / weekly plan.

;;; Code:
;; 1. [done] logged time (needs to compute time range though)
;; 2. [done] planned time
;; then it is possible to answer questions like
;; How much effort I've planned on in Jan? For Category 2, for Goal 2 in Jan?
;; How much working hours I have commited in Jan? For Category 2? For Goal 2?
;; How much more effort I've planned but not committed in Jan?
;; How much effort I could plan for March?
;;   minus commited hours / minus previous planned hours

;; TODO: restriction on category/goal ids. when they are created, they
;; should automatically fetch from the top level properties and update
;; them accordingly; category ids must be unique / goal ids must be
;; unique within one category.

;; TODO: monthly and weekly plan should be nested into the yearly plan

;; TODO: Investigate `org-clock-get-table-data' / `org-clock-sum' / `org-element'

(require 'dash)

(defvar org-tr-regexp-both)

(declare-function calendar-month-name "calendar" (month &optional abbrev))
(declare-function org-clock-special-range "org-clock" (key &optional time as-strings wstart mstart))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-float-time "org-compat" (&optional time))
(declare-function org-time-string-to-time "org" (s &optional buffer pos))
(declare-function org-make-tdiff-string "org" (y d h m))
(declare-function org-read-property-value "org" (property))
(declare-function org-set-property "org" (property value))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-occur "org" (regexp &optional keep-previous callback))

(defun org-awesome-plan/plan-label (plan-type)
  (upcase (format "%s_plan" plan-type)))

(defun org-awesome-plan/plan-re (plan-type)
  (concat ":" (org-awesome-plan/plan-label plan-type) ":[ \t]+"
          "\\((\\(([[:digit:] \\t\\.]+)[ \\t]*\\)+)\\)"))

;; TODO: add a helper method to calculate a TIME based on
;; week / month / year and passed to org-clock-special-range
(defun org-awesome-plan/logged-hours-for-week ()
  "Calculate the sum of logged hours for current subtree of the
WEEK of current year."
  (interactive)
  (org-awesome-plan/logged-hours-for-period
   (butlast (org-clock-special-range 'thisweek))))

(defun org-awesome-plan/logged-hours-for-month ()
  "Calculate the sum of logged hours for current subtree of the
MONTH of current year."
  (interactive)
  (org-awesome-plan/logged-hours-for-period
   (butlast (org-clock-special-range 'thismonth))))

(defun org-awesome-plan/logged-hours-for-year ()
  "Calculate the sum of logged hours for current subtree in the
YEAR."
  (interactive)
  (org-awesome-plan/logged-hours-for-period
   (butlast (org-clock-special-range 'thisyear))))

(defun org-awesome-plan/logged-hours-until-now ()
  "Calculate the sum of logged hours for current subtree in the
YEAR."
  (interactive)
  (org-awesome-plan/logged-hours-for-period
   (butlast (org-clock-special-range 'untilnow))))

(defun org-awesome-plan/logged-hours-for-period (range)
  "Calculate the sum of logged hours for current subtree in the
RANGE of time."
  (let* ((begin-time (car range))
         (end-time (cadr range))
         (beg (save-excursion
                (org-back-to-heading t)
                (point)))
         (end (save-excursion
                (org-end-of-subtree t t)
                (if (and (org-at-heading-p) (not (eobp)))
                    (backward-char 1))
                (point)))
         (diffs '()))
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward org-tr-regexp-both end t)
            ;; reference `org-evaluate-time-range' for diff calculate
            ;; Remove the cases for time string without time and ignore
            ;; calculation for years.
          (let* ((ts1 (match-string 1))
                 (ts2 (match-string 2))
                 (time1 (org-time-string-to-time ts1))
                 (time2 (org-time-string-to-time ts2))
                 (t1 (max (org-float-time time1)
                          (org-float-time begin-time)))
                 (t2 (min (org-float-time time2)
                          (org-float-time end-time)))
                 (diff (- t2 t1)))
            ;; ignore cases when there is no overlapping of period and
            ;; logging time ranges
            (when (> diff 0)
              (push diff diffs))))
        (let* ((diff (-reduce '+ diffs))
               (ds (* 24 60 60))
               (hs (* 60 60))
               (fh "%02d:%02d")
               (fd (concat "%dd" fh))
               d h m)
          (setq
           d (floor (/ diff ds))  diff (mod diff ds)
           h (floor (/ diff hs))  diff (mod diff hs)
           m (floor (/ diff 60)))
          (message "%s" (org-make-tdiff-string 0 d h m)))))))

(defun org-awesome-plan/mark-as-goal ()
  (interactive)
  (let ((value (org-read-property-value "GOAL_ID")))
    (org-set-property "GOAL_ID" value)))

(defun org-awesome-plan/is-goal (&optional pom)
  (org-entry-get pom "GOAL_ID" nil))

(defun org-awesome-plan/mark-as-category ()
  (interactive)
  (let ((value (org-read-property-value "CATEGORY_ID")))
    (org-set-property "CATEGORY_ID" value)))

(defun org-awesome-plan/is-category (&optional pom)
  (org-entry-get pom "CATEGORY_ID" nil))

(defun org-awesome-plan/only-show-plan-for-week (week)
  (org-awesome-plan/interactive-for week)
  (org-awesome-plan/only-show-plan 'week week))

(defun org-awesome-plan/only-show-plan-for-month (month)
  (org-awesome-plan/interactive-for month)
  (org-awesome-plan/only-show-plan 'month month))

(defun org-awesome-plan/only-show-plan-for-year (year)
  (org-awesome-plan/interactive-for year)
  (org-awesome-plan/only-show-plan 'year year))

(defun org-awesome-plan/total-hours-for-year (year)
  (org-awesome-plan/interactive-for year)
  (message "%s year total hours: %d"
           year
           (org-awesome-plan/acc-hours-for-period 'year year)))

(defun org-awesome-plan/total-hours-for-month (month)
  (org-awesome-plan/interactive-for month)
  (message "%s total hours: %d"
           (calendar-month-name month)
           (org-awesome-plan/acc-hours-for-period 'month month)))

(defun org-awesome-plan/total-hours-for-week (week)
  (org-awesome-plan/interactive-for week)
  (message "%dth week total hours: %d"
           week
           (org-awesome-plan/acc-hours-for-period 'week week)))

(defun org-awesome-plan/create-or-update-year-plan-property (hours &optional year)
  (org-awesome-plan/interactive-for hours year)
  (org-awesome-plan/update-plans-property 'year year hours))

(defun org-awesome-plan/create-or-update-month-plan-property (hours &optional month)
  (org-awesome-plan/interactive-for hours month)
  (org-awesome-plan/update-plans-property 'month month hours))

(defun org-awesome-plan/create-or-update-week-plan-property (hours &optional week)
  (org-awesome-plan/interactive-for hours week)
  (org-awesome-plan/update-plans-property 'week week hours))

(defmacro org-awesome-plan/interactive-for (&rest arg-types)
  "Macros to build `interactive'for commands. ARG-TYPES should be week/month/year/hours.
It will setup reasonable default and call the read functions for
types in PLAN_TYPES and validate the input in the `interactive'
call. "
  `(interactive
    (let* ((cur-week (read (format-time-string "%U" (current-time))))
           (cur-month (nth 4 (decode-time (current-time))))
           (cur-year (nth 5 (decode-time (current-time))))
           (hours ,(when (member 'hours arg-types)
                     '(read-number "Hours: ")))
           (week ,(when (member 'week arg-types)
                    '(read-number "Week: " cur-week)))
           (month ,(when (member 'month arg-types)
                     '(read-number "Month: " cur-month)))
           (year ,(when (member 'year arg-types)
                    '(read-number "Year: " cur-year))))
      (when (and year (not (string-match "^[12][0-9]\\{3\\}$" (format "%s" year))))
        (user-error "invalid year: %s." year))
      (when (and month (not (ignore-errors (calendar-month-name month))))
        (user-error "invalid month: %s." month))
      (when (and week (not (<= week 52)))
        (user-error "invalid week: %s." week))
      ,(cons 'list arg-types))))

(defun org-awesome-plan/acc-hours-for-period (plan-type period)
  "Return the accumulated hours for the PERIOD of PLAN-TYPE in current buffer. "
  (let ((re (org-awesome-plan/plan-re plan-type))
        (acc 0))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (re-search-forward re (point-max) t))
        (let* ((plans (read (match-string-no-properties 1)))
               (hour (org-awesome-plan/plan-hours (assoc period plans))))
          (when hour
            (setq acc (+ acc hour))))))
    acc))

(defun org-awesome-plan/only-show-plan (plan-type period)
  "Make a sparse tree of current buffer of headings contains the PERIOD of PLAN-TYPE. i.e. 8th week, year 2015, 2nd month etc."
  (let* ((case-fold-search nil)
	 (regexp (org-awesome-plan/plan-re plan-type))
	 (callback
	  (lambda ()
            (let ((plans (read (match-string-no-properties 1))))
              (assoc period plans)))))

    (save-match-data
      (message "%d goals in %d %s."
               (org-occur regexp nil callback)
               period plan-type))))

(defun org-awesome-plan/update-plans-property (plan-type period hours)
  "Update the PROPERTY for hours allocated in the PERIOD of
PLAN-TYPE. PLAN-TYPE should be one of 'week/'month/'year and
formatted with `org-awesome-plan/plan-label'. "
  (let ((label (org-awesome-plan/plan-label plan-type)))
    (org-set-property
     label
     (format "%s" (org-awesome-plan/create-or-update-plans
                   period hours
                   (car (read-from-string
                         (or (org-entry-get nil label nil) "nil"))))))))

;; the data model for plan
(defun org-awesome-plan/create-or-update-plans (period hours &optional plans)
  "Create or update the PLANS with PERIOD and HOURS.  IF old
PLANS list exists and PLAN exists for the PERIOD, it is updated
for its HOURS; otherwise, append new PLAN created by
`org-awesome-plan/create-plan' to the PLANS.  If PLANS is nil,
return a list contain a new plan created by
`org-awesome-plan/create-plan'."
  (if plans
      (let ((plan (assoc period plans)))
        (if plan (progn
                   (setcdr plan hours)
                   plans)
          (add-to-list 'plans (org-awesome-plan/create-plan period hours))))
    (list (org-awesome-plan/create-plan period hours))))

(defun org-awesome-plan/create-plan (period hours)
  "PLAN constructor. Create plan from PERIOD and HOURS."
  (cons period hours))

(defun org-awesome-plan/plan-period (plan)
  "Get the period of PLAN."
  (car plan))

(defun org-awesome-plan/plan-hours (plan)
  "Get the hours of PLAN."
  (cdr plan))

(provide 'org-awesome-plan)
;;; org-awesome-plan.el ends here
