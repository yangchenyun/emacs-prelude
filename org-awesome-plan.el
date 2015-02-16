;;; org-awesome-plan.el --- Make org-mode more awesome with plans.

;;; Commentary:

;; This file contains the code to implement awesome plan system developed by myself back into 2010. It contains periodical plans and related views.

;; Related models:
;; Category: id, no property, act as a label holder
;; Goal: id, hold plans
;; Project: same as goal, for temporary project
;; Action: a normal TODO item, logging time on

;; Plan has a covering period and available hours, in plain English, I plan to "spend * hours in this period"; currently available period includes yearly / monthly / weekly plan.

;;; Code:
;; TODO: reducers to collect by weekly/monthly/yearly of:
;; 1. logged time
;; 2. planned time
;; then it is possible to answer questions like
;; How much effort I've planned on in Jan? For Category 2, for Goal 2 in Jan?
;; How much working hours I have commited in Jan? For Category 2? For Goal 2?
;; How much more effort I've planned but not committed in Jan?
;; How much effort I could plan for March?
;;   minus commited hours / minus previous planned hours

;; TODO: sparse tree view, only view tasks assigned to this week / month

;; TODO: restriction on category/goal ids. when they are created, they should automatically fetch from the top level properties and update them accordingly; category ids must be unique / goal ids must be unique within one category.

;; TODO: monthly and weekly plan should be nested into the yearly plan

(defun org-awesome-plan/mark-as-goal ()
  (interactive (list nil))
  (let ((value (org-read-property-value "GOAL_ID")))
    (org-set-property "GOAL_ID" value)))

(defun org-awesome-plan/is-goal (&optional pom)
  (org-entry-get pom "GOAL_ID" nil))

(defun org-awesome-plan/mark-as-category ()
  (interactive (list nil))
  (let ((value (org-read-property-value "CATEGORY_ID")))
    (org-set-property "CATEGORY_ID" value)))

(defun org-awesome-plan/is-category (&optional pom)
  (org-entry-get pom "CATEGORY_ID" nil))

(defun org-awesome-plan/total-hours-for-year (year)
  (interactive
   (let* ((cur-year (nth 5 (decode-time (current-time))))
          (year (read-number "Year: " cur-year)))
     (list year)))
  (message "%s year total hours: %d"
           year
           (org-awesome-plan/acc-hours-for-period 'year year)))

(defun org-awesome-plan/total-hours-for-month (month)
  (interactive
   (let* ((cur-month (nth 4 (decode-time (current-time))))
          (month (read-number "Month: " cur-month)))
     (list month)))
  (message "%s total hours: %d"
           (calendar-month-name month)
           (org-awesome-plan/acc-hours-for-period 'month month)))

(defun org-awesome-plan/total-hours-for-week (week)
  (interactive
   (let* ((cur-week (read (format-time-string "%U" (current-time))))
          (week (read-number "Week: " cur-week)))
     (list week)))
  (message "%dth week total hours: %d"
           week
           (org-awesome-plan/acc-hours-for-period 'week week)))

(defun org-awesome-plan/create-or-update-year-plan-property (hours &optional year)
  (interactive
   (let* ((cur-year (nth 5 (decode-time (current-time))))
          (hours (read-number "Hours: "))
          (year (read-number "Year: " cur-year)))
     (list hours year)))
  (when (and year (not (string-match "^[12][0-9]\\{3\\}$" (format "%s" year))))
    (user-error "invalid year: %s." year))
  (org-awesome-plan/update-plans-property 'year year hours))

(defun org-awesome-plan/create-or-update-month-plan-property (hours &optional month)
  (interactive
   (let* ((cur-month (nth 4 (decode-time (current-time))))
          (hours (read-number "Hours: "))
          (month (read-number "Month: " cur-month)))
     (list hours month)))
  (when (and month (not (ignore-errors (calendar-month-name month))))
    (user-error "invalid month: %s." month))
  (org-awesome-plan/update-plans-property 'month month hours))

(defun org-awesome-plan/create-or-update-week-plan-property (hours &optional week)
  (interactive
   (let* ((cur-week (read (format-time-string "%U" (current-time))))
          (hours (read-number "Hours: "))
          (week (read-number "Week: " cur-week)))
     (list hours week)))
  (when (and week (not (<= week 52)))
    (user-error "invalid week: %s." week))
  (org-awesome-plan/update-plans-property 'week week hours))

(defun org-awesome-plan/acc-hours-for-period (type period)
  (let ((re (concat (upcase (format ":%s_plan:[ \\t]+" type))
                    "\\((\\(([[:digit:] \\t\\.]+)[ \\t]*\\)+)\\)"))
        (acc 0))
    (goto-char (point-min))
    (while (and (not (eobp))
                (re-search-forward re (point-max) t))
      (let* ((plans (read (match-string-no-properties 1)))
             (hour (org-awesome-plan/plan-hours (assoc period plans))))
        (when hour
          (setq acc (+ acc hour)))))
    acc))

(defun org-awesome-plan/update-plans-property (type period hours)
  (let ((label (upcase (format "%s_plan" type))))
    (org-set-property
    label
    (format "%s" (org-awesome-plan/create-or-update-plans
                  period hours
                  (car (read-from-string
                        (or (org-entry-get nil label nil) "nil"))))))))

;; the data model for plan
(defun org-awesome-plan/create-or-update-plans (period hours &optional plans)
  (if plans
      (let ((plan (assoc period plans)))
        (if plan (progn
                   (setcdr plan hours)
                   plans)
          (add-to-list 'plans (org-awesome-plan/create-plan period hours))))
    (list (org-awesome-plan/create-plan period hours))))

(defun org-awesome-plan/create-plan (period hours)
  (cons period hours))

(defun org-awesome-plan/plan-period (plan)
  (car plan))

(defun org-awesome-plan/plan-hours (plan)
  (cdr plan))

(provide 'org-awesome-plan)
