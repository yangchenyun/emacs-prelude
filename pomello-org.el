;;;  pomello-org.el --- Utilities to sync pomello times to org-trello-mode.

;;; Commentary:

;;; pomello-sync-item-to-org-clock: syncs one item's pomello time to org-mode clock
;;; pomello-sync-buffer-to-org-clock: syncs for one buffer


;;; Code:

(require 's)

(defconst pomello-ts-reg
  (rx (and " - *"
           (group
            (repeat 1 2 digit) ":" (repeat 1 2 digit) blank (or "pm" "am")
            " on "
            (repeat 3 letter) blank (repeat 1 2 digit) "," blank (repeat 4 digit))
           "*"))
  "Regular expression to match pomello time stamp.")

(defconst pomello-start-ts-reg
  (concat "Timer started" pomello-ts-reg )
  "Regular expression to match pomello start time stamp.")

(defconst pomello-end-ts-reg
  (concat (rx (or "Timer ended"
                  "Timer paused"
                  "Task voided")) pomello-ts-reg)
  "Regular expression to match pomello end time stamp.")

(defun pomello-parse-time (timestr)
  "Parse the TIMESTR to time used by pomello."
  ;;; `org-read-date' helps to handle am/pm
  (date-to-time
   (org-read-date nil nil
                  (s-replace-all '((" am" . "am") (" pm" . "pm")) timestr))))

(defun pomello-after-org-clock-p (tstart beg end)
  "Check TSTART against latest org clock records for the item.
Return 't if TSTART is not covered in the existing records."
  (interactive)
  (let* ((re (concat "CLOCK: "
                     "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
         ts te tt)
    (save-excursion
      (goto-char beg)
      (re-search-forward re end t)
      (if (match-end 2)
          ;; Two time stamps
          (progn
            (setq ts (match-string 1)
                  te (match-string 2)
                  ts (org-float-time
                      (apply 'encode-time (org-parse-time-string ts)))
                  te (org-float-time
                      (apply 'encode-time (org-parse-time-string te)))
                  tt (org-float-time tstart))
            (and (> tt ts) (> tt te)))
        'empty))))

(defun pomello-sync-item-to-org-clock ()
  "Copy the pomello ts in comments to org."
  (interactive)
  (let* ((beg (save-excursion
                (org-back-to-heading t)
                (point)))
         (end (save-excursion
                (org-end-of-subtree t t)
                (if (and (org-at-heading-p) (not (eobp)))
                    (backward-char 1))
                (point)))
         (stimes '())
         (etimes '()))
    (save-excursion
      (save-match-data
        (widen)
        (goto-char beg)
        (while (re-search-forward pomello-start-ts-reg end t)
          (let ((time (pomello-parse-time (match-string 1))))
            (push time stimes))))
      (save-match-data
        (widen)
        (goto-char beg)
        (while (re-search-forward pomello-end-ts-reg end t)
          (let ((time (pomello-parse-time (match-string 1))))
            (push time etimes))))

      ;;; print happens in reverse chronical order
      (dotimes (i (length stimes))
        (goto-char beg)
        (org-clock-find-position (point))
        (let ((stime (nth i stimes))
              (etime (nth i etimes)))
          (if (pomello-after-org-clock-p stime beg end)
             (progn
               (org-clock-in nil stime)
               (org-clock-out nil nil etime))))))))

(defun pomello-sync-buffer-to-org-clock ()
  "Copy the pomello ts in comments to org for current buffer."
  (interactive)
  (save-excursion
    (let ((last-pos (point-min))
          (next-pos (point-min)))
      (goto-char last-pos)
      (org-forward-heading-same-level nil)
      (setq next-pos (point))
      (while (not (= last-pos next-pos))
        (pomello-sync-item-to-org-clock)
        (setq last-pos next-pos)
        (org-forward-heading-same-level nil)
        (setq next-pos (point))))))

(provide 'pomello-org)

;;; pomello-org.el ends here
