;;; setup-hydra.el --- hydra configuration.
;;; Commentary:
;;; Setup of hydra for different mode / plugins.

;;; Code:
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

(defhydra hydra-window (:color orange
                        :hint nil)
  "
 Split: _s_:split _H_:oriaontal _V_:vertical
Delete: _o_nly  _da_ce  _x_window  _db_uffer  _df_rame
  Move: _S_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
;;; TODO(steveyang): when hjik is pressed after a command after a
;;; short threshold, automatically quit hydra map.
;;; If makes C-w l l works seamlessly
  ("h" windmove-left :exit t)
  ("j" windmove-down :exit t)
  ("k" windmove-up :exit t)
  ("l" windmove-right :exit t)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" cofi/smart-split)
  ("V" split-window-vertically)
  ("H" split-window-horizontally)
  ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("S" ace-swap-window)
  ("da" ace-delete-window)
  ("x" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ;("i" ace-maximize-window "ace-one" :color blue)
  ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))


(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     _B_ash      plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("e" (progn
         (hot-expand "<s")
         (insert "emacs-lisp")
         (forward-line)))
  ("p" (progn
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("u" (progn
         (hot-expand "<s")
         (insert "plantuml :file CHANGE.png")
         (forward-line)))
  ("P" (progn
         (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("B" (progn
         (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env bash\"\n")
         (hot-expand "<s")
         (insert "sh")
         (forward-line)))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defhydra hydra-project (:color blue :hint nil :idle 0.4)
        "
                                                                    ╭────────────┐
    Files             Search          Buffer             Do         │ Projectile │
  ╭─────────────────────────────────────────────────────────────────┴────────────╯
    [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
    [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
    [_r_] recent file   [_S_] occur       [_V_] ibuffer        [_i_] info
    [_d_] dir           [_R_] replace     [_K_] kill all       [_s_] switch
    [_o_] other         [_t_] find tag
    [_u_] test file     [_T_] make tags
    [_h_] root
                                                                        ╭────────┐
    Other Window      Run             Cache              Do             │ Fixmee │
  ╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
    [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
    [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
    [_D_] dir           [_c_] shell       [_ks_] cleanup
    [_O_] other         [_C_] command     [_kd_] remove
    [_B_] buffer
  --------------------------------------------------------------------------------
        "
        ("<tab>" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("a"   projectile-ag)
        ("A"   projectile-grep)
        ("b"   projectile-switch-to-buffer)
        ("B"   projectile-switch-to-buffer-other-window)
        ("c"   projectile-run-async-shell-command-in-root)
        ("C"   projectile-run-command-in-root)
        ("d"   projectile-find-dir)
        ("D"   projectile-find-dir-other-window)
        ("f"   projectile-find-file)
        ("F"   projectile-find-file-other-window)
        ("g"   projectile-vc)
        ("h"   projectile-dired)
        ("i"   projectile-project-info)
        ("kc"  projectile-invalidate-cache)
        ("kd"  projectile-remove-known-project)
        ("kk"  projectile-cache-current-file)
        ("K"   projectile-kill-buffers)
        ("ks"  projectile-cleanup-known-projects)
        ("l"   projectile-find-file-dwim)
        ("L"   projectile-find-file-dwim-other-window)
        ("m"   projectile-compile-project)
        ("o"   projectile-find-other-file)
        ("O"   projectile-find-other-file-other-window)
        ("p"   projectile-commander)
        ("r"   projectile-recentf)
        ("S"   projectile-multi-occur)
        ("s"   projectile-switch-project)
        ("R"   projectile-replace)
        ("t"   projectile-find-tag)
        ("T"   projectile-regenerate-tags)
        ("u"   projectile-find-test-file)
        ("U"   projectile-test-project)
        ("v"   projectile-display-buffer)
        ("V"   projectile-ibuffer)
        ("X"   fixmee-mode)
        ("x"   fixmee-view-listing))


(defun ora-open-info (topic bname)
  "Open info on TOPIC in BNAME."
  (if (get-buffer bname)
      (progn
        (switch-to-buffer bname)
        (unless (string-match topic Info-current-file)
          (Info-goto-node (format "(%s)" topic))))
    (info topic bname)))

(defhydra hydra-info-to (:hint nil :color teal)
  "
_o_rg e_l_isp _e_macs _h_yperspec"
  ("o" (ora-open-info "org" "*org info*"))
  ("l" (ora-open-info "elisp" "*elisp info*"))
  ("e" (ora-open-info "emacs" "*emacs info*"))
  ("h" (ora-open-info "gcl" "*hyperspec*")))

(defhydra hydra-info (:color blue :hint nil :idle 0.4)
        "
                                                                  ╭────────────┐
    Nav Files             Navi Nodes          Action              │ Info-mode  │
  ╭───────────────────────────────────────────────────────────────┴────────────╯
    [_?_] summary       [_[_] forward          [_g_] goto node
    [_<_] top node      [_]_] backward         [_s_] search
    [_>_] final node    [_f_] follow ref       [_S_] case-search
    [_d_] info dir      [_l_] hist back        [_m_] menu
    [_i_] index         [_r_] hist forward     [_h_] help
    [_I_] virtual index [_n_] next             [_t_] info-to
    [_L_] hist          [_p_] previous
    [_T_] TOC           [_u_] up
        "
        ("?" Info-summary)
        ("]" Info-forward-node)
        ("[" Info-backward-node)
        ("<" Info-top-node)
        (">" Info-final-node)
        ;; ("b" beginning-of-buffer)
        ;; ("e" end-of-buffer)
        ("h" Info-help)
        ("d" Info-directory)
        ("f" Info-follow-reference)
        ("g" Info-goto-node)
        ("l" Info-history-back)
        ("r" Info-history-forward)
        ("i" Info-index)
        ("I" Info-virtual-index)
        ("L" Info-history)
        ("n" Info-next)
        ("p" Info-prev)
        ("s" Info-search)
        ("S" Info-search-case-sensitively)
        ("T" Info-toc)
        ("u" Info-up)
        ("m" Info-menu)
        ("t" hydra-info-to/body))

(define-key Info-mode-map "." 'hydra-info/body)

(defhydra hydra-org-trello (:color blue :hint nil :idle 0.4)
        "
                                                                  ╭────────────┐
    Cards               Boards                 Sync               │ Org-Trello │
  ╭───────────────────────────────────────────────────────────────┴────────────╯
    [_a_] archive       [_l_] show labels      [_s_] push card
    [_j_] jump to card  [_J_] jump to board    [_b_] push buffer
    [_m_] comment       [_I_] install board    [_c_] push comment
    [_k_] delete card   [_U_] update board     [_S_] pull card
    [_@_] assign me     [_C_] create board     [_B_] pull buffer
                                               [_C_] pull comment
        "

        ("a" org-trello/archive-card)
        ("j" org-trello/jump-to-trello-card)
        ("k" org-trello/kill-entity)
        ("@" org-trello/assign-me)
        ("m" org-trello/add-card-comment)

        ("l" org-trello/show-board-labels)
        ("J" org-trello/jump-to-trello-board)
        ("I" org-trello/install-board-metadata)
        ("U" org-trello/update-board-metadata)
        ("C" org-trello/create-board-and-install-metadata)

        ;; ("S" org-trello/sync-card) needs to do trello->card sync
        ("s" org-trello/sync-card)
        ("b" org-trello/sync-buffer)
        ("c" org-trello/sync-comment)
        ("S" (org-trello/sync-card 4))
        ("B" (org-trello/sync-buffer 4))
        ("C" (org-trello/sync-comment 4))

        ;; not listed in the menu
        ("K" org-trello/kill-cards)
        ("A" org-trello/archive-cards))

(provide 'setup-hydra)
;;; setup-hydra.el ends here.
