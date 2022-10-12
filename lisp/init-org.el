;;; init-org.el --- setup org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs))

  ;; Open help:* links with helpful-* instead of describe-*
  (advice-add #'org-link--open-help :around #'doom-use-helpful-a)


  )

(use-package org
  :ensure nil
  ;; :init
  ;; (add-hook 'org-load-hook #'+org-init-hacks-h)
  :hook (org-load . +org-init-hacks-h)
  :config
  ;; To speed up startup, don't put to init section
  (setq org-directory "/home/derek/pkms/gtd/")
  (define-key org-mode-map (kbd "RET") '+org/dwim-at-point)

  ;; having the time a item is done sounds convininet
  (setq org-log-done 'time)

  ;; disable the default org-mode stuck projects agenda view
  (setq org-stuck-projects (quote ("" nil nil "")))

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo)
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  ;; used for filtering block agenda view
  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		(done ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD") )
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD") )
		("DONE" ("WAITING") ("CANCELLED") ("HOLD") ))))

  ;; -------- refile settings -----
  ;; (setq myroamfiles (directory-files "~/pkms/roam" t "org$"))
  ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 5) (myroamfiles :maxlevel . 5)))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-capture-templates
	`(
	  ("t" "Tasks" entry (file "inbox.org")
	   "* TODO %?\n%U\n")
	  ("b" "Books" entry (file,(expand-file-name "inbox.org" org-directory ) )
	   "* Read %^{TITLE}\n\%U%^{AUTHOR}p\n%\\1\n%?"
	   :empty-lines 1)
	  ("n" "Notes" entry (file,(expand-file-name "inbox.org" org-directory ) )
	   "* %?  :NOTES:")
	  ("c" "Calendar" entry (file+headline ,(expand-file-name "tasks.org" org-directory) "Tasks")
	   "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
	   :empty-lines 1)
	  ("o" "Inbox" entry (file+headline ,(expand-file-name "inbox.org" org-directory ) "Others")
	   "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
	   :empty-lines 1)
	  ("w" "Weekly Review" entry (file+olp+datetree ,(concat org-directory "weekly-reviews.org"))
	   (file ,(concat org-directory "templates/weekly_review.org"))
	   :tree-type week)
	  ("l" "org-protocol-capture" entry (file "inbox.org")
	   "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
	  ("d" "Daily entry" entry () (function org-roam-dailies-capture-today)
	   )
	  ("h" "Habit" entry (file "inbox.org")
	   "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%U")
	  )
	)


;;; customize org agenda view
  (setq org-agenda-files (quote ("~/pkms/gtd"
				 "~/pkms/adva"
				 "~/pkms/notes"
				 "~/pkms/roam/beorg-inbox.org"
				 ;; "~/pkms/roam/daily"
				 )))

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
	(quote (("N" "Notes" tags "NOTE"
		 ((org-agenda-overriding-header "Notes")
		  (org-tags-match-list-sublevels t)))
		("h" "Habits" tags-todo "STYLE=\"habit\""
		 ((org-agenda-overriding-header "Habits")
		  (org-agenda-sorting-strategy
		   '(todo-state-down effort-up category-keep))))
		("n" "Next Agenda"
		 ((agenda ""
			  ((org-agenda-span 'day)
			   (org-agenda-start-day nil)
			   (org-deadline-warning-days 3)))
		  (tags-todo "-CANCELLED/!NEXT"
			     ((org-agenda-overriding-header "Next Tasks")
			      (org-tags-match-list-sublevels t)
			      (org-agenda-todo-ignore-with-date t)
			      (org-agenda-sorting-strategy
			       '(effort-up category-keep))))))
		(" " "Block Agenda"
		 ((agenda "" nil)
		  (tags-todo "REFILE"
			     ((org-agenda-overriding-header "Tasks to Refile")
			      (org-tags-match-list-sublevels nil)))
		  (tags-todo "-CANCELLED/!"
			     ((org-agenda-overriding-header "Stuck Projects")
			      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-HOLD-CANCELLED/!"
			     ((org-agenda-overriding-header "Projects")
			      (org-agenda-skip-function 'bh/skip-non-projects)
			      (org-tags-match-list-sublevels 'indented)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-CANCELLED/!NEXT"
			     ((org-agenda-overriding-header (concat "Project Next Tasks"
								    (if bh/hide-scheduled-and-waiting-next-tasks
									""
								      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
			      (org-tags-match-list-sublevels t)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-sorting-strategy
			       '(todo-state-down effort-up category-keep))))
		  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
			     ((org-agenda-overriding-header (concat "Project Subtasks"
								    (if bh/hide-scheduled-and-waiting-next-tasks
									""
								      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-non-project-tasks)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
			     ((org-agenda-overriding-header (concat "Standalone Tasks"
								    (if bh/hide-scheduled-and-waiting-next-tasks
									""
								      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-project-tasks)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-sorting-strategy
			       '(category-keep))))
		  (tags-todo "-CANCELLED+WAITING|HOLD/!"
			     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
								    (if bh/hide-scheduled-and-waiting-next-tasks
									""
								      " (including WAITING and SCHEDULED tasks)")))
			      (org-agenda-skip-function 'bh/skip-non-tasks)
			      (org-tags-match-list-sublevels nil)
			      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
			      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
		  (tags "-REFILE/"
			((org-agenda-overriding-header "Tasks to Archive")
			 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
			 (org-tags-match-list-sublevels nil))))
		 nil))))


  ;; NEXT is only for tasks
  (defun bh/mark-next-parent-tasks-todo ()
    "Visit each parent task and change NEXT states to TODO"
    (let ((mystate (or (and (fboundp 'org-state)
			    state)
		       (nth 2 (org-heading-components)))))
      (when mystate
	(save-excursion
	  (while (org-up-heading-safe)
	    (when (member (nth 2 (org-heading-components)) (list "NEXT"))
	      (org-todo "TODO")))))))

  (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
  (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)


  (defun org-habit-streak-count ()
    (point-min)
    (while (not (eobp))
      (when (get-text-property (point) 'org-habit-p)
	(let ((count (count-matches
		      (char-to-string org-habit-completed-glyph)
		      (line-beginning-position) (line-end-position))))
	  (end-of-line)
	  (insert (number-to-string count))))
      (forward-line 1))
    (point-min))
  (add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)
  )

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))





(provide 'init-org)
;;; init-org.el ends here
