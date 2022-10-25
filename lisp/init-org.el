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

  ;; having the time a item is done sounds convininet
  (setq org-log-done 'time)

  ;; disable the default org-mode stuck projects agenda view
  (setq org-stuck-projects (quote ("" nil nil "")))

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)

    (setq org-return-follows-link t)

    (setq org-plantuml-jar-path
          (expand-file-name "~/.doom.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.doom.d/ditaa.jar")

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo)

  (defun my/org-return (&optional indent)
    "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
When optional INDENT argument is non-nil, call
`newline-and-indent' instead of `newline'.
When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
    (interactive)
    (let ((context (if org-return-follows-link (org-element-context)
		     (org-element-at-point))))
      (cond
       ;; In a table, call `org-table-next-row'.  However, before first
       ;; column or after last one, split the table.
       ((or (and (eq 'table (org-element-type context))
		 (not (eq 'table.el (org-element-property :type context)))
		 (>= (point) (org-element-property :contents-begin context))
		 (< (point) (org-element-property :contents-end context)))
	    (org-element-lineage context '(table-row table-cell) t))
	(if (or (looking-at-p "[ \t]*$")
		(save-excursion (skip-chars-backward " \t") (bolp)))
	    (insert "\n")
	  (org-table-justify-field-maybe)
	  (call-interactively #'org-table-next-row)))
       ;; On a link or a timestamp, call `org-open-at-point' if
       ;; `org-return-follows-link' allows it.  Tolerate fuzzy
       ;; locations, e.g., in a comment, as `org-open-at-point'.
       ((and org-return-follows-link
	     (or (and (eq 'link (org-element-type context))
		      ;; Ensure point is not on the white spaces after
		      ;; the link.
		      (let ((origin (point)))
			(org-with-point-at (org-element-property :end context)
			  (skip-chars-backward " \t")
			  (> (point) origin))))
		 (org-in-regexp org-ts-regexp-both nil t)
		 (org-in-regexp org-tsr-regexp-both nil t)
		 (org-in-regexp org-any-link-re nil t)))
	(call-interactively #'org-open-at-point))
       ;; Insert newline in heading, but preserve tags.
       ((and (not (bolp))
	     (let ((case-fold-search nil))
	       (org-match-line org-complex-heading-regexp)))
	;; At headline.  Split line.  However, if point is on keyword,
	;; priority cookie or tags, do not break any of them: add
	;; a newline after the headline instead.
	(let ((tags-column (and (match-beginning 5)
				(save-excursion (goto-char (match-beginning 5))
						(current-column))))
	      (string
	       (when (and (match-end 4) (org-point-in-group (point) 4))
		 (delete-and-extract-region (point) (match-end 4)))))
	  ;; Adjust tag alignment.
	  (cond
	   ((not (and tags-column string)))
	   (org-auto-align-tags (org-align-tags))
	   (t (org--align-tags-here tags-column))) ;preserve tags column
	  (end-of-line)
	  (org-show-entry)
	  (if indent (newline-and-indent) (newline))
	  (when string (save-excursion (insert (org-trim string))))))
       ;; In a list, make sure indenting keeps trailing text within.
       ((and indent
	     (not (eolp))
	     (org-element-lineage context '(item)))
	(let ((trailing-data
	       (delete-and-extract-region (point) (line-end-position))))
	  (newline-and-indent)
	  (save-excursion (insert trailing-data))))
       ((and (eolp) (org-at-item-p))
	(end-of-visible-line)
	(org-insert-item (org-at-item-checkbox-p)))
       (t
	;; Do not auto-fill when point is in an Org property drawer.
	(let ((auto-fill-function (and (not (org-at-property-p))
				       auto-fill-function)))
	  (if indent
	      (newline-and-indent)
	    (newline)))))))


  (define-key org-mode-map (kbd "RET")
    'my/org-return)

  (evil-define-key 'normal org-mode-map
    "+" #'org-cycle-list-bullet)

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
		 nil)
      ("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda ""
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))
		)))


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
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))





(provide 'init-org)
;;; init-org.el ends here
