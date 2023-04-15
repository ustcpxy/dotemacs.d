;; setup for extra pacages whicn not builtin -*- lexical-binding: t -*-

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (;;("C-c d j" . my-denote-journal)
	 ("C-c d n" . denote)
	 ("C-c d d" . denote-date)
	 ("C-c d t" . denote-type)
	 ("C-c d s" . denote-subdirectory)
	 ("C-c d f" . denote-open-or-create)
	 ("C-c d r" . denote-dired-rename-file))
  :init
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")

    (add-to-list 'org-capture-templates
		 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :config
  (setq denote-directory (expand-file-name "~/notesdb/denote/"))
  (setq denote-known-keywords '("emacs" "entertainment" "reading" "studying"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)


  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.


  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)

  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
	(list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; (defun my-denote-journal ()
;; "Create an entry tagged 'journal' with the date as its title."
;;     (interactive)
;;     (denote
;;    (format-time-string "%Y-%m-%d") ; "%A %e %B %Y" format like Tuesday 14 June 2022
;;    '("journal"))
  
;;     )
  ;; see https://github.com/protesilaos/denote/issues/95
  (defun first-file-with-substring (dir substring)
  "Return the first file in DIR containing SUBSTRING.
Return nil if there is no files with SUBSTRING in its name."
  (let ((files (file-expand-wildcards (concat dir "*" substring "*"))))
    (when (>= (length files) 1)
      (car files))))

(defun my-denote-journal (&optional date-prompt)
  "Add or modify today's journal entry.

With prefix arg of if DATE-PROMPT is non-nil, prompt for a date."
  (interactive "P")
  (let* ((denote-directory (expand-file-name "~/notesdb/denote/"))  ; I don't keep them in the same place as my other notes.
         ;; (denote-file-type 'markdown-yaml)
         ;; (time (org-read-date nil t))
         ;; (title (format-time-string "%A %-d %B %Y" time))
	          (title (format-time-string "%Y-%m-%d"))

         (file-name-string (concat (replace-regexp-in-string " " "-" (downcase title)) "__journal"))
         (existing-journal-entry (first-file-with-substring (denote-directory) file-name-string)))
    (if existing-journal-entry
        (find-file existing-journal-entry)
      (denote title '("journal")))))

(global-set-key (kbd "C-c d j") #'my-denote-journal)
(global-set-key (kbd "C-c d J") #'(lambda () (interactive) (my-denote-journal t)))
  )

  


(provide 'setup-extra)
