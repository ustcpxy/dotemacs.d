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
  (setq denote-directory (expand-file-name "~/notesdb/denote/"))
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


;;; 英文翻译配置
;; 这个插件依赖于 `posframe' 这个插件
(use-package posframe
  :ensure t
  )
(require 'sdcv)
(setq sdcv-say-word-p t)               ;say word after translation

(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic")) ;setup directory of stardict dictionary

(setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '("朗道汉英字典5.0"
        "朗道英汉字典5.0"
        ))

(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '(
	"朗道英汉字典5.0"
        "朗道汉英字典5.0"
	"Longman Dictionary of Contemporary English"
        ))
  (setq sdcv-tooltip-timeout 10)
  (setq sdcv-fail-notify-string "没找到释义")
(setq sdcv-tooltip-border-width 2)

(global-set-key (kbd "C-,") 'sdcv-search-pointer+)

(use-package fanyi
  :ensure t
  :bind-keymap ("C-=" . fanyi-map)
  :bind (:map fanyi-map
              ("w" . fanyi-dwim2)
              ("i" . fanyi-dwim))
  :init
  ;; to support `org-store-link' and `org-insert-link'
 (require 'ol-fanyi)
  ;; 如果当前指针下有单词，选择当前单词，否则选择剪贴板
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("x" "New word" entry (file+olp+datetree "/home/derek/notesdb/denote/20230419T164630--vocabulary__studying.org" "New")
                   "* %^{Input the new word:|%(cond ((with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'word 'no-properties))) ((clipboard/get)))}\n\n[[fanyi:%\\1][%\\1]]\n\n[[https://dictionary.cambridge.org/dictionary/english/%\\1][Cambridge：%\\1]]%?"
                   :tree-type day
                   :empty-lines 1
                   :jump-to-captured t)))
  :config
  (defvar fanyi-map nil "keymap for `fanyi")
  (setq fanyi-map (make-sparse-keymap))
  (setq fanyi-sound-player "mpv")
  
  ;; 从剪贴板获取内容
(defun clipboard/get ()
  "return the content of clipboard as string"
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (buffer-substring-no-properties (point-min) (point-max))))
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; ;; Etymonline
                     ;; fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider
                     ;; ;; LibreTranslate
                     ;; fanyi-libre-provider
                     )))

(provide 'setup-extra)
