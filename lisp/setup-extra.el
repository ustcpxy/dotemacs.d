;; setup for extra pacages whicn not builtin -*- lexical-binding: t -*-

(use-package denote
  :ensure nil
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (("C-c d j" . my-denote-journal-for-today)
	 ("C-c d J" . my-denote-journal-with-date)
	 ("C-c d n" . denote)
	 ("C-c d d" . denote-date)
	 ("C-c d t" . denote-type)
	 ("C-c d s" . denote-subdirectory)
	 ("C-c d f" . my-denote-note)
	 ("C-c d r" . denote-dired-rename-file))
  :init
  (setq denote-directory (expand-file-name "~/notesdb/"))
    (require 'denote-silo-extras)
    (setq denote-silo-extras-directories '("~/hugo-blog/content/posts/"))
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")

    (add-to-list 'org-capture-templates
		 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'my-denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :config

  (setq denote-known-keywords '("emacs" "entertainment" "reading" "studying"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'markdown-yaml) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-yaml-front-matter
 "---
title:      %s
date:       %s
lastmod: 
tags:       %s
categories: 
draft:  false
toc: true
identifier: %S
---\n\n"	)
  
  ;; @https://gsj987.github.io/posts/take-note-with-denote/
  ;;; 配置目录结构，让其与 logseq 的兼容，这样就能通过 icloud 在移动端读取笔记
  (setq denote-journal-home (expand-file-name "journals/" denote-directory))
  (setq denote-note-home (expand-file-name "denote/" denote-directory))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)


  ;; Read this manual for how to specify `denote-templates'.  We do
  ;; not include an example here to avoid potential confusion.


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
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

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


  ;; @https://gsj987.github.io/posts/take-note-with-denote/
  ;;; 根据日期创建或打开一篇 journal
  (defun my-denote-journal-with-date (date)
    "Create an entry tagged 'journal' and the other 'keywords' with the date as its title, there will be only one entry per day."
    ;;; 如果没传日期，则使用日历选择一个日期创建
    (interactive (list (denote-date-prompt)))
    (let* ((formatted-date (format-time-string "%Y-%m-%d" (denote--valid-date date)))
	   (entry-of-date-regex (concat "^[^\\.].*" formatted-date))
	   (entry-of-date (car (directory-files denote-journal-home nil entry-of-date-regex)))
	   )

      (if entry-of-date
	  (find-file (expand-file-name entry-of-date denote-journal-home))
	(denote
	 formatted-date
	 '("journal")
	 nil
	 denote-journal-home)
	)))

  ;;; 创建或打开今天的 journal
  (defun my-denote-journal-for-today ()
    "Write a journal entry for today."
    (interactive)
    (my-denote-journal-with-date
     (format-time-string "%Y-%m-%dT00:00:00")))

  (defun my-denote-split-org-subtree-to-journal()
    "Refile the org subtree as a node of the journal"
    (interactive)

    (org-copy-subtree)
    (delete-region (org-entry-beginning-position) (org-end-of-subtree))
    (my-denote-journal-for-today)
    (end-of-buffer)
    (org-return)
    (org-yank))
  
  (defun my-denote-weekly ()
    "Create an entry tagged 'weekly' with the date as its title.
If a journal for the current day exists, visit it.  If multiple
entries exist, prompt with completion for a choice between them.
Else create a new file."
    (interactive)
    (let* ((denote-directory denote-note-home)
	   (week (format-time-string "%Y Week%W"))
	   (string (denote-sluggify-title week))
	   (files (denote-directory-files-matching-regexp string)))
      (cond
       ((> (length files) 1)
	(find-file (completing-read "Select file: " files nil :require-match)))
       (files
	(find-file (car files)))
       (t
	(denote
	 week
	 '("weekly-review"))))))

  
  (defun my-denote-note ()
    "Create a note to pages, need to provide a title and tag"
    (interactive)
    (let ((denote-prompts '(title keywords))
          (denote-directory denote-note-home))
      (call-interactively #'denote-open-or-create)))

  (defun my-denote-org-capture()
    "Capture a note to pages"
    (interactive)
    (let ((denote-directory denote-note-home))
      (denote-org-capture)))


  (defun my-denote-split-org-subtree-to-note ()
    "Create new Denote note as an Org file using current Org subtree."
    (interactive)
    (let* ((keywords (denote--keywords-prompt))
           (text (org-get-entry))
           (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment))
           (tags (org-get-tags)))

      (delete-region (org-entry-beginning-position) (org-end-of-subtree))

      (if (> (length tags) 0)
          (dolist (tag tags)
            (push tag keywords)))

      (let (path)
        (save-window-excursion
          (denote heading keywords nil denote-note-home)
          (insert text)
          (save-buffer)
          (setq path (buffer-file-name)))
        (denote-link path))
      ))

  (defun my-denote-link-or-create-note()
    "Link or create a note"
    (interactive)
    (let ((denote-directory denote-note-home))
      (call-interactively #'denote-link-or-create)))
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

(global-set-key (kbd "C-;") 'sdcv-search-pointer+)

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

;;; wgrep
(require 'wgrep)

;;; consistent structural editing interface
(repeat-mode 1)
(defvar structural-edit-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("u" . backward-up-list)
                     ("f" . forward-sexp)
                     ("b" . backward-sexp)
                     ("d" . down-list)
                     ("k" . kill-sexp)
                     ("n" . sp-next-sexp)
                     ("p" . sp-previous-sexp)
                     ("K" . sp-kill-hybrid-sexp)
                     ("]" . sp-forward-slurp-sexp)
                     ("[" . sp-backward-slurp-sexp)
                     ("}" . sp-forward-barf-sexp)
                     ("{" . sp-backward-barf-sexp)
                     ("C" . sp-convolute-sexp)
                     ("J" . sp-join-sexp)
                     ("S" . sp-split-sexp)
                     ("R" . sp-raise-sexp)
                     ("\\" . indent-region)
                     ("/" . undo)
                     ("t" . transpose-sexps)
                     ("x" . eval-defun)))
      (define-key map (kbd k) f))
    map))

(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'structural-edit-map))
 structural-edit-map)

;; (use-package apheleia
;;   :ensure t
;;   :init
;;   (apheleia-global-mode +1)

;;   :hook
;;   (prog-mode . apheleia-mode)
;;   :config
;;                    (setf (alist-get 'clang-format apheleia-formatters)
;;         '("git" "clang-format" "--force" "--quiet" (buffer-file-name)))
  
;;   (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
;;   )

;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (yas-global-mode 1)
;;   :config
;;   (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))


(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "snippets"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  (defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  
  (define-auto-insert "\\.c?$" ["default-c.c" my/autoinsert-yas-expand]))

(load "gendoxy.el")


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "pandoc -t html5 -f gfm  --embed-resources --standalone --mathjax --quiet --highlight-style=zenburn --template github.html5")
  (setq markdown-asymmetric-header t) ;; 默认使用命令插入标题会在左右两侧对称加上#标记，设置为true，则只会在左侧有标记
  )




;; 支持直接复制图片到emacs buffer，图片自动保持到buffer文件的相对目录 ./img/目录下
(if (eq system-type 'windows-nt)
    (progn
    (setq pasteex-executable-path (expand-file-name "~/PasteEx/PasteEx.exe"))
  (require 'pasteex-mode)
  (global-set-key (kbd "C-x p i") 'pasteex-image)
  )
  )

;; 解决ripgrep搜索不了中文
;; see @https://emacs-china.org/t/emacs-utf-8/21143/3
(add-to-list 'process-coding-system-alist 
             '("[rR][gG]" . (utf-8 . gbk-dos)))

(setq project-vc-extra-root-markers '(".project"))

(provide 'setup-extra)
