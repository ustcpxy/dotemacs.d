;;;  -*- lexical-binding: t; -*-

;; copied from doom-emacs - core-keybinds.el
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'doom/escape)


(global-set-key (kbd "M-i") 'my/imenu)
(global-set-key (kbd "M-`") 'consult-line)


(use-package which-key
  :hook (after-init . which-key-mode)
  :ensure t
  :init
  (setq which-key-side-window-location 'bottom))

(use-package general
  :init
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                      nil
                      nil
                      t))

  (general-emacs-define-key 'global [remap imenu] 'consult-imenu)
  (general-emacs-define-key 'global [remap apropos] 'consult-apropos)

  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (global-definer
   "!" 'shell-command
   ":" 'eval-expression
   "SPC" 'avy-goto-word-or-subword-1
   "TAB" 'spacemacs/alternate-buffer
   "x" 'switch-to-scratch-buffer
   "*" 'consult-ripgrep-thing-at-point
   "/" 'consult-ripgrep

   "'" 'vertico-repeat
   "=" 'indent-buffer
   "u" 'universal-argument
   "v" 'er/expand-region
   "0" 'select-window-0
   "1" 'select-window-1
   "2" 'select-window-2
   "3" 'select-window-3
   ";" 'vterm
   "`" 'multi-vterm-project
   "hf" 'describe-function
   "hv" 'describe-variable
   "hk" 'describe-key
   "qq" 'save-buffers-kill-terminal
   "qR" 'restart-emacs
   "hh" 'zilongshanren/highlight-dwim
   "hc" 'zilongshanren/clearn-highlight
   "i" 'my/imenu
   "en" 'my-goto-next-error
   "ry" 'consult-yank-pop
   "R" 'zilongshanren/run-current-file
   "ep" 'my-goto-previous-error
   "el" 'my-list-errors
   "sp" 'consult-ripgrep
   "oy" 'youdao-dictionary-search-at-point+
   "oo" 'zilongshanren/hotspots
   "or" 'org-roam-node-find
   "gs" 'magit-status
   "gd" 'vc-diff
   "gg" 'xref-find-definitions
   "gr" 'xref-find-references)


  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map ',(intern (concat "+general-global-" name "-map"))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  (+general-global-menu! "buffer" "b"
                         "d" 'kill-current-buffer
                         "b" '(switch-to-buffer :which-key "switch buffer")
                         "B" '(consult-buffer :which-key "consult buffer")
                         "p" 'previous-buffer
                         "R" 'rename-buffer
                         "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
                               :which-key "messages-buffer")
                         "n" 'next-buffer
                         "i" 'ibuffer
                         "f" 'my-open-current-directory
                         "k" 'kill-buffer
                         "y" 'copy-buffer-name
                         "K" 'kill-other-buffers
                         )

  (+general-global-menu! "layout" "l"
                         "l" 'persp-switch
                         "L" 'persp-state-load
                         "TAB" 'persp-switch-last
                         "A" 'persp-add-buffer
                         "s" 'persp-state-save
                         "b" 'persp-switch-to-buffer
                         "R" 'persp-remove-buffer
                         "k" 'persp-kill)

  (+general-global-menu! "file" "f"
                         "f" 'find-file
                         "r" 'consult-recent-file
                         "L" 'consult-locate
                         "d" 'consult-dir
                         "ed" 'open-my-init-file
                         "s" 'save-buffer
                         "w" 'sudo-edit
                         "S" 'save-some-buffers
                         "j"  'dired-jump
                         "y" 'copy-file-name
                         "R" 'my/rename-current-buffer-file
                         "k" 'my/delete-file-and-buffer
                         "!" 'my/exec-shell-on-buffer)

  (+general-global-menu! "window" "w"
                         "/" 'split-window-right
                         "-" 'split-window-below
                         "m" 'delete-other-windows
                         "u" 'winner-undo
                         "z" 'winner-redo)

  (+general-global-menu! "toggle" "t"
                         "s" 'flycheck-mode
                         "S" 'flyspell-prog-mode
                         "e" 'toggle-corfu-english-helper
                         "r" 'read-only-mode
                         "n" 'my-toggle-line-numbber
                         "w" 'distraction-free
                         "l" 'my/enable-lsp-bridge
                         "k" '+toggle-keycast
                         "c" 'global-corfu-mode
                         "m" 'consult-minor-mode-menu)

  (+general-global-menu! "project" "p"
                         "f" 'project-find-file
                         "o" 'ff-find-other-file
                         "r" 'consult-recent-file
                         "s" 'project-find-regexp
                         "d" 'project-dired
                         "b" 'consult-project-buffer
                         "e" 'project-eshell
                         "m" 'my/project-run-makefile-target
                         "c" 'project-compile
                         "t" 'my/project-citre
                         "p" 'project-switch-project
                         "i" 'my/project-info
                         "a" 'project-remember-projects-under
                         "x" 'project-forget-project)

  (+general-global-menu! "notes" "n"
    "f" 'org-roam-node-find
    "i" 'org-roam-node-insert
    "l" 'org-roam-buffer-toggle
    "c" 'org-roam-capture
    "j" 'org-roam-dailies-capture-today
                         )
  (general-create-definer global-leader
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ","
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  ;; mode specific major key
  (global-leader
   :major-modes
   '(org-mode t)
   ;;and the keymaps:
   :keymaps
   '(org-mode-map)
   "p" 'org-pomodoro
   "t" 'org-todo
   "e" 'org-set-effort
   ">" 'org-metaright
   "<" 'org-metaleft
   "J" 'org-metadown
   "K" 'org-metaup
   "T" 'org-set-tags-command
   "l" 'org-toggle-link-display
   "L" 'org-toggle-inline-images
   "I" 'org-clock-in
   "O" 'org-clock-out
   "P" 'org-set-property
   "s" 'org-schedule
   "+" 'org-increase-number-at-point
   "-" 'org-decrease-number-at-point
   "n" 'org-narrow-to-subtree
   "dc" 'org-download-clipboard
   "ds" 'org-download-screenshot
   "w" 'widen)

  (global-leader
   :major-modes
   '(org-agenda-mode t)
   ;;and the keymaps:
   :keymaps
   '(org-agenda-mode-map)
   "d" 'org-agenda-day-view
   "w" 'org-agenda-week-view
   "," 'org-agenda-priority
   "e" 'org-agenda-set-effort
   ":" 'org-agenda-set-tags
   "T" 'org-agenda-show-tags)

  (global-leader
   :major-modes
   '(python-mode t)
   ;;and the keymaps:
   :keymaps
   '(python-mode-map)
   "e" 'live-py-set-version)

  )

(provide 'init-keybindings)