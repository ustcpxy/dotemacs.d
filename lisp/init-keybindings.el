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
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; (global-set-key (kbd "\e\e a") 'org-agenda)

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f9>") 'org-capture)
;; (global-set-key (kbd "<f8>") 'org-capture-finalize)
(global-set-key (kbd "<f5>") 'org-refile)
(global-set-key (kbd "M-<f9>") 'org-capture-refile)
(global-set-key (kbd "M-<f8>") 'org-capture-kill)
(global-set-key (kbd "M-<f12>") 'pop-to-org-agenda-with-next-actions)

(global-set-key (kbd "M-i") 'my/imenu)
(global-set-key (kbd "M-`") 'consult-line)
(global-set-key (kbd "M-m") 'evilmi-jump-items-native)
(global-set-key (kbd "M-q") 'save-buffers-kill-emacs)

;; window access
;; (global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-1") 'toggle-one-window)
(global-set-key (kbd "M-2") 'split-window-right)

;; search
(global-set-key (kbd "M-/") 'my/search-other-project)
(global-set-key (kbd "M-s") 'consult-ripgrep-thing-at-point)
(global-set-key (kbd "C-o") 'open-newline-above)
(global-set-key (kbd "C-l") 'open-newline-below)
(global-set-key (kbd "C-M-o") 'duplicate-line-or-region-above)
(global-set-key (kbd "C-M-l") 'duplicate-line-or-region-below)

;; high frequently keybindings
(global-set-key (kbd "M-o") 'project-switch-project)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "ESC <down>") 'end-of-buffer)
(global-set-key (kbd "M-u") 'undo)

(global-set-key (kbd "M-[") 'beginning-of-defun)
(global-set-key (kbd "M-]") 'end-of-defun)

(provide 'init-keybindings)
