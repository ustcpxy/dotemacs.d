;;; init-incremental-narrowing.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Description
;;  yet another solution for incremental narrowing in Emacs
;;  selectrum +
;;
;;; Code:

;;; Preserve Minibuffer History with savehist-mode
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;;; Improved Candidate Filtering with Orderless
(use-package orderless)

;;; Completions with icomplete
(use-package icomplete-vertical
  :demand t
  :after orderless
  :bind (:map icomplete-minibuffer-map
              ("C-j"   . icomplete-forward-completions)
              ("C-k"   . icomplete-backward-completions)
              ("C-f"   . icomplete-force-complete-and-exit)
              ("C-M-f" . icomplete-force-complete)
              ("TAB"   . icomplete-force-complete)
              ("RET"   . icomplete-force-complete-and-exit)
              ("M-h"   . backward-kill-word))
  :custom
  (completion-styles '(orderless partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-cycling t)
  (completion-cycle-threshold 5)
  (icomplete-compute-delay 0.1)
  (icomplete-vertical-prospects-height 7)
   :custom-face
  (icomplete-first-match ((t (:foreground "LightGreen" :weight bold))))
  :config
  ;; Deal with a weird issue where the minibuffer disappears
  ;; in some cases when resize-mini-windows isn't nil
  (setq resize-mini-windows nil)

  ;; Enable icomplete and vertical completions
  (icomplete-mode)
  (icomplete-vertical-mode))

;; (use-package restricto
;;   :straight t
;;   :after selectrum
;;   :demand t
;;   :bind (:map minibuffer-local-completion-map
;;          ("S-SPC" . restricto-narrow))
;;   :config
;;   (restricto-mode))

;;; Consult Commands
(defun my/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . consult-buffer))
  :custom
  (consult-project-root-function #'my/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  )

;;; Completion Annotations with Marginalia
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  ;; Ensure that Selectrum is refreshed when cycling annotations.
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after (lambda () (selectrum-exhibit))))

;;; Completion Actions with Embark
(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config
  ;; ---- Selectrum only ----
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  ;; (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
            ;; Pass relative file names for dired.
            minibuffer-completing-file-name))))

  ;; No unnecessary computation delay after injection.
  ;; (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  ;; (add-hook 'embark-candidate-collectors #'current-candidates+category))

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)

  ;; Use this for icomplete
  (add-hook 'embark-pre-action-hook #'completion--flush-all-sorted-completions))

;;; Launching apps
;; (use-package app-launcher)
  ;; :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))
(autoload 'ffap-file-at-point "ffap")

(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn
                             (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table)))) 'append)
(setq file-name-shadow-properties
      '(invisible t))
(use-package selectrum
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 7)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1))

(provide 'init-selectrum)
;;; init-selectrum.el ends here
