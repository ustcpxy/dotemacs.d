;;; init-ui.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Basic UI Configuration
(setq inhibit-startup-screen t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Theme
;; This prevents Emacs from asking if it is safe to load the theme.
(setq custom-safe-themes t)
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; Fonts
;; copied from centaur's emacs
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(when (display-graphic-p)
  ;; Set default font
  ;; (cl-loop for font in '("monospace" "SF Mono" "Hack" "Source Code Pro" "Fira Code"
  ;;                        "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
  ;;          when (font-installed-p font)
  ;;          return (set-face-attribute 'default nil :font font))
  (set-face-attribute 'default nil :font (font-spec :family "monospace" :size 16))

  ;; Specify font for all unicode characters
  ;; (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
  ;;          when (font-installed-p font)
  ;;          return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  ;;(cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
  ;;         when (font-installed-p font)
  ;;         return (set-fontset-font t '(#x4e00 . #x9fff) font))
  )

(set-fontset-font t 'han "WenQuanYi Micro Hei")
;;; bettor modeline
;; NOTE: The first time you load your configuration on a new machine,
;; you’ll need to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;; which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(defvar +popup-mode-map (make-sparse-keymap)
  "Active keymap in a session with the popup system enabled. See
`+popup-mode'.")

(defvar +popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
      ;; For maximum escape coverage in emacs state buffers; this only works in
      ;; GUI Emacs, in tty Emacs use C-g instead
      (define-key map (kbd "<escape>") #'keyboard-escape-quit)
    map)
  "Active keymap in popup windows. See `+popup-buffer-mode'.")

(define-minor-mode +popup-mode
  "Global minor mode representing Doom's popup management system."
  :init-value nil
  :global t
  :keymap +popup-mode-map
  ;; (cond (+popup-mode
  ;;        (add-hook 'doom-escape-hook #'+popup-close-on-escape-h 'append)
  ;;        (setq +popup--old-display-buffer-alist display-buffer-alist
  ;;              display-buffer-alist +popup--display-buffer-alist
  ;;              window--sides-inhibit-check t)
  ;;        (dolist (prop +popup-window-parameters)
  ;;          (push (cons prop 'writable) window-persistent-parameters)))
  ;;       (t
  ;;        (remove-hook 'doom-escape-hook #'+popup-close-on-escape-h)
  ;;        (setq display-buffer-alist +popup--old-display-buffer-alist
  ;;              window--sides-inhibit-check nil)
  ;;        (+popup-cleanup-rules-h)
  ;;        (dolist (prop +popup-window-parameters)
  ;;          (delq (assq prop window-persistent-parameters)
  ;;                window-persistent-parameters)))))
  )

(define-minor-mode +popup-buffer-mode
  "Minor mode for individual popup windows.

It is enabled when a buffer is displayed in a popup window and disabled when
that window has been changed or closed."
  :init-value nil
  :keymap +popup-buffer-mode-map
  ;; (if (not +popup-buffer-mode)
  ;;     (remove-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h t)
  ;;   (add-hook 'after-change-major-mode-hook #'+popup-set-modeline-on-enable-h
  ;;             nil 'local)
  ;;   (when (timerp +popup--timer)
  ;;     (remove-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h t)
  ;;     (cancel-timer +popup--timer)
  ;;     (setq +popup--timer nil))))
)
(put '+popup-buffer-mode 'permanent-local t)
(put '+popup-buffer-mode 'permanent-local-hook t)
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)

(provide 'init-ui)
