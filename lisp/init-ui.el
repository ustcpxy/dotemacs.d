;;; init-ui.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Basic UI Configuration
(setq inhibit-startup-screen t)
(setq-default cursor-type 'bar)

;; auto fullscreen
(setq  initial-frame-alist (quote ((fullscreen . maximized))))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

(global-hl-line-mode t)
(column-number-mode)

(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

(setq select-enable-primary t)


;;; Theme
;; This prevents Emacs from asking if it is safe to load the theme.
(setq custom-safe-themes t)
(use-package doom-themes
  :init
  (if (display-graphic-p)
      (load-theme 'doom-solarized-light)
    (load-theme 'doom-tomorrow-night))
  )

;;; Fonts
;; copied from centaur's emacs
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(when (display-graphic-p)
  ;; Set default font
  ;; (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
  ;;                        "SF Mono" "Hack" "Source Code Pro" "Menlo"
  ;;                        "Monaco" "DejaVu Sans Mono" "Consolas")
  ;;          when (font-installed-p font)
  ;;          return (set-face-attribute 'default nil :font font))
  ;; (set-face-attribute 'default nil :font (font-spec :family "monospace" :size 16))
  (set-face-attribute 'default nil :font (font-spec :family "Source Code Pro" :size 18))

  ;; Specify font for all unicode characters
  ;; (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
  ;;          when (font-installed-p font)
  ;;          return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Emoji
  ;; (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
  ;;          when (font-installed-p font)
  ;;          return (cond
  ;;                  ((< emacs-major-version 27)
  ;;                   (set-fontset-font
  ;;                    "fontset-default" 'unicode font nil 'prepend))
  ;;                  ((< emacs-major-version 28)
  ;;                   (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
  ;;                  (t
  ;;                   (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

  ;; Specify font for Chinese characters
  ;; (cl-loop for font in '("WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
  ;;          when (font-installed-p font)
  ;;          return (progn
  ;;                   (setq face-font-rescale-alist `((,font . 1.3)))
  ;;                   (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))

(set-fontset-font t 'han "WenQuanYi Micro Hei")
)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-="   . default-text-scale-increase)
         ("s--"   . default-text-scale-decrease)
         ("s-0"   . default-text-scale-reset)
         ("C-s-=" . default-text-scale-increase)
         ("C-s--" . default-text-scale-decrease)
         ("C-s-0" . default-text-scale-reset)))

;;; bettor modeline
;; NOTE: The first time you load your configuration on a new machine,
;; you’ll need to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes t)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :hook (after-init . doom-modeline-mode))

(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))

(provide 'init-ui)
