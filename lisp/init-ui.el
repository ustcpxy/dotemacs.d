
;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; disable all bar mode
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; Theme
;; This prevents Emacs from asking if it is safe to load the theme.
(setq custom-safe-themes t)
(load-theme 'modus-vivendi)
;; (load-theme 'doom-molokai)


;; specify font for chinese characters
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
         when (font-installed-p font)
         return (set-fontset-font t '(#x4e00 . #x9fff) font))

(provide 'init-ui)
