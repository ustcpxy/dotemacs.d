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
  ;; (load-theme 'doom-solarized-light)
  )
  ;; (load-theme 'modus-operandi)
  ;; (load-theme 'modus-vivendi)

;; 随机选择主题
;; 亮色主题和暗色主题
;; (setq day-theme-list '( modus-operandi ef-day ef-spring ef-summer))
;; (setq dark-theme-list '( modus-vivendi ef-night ef-autumn ef-winter))
(setq day-theme-list '( modus-operandi ))
(setq dark-theme-list '( modus-vivendi ))

;; 随机选取主题
(defun my-random-element (my-list)
  "Return a random element from MY-LIST."
  (let ((my-length (length my-list))
        (my-random-index (random (length my-list))))
    (nth my-random-index my-list)))

;; 根据时间选择亮/暗主题
(defun synchronize-theme ()
  (setq hour
	(string-to-number
	 (substring (current-time-string) 11 13)))  ;; 获取小时，24小时制
  (if (member hour (number-sequence 6 17))  ;; 判断时间在早上6点到下午6点
      (progn
	(setq now (my-random-element day-theme-list))
	(setq lst (my-random-element dark-theme-list))
	)
    (setq now (my-random-element dark-theme-list))
    (setq lst (my-random-element day-theme-list))
    )
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme now t))
(synchronize-theme) ;; 启动时立即执行一次
;; 每小时执行一次
(let* ((current-minutes
	(string-to-number (substring (current-time-string) 14 16)))
       (current-seconds
	(string-to-number (substring (current-time-string) 17 20)))
       (remain-seconds
	;; remaining seconds = 3600 - 60 * min - sec
	(- 3600 (* 60 current-minutes) current-seconds))
       )
  (run-with-timer remain-seconds 3600 'synchronize-theme))


;;; Fonts
;; copied from centaur's emacs
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
;;(when (display-graphic-p)
  ;; Set default font
  ;; (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
  ;;                        "SF Mono" "Hack" "Source Code Pro" "Menlo"
  ;;                        "Monaco" "DejaVu Sans Mono" "Consolas")
  ;;          when (font-installed-p font)
  ;;          return (set-face-attribute 'default nil :font font))
  ;; font family candidate ("DejaVu Sans Mono" "Source Code Pro")
  ;; (set-face-attribute 'default nil :font (font-spec :family "monospace" :size 16))
  ;;(set-face-attribute 'default nil :font (font-spec :family "DejaVu Sans Mono") :height 150)
;;  (set-face-attribute 'default nil :font (font-spec :family "Microsoft Yahei") :height 150)
  
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
;; (if (eq system-type 'windows-nt)
;;  (set-face-attribute 'default nil :font (font-spec :family "Microsoft Yahei") :height 150)

;;    (set-fontset-font t 'han "WenQuanYi Micro Hei"))
;; )

;; copied @https://emacs-china.org/t/emacs/22193
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 150))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
             when (font-installed-p font)
             return (if (>= emacs-major-version 28)
                        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-="   . default-text-scale-increase)
         ("s--"   . default-text-scale-decrease)
         ("s-0"   . default-text-scale-reset)
         ))

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
