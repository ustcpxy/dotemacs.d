;; 优化启动速度
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))


(setq native-comp-async-report-warnings-errors nil)

;; @https://emacs-china.org/t/emacs-utf-8/21143/28
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")
