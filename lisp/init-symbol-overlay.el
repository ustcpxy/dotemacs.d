;;; Commentary:
;;
;; symbol-oeverlay configurations.
;;

;;; Code:

(require 'symbol-overlay)

(require 'symbol-overlay)
(global-set-key (kbd "<f8>") 'symbol-overlay-put)
;; (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
;; (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)

(global-set-key (kbd "<f7>") 'symbol-overlay-remove-all)

(provide 'init-symbol-overlay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-kill-ring.el ends here
