;;; Commentary:
;;
;; blink search configurations.
;;

;;; Code:

(require 'blink-search)

(setq blink-search-common-directory '(("HOME" "~/")
                                      ("CONFIG" "~/.emacs.d/")
                                      ("REPO" "~/.emacs.d/site-lisp/")
                                      ))

(setq blink-search-grep-pdf-search-paths "/data/Book")
(setq blink-search-grep-pdf-backend 'eaf-pdf-viewer)

;; (setq blink-search-enable-posframe t)

(provide 'init-blink-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-kill-ring.el ends here
