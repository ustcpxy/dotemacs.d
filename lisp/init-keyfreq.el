;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'keyfreq)

(defun turnon-keyfreq-mode ()
  "Turn on keyfreq."
  (interactive)
  ;; Fire up keyfreq a few seconds later to start up emacs faster
  (my-run-with-idle-timer 4 (lambda ()
                               (keyfreq-mode 1)
                               (keyfreq-autosave-mode 1))))

(setq keyfreq-excluded-commands
      '(self-insert-command
        forward-char
        backward-char
        previous-line
        next-line
	keyfreq-show
	evil-next-line
	evil-previous-line
evil-forward-char
	right-char
	left-char
	scroll-up-command
	scroll-down-command
	org-self-insert-command
	org-delete-backward-char
	mwheel-scroll
	vertico-exit
	vertico-next
	vertico-directory-delete-char
	))

(my-write-to-missing-file "()" keyfreq-file)

(turnon-keyfreq-mode)

(provide 'init-keyfreq)
