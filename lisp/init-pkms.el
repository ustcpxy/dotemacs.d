;;; init-pkms.el --- setup org-roam -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; for notes taking
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "/home/derek/notesdb/"))
  (org-roam-dailies-directory "journals/")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-db-location (concat org-roam-directory ".org-roam.db"))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "pages/${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n")
           :unnarrowed t)))

  (setq time-stamp-active t
        time-stamp-start "^#\\+last_modified: [ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp nil)

  (defun my/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'my/tag-new-node-as-draft)

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'init-pkms)
;;; init-pkms.el ends here
