;;; init-treesit.el -*- lexical-binding: t no-byte-compile: t -*-

;;; Require
(require 'treesit)

;;; Code:

;; M-x `treesit-install-language-grammar` to install language grammar.
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

;; (setq major-mode-remap-alist
;;       '((c-mode          . c-ts-mode)
;;         (c++-mode        . c++-ts-mode)
;;         (cmake-mode      . cmake-ts-mode)
;;         (conf-toml-mode  . toml-ts-mode)
;;         (css-mode        . css-ts-mode)
;;         (js-mode         . js-ts-mode)
;;         (js-json-mode    . json-ts-mode)
;;         (python-mode     . python-ts-mode)
;;         (sh-mode         . bash-ts-mode)
;;         (typescript-mode . typescript-ts-mode)))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))


(provide 'init-treesit)
