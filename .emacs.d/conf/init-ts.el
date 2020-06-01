(require 'typescript-mode)

;; インデント
(add-hook 'typescript-mode-hook
	  (lambda ()
	    (setq-default typescript-indent-level 2)
	    ))

(add-hook 'typescript-mode-hook
	  (lambda ()
    (add-hook 'after-save-hook 'prettier-reload t t)))
