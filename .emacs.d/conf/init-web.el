(require 'web-mode)

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

;; インデント
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 4)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
	     (setq web-mode-tag-auto-close-style 2)
	     ))


;; (defun web-mode-indent (num)
;;   (interactive "nIndent: ")
;;   (setq web-mode-markup-indent-offset num)
;;   (setq web-mode-css-indent-offset num)
;;   (setq web-mode-style-padding num)
;;   (setq web-mode-code-indent-offset num)
;;   (setq web-mode-script-padding num)
;;   (setq web-mode-block-padding num)
;;   )
;; (web-mode-indent 4)
