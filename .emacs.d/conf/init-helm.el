(require 'helm)
(require 'helm-config)

(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-scroll-amount 8
      )

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(helm-mode 1)
