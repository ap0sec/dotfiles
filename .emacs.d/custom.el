(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))
 '(auto-save-interval 60)
 '(auto-save-list-file-prefix "~/.emacs.d/backups/.saves-")
 '(auto-save-timeout 15)
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups") ("\\`/[^/:]+:[^/:]*:")))
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-file "~/.emacs.d/custom.el")
 '(debug-on-error t)
 '(delete-old-versions t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(enable-recursive-minibuffers t)
 '(frame-resize-pixelwise t)
 '(global-display-line-numbers-mode t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(imenu-list-position 'right t)
 '(imenu-list-size 30 t)
 '(indent-tabs-mode nil)
 '(init-file-debug t t)
 '(menu-bar-mode t)
 '(mouse-wheel-scroll-amount '(1 ((control) . 5)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(unicode-fonts zenburn-theme yaml-mode use-package py-autopep8 powerline neotree lsp-ui lsp-jedi leaf-tree leaf-convert hydra git-gutter el-get doom-themes doom-modeline counsel company blackout))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100)
 '(scroll-preserve-screen-position t)
 '(show-paren-delay 0.1)
 '(text-quoting-style 'straight)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(user-full-name "Takumi ODAKA")
 '(user-mail-address "ap0.t.contact@gmail.com")
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4"))) nil "Customized with leaf in doom-themes block"))
