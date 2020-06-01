
;;#init.el#;;

;;##############################

;;パス管理

;;ロードパス追加用関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;;ロードパス追加
(add-to-load-path "elisp" "public_repos" "conf")

;;##############################

;;ユーザビリティ

;;キーバインド
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x p") 'prettier-reload)


;;フレーム設定
(column-number-mode t)
(global-linum-mode t)
(menu-bar-mode -1)

;;バックアップファイル設定
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/") t)))

;;カラーテーマ
(load-theme 'tsdh-dark t)

;;##############################

;;拡張子設定
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))
;;##############################

;;パッケージ管理関連

;;有効化
(require 'package) ; package.el
;;;marmaladeリポジトリの追加
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
;;;MELPAリポジトリの追加
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
;;;orgリポジトリの追加
(add-to-list
 'package-archives
 '("org" . "http://orgmode.org/elpa/"))


;;パッケージ読み込み
(package-initialize)

;;##############################

;;カスタムファイル設定
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;コンフィグファイル設定
(load "init-company")
(load "init-helm")
(load "init-org")
(load "init-rjsx")
(load "init-js")
(load "init-web")
(load "prettier")

(when (eq system-type 'darwin)
  (load "init-osx"))

;;##############################

(setq markdown-command-needs-filename t)
(setq markdown-command "pandoc")
