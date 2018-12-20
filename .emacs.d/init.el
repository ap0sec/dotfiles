
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

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;フレーム設定
(column-number-mode t)
(global-linum-mode t)

;;インデント設定
(setq-default indent-tabs-mode nil)

;;バックアップファイル設定
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/") t)))

;;##############################

;;拡張子設定

;;.mdをmarkdownモードで開く
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;;##############################

;;テーマ関連
(setq custom-theme-directory "~/.emacs.d/elisp/themes/")
(load-theme 'euphoria t)

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

;;コンフィグファイル設定

;;カスタムファイル設定
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;各種設定ファイル読み込み
(load "init-company")
(load "init-helm")
(load "init-org")
(load "init-rust")

(require 'ox-qmd)

;;##############################

(setq markdown-command-needs-filename t)
(setq markdown-command "pandoc")
