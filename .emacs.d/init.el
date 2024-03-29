
;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
	:ensure t
	:init
	;; optional packages if you want to use :hydra, :el-get, :blackout,,,
	(leaf hydra :ensure t)
	(leaf el-get :ensure t)
	(leaf blackout :ensure t)

	:config
	;; initialize leaf-keywords.el
	(leaf-keywords-init)))

(leaf *built-in
  :config
  (leaf cus-edit
    :doc "tools for customizing Emacs and Lisp packages"
    :tag "builtin" "faces" "help"
    :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

  (leaf cus-start
    :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :preface
    (defun c/redraw-frame nil
      (interactive)
      (redraw-frame))
    :bind (("M-ESC ESC" . c/redraw-frame))
    :custom '((user-full-name . "Takumi ODAKA")
              (user-mail-address . "ap0.t.contact@gmail.com")
              (create-lockfiles . nil)
              (debug-on-error . t)
              (init-file-debug . t)
              (frame-resize-pixelwise . t)
              (enable-recursive-minibuffers . t)
              (history-length . 1000)
              (history-delete-duplicates . t)
              (scroll-preserve-screen-position . t)
              (scroll-conservatively . 100)
              (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
              (text-quoting-style . 'straight)
              (truncate-lines . t)
              (scroll-bar-mode . nil)
              (column-number-mode . t)
              (menu-bar-mode . nil)
              (tool-bar-mode . nil)
              (global-display-line-numbers-mode . t)
              (indent-tabs-mode . nil))
    :config
    (defalias 'yes-or-no-p 'y-or-n-p)
    (keyboard-translate ?\C-h ?\C-?))
              
  (leaf delsel
    :doc "delete selection if you insert"
    :tag "builtin"
    :global-minor-mode delete-selection-mode)

  (leaf paren
    :doc "highlight matching paren"
    :tag "builtin"
    :custom ((show-paren-delay . 0.1))
    :global-minor-mode show-paren-mode)

  (leaf files
    :doc "file input and output commands for Emacs"
    :tag "builtin"
    :custom `((auto-save-timeout . 15)
              (auto-save-interval . 60)
              (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backups/") t)))
              (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backups"))
                                          (,tramp-file-name-regexp . nil)))
              (version-control . t)
              (delete-old-versions . t)))

  (leaf startup
    :doc "process Emacs shell arguments"
    :tag "builtin" "internal"
    :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backups/.saves-")))))

(leaf key-settings
  :config
  (global-set-key (kbd "C-t") 'other-window)
  (global-set-key (kbd "C-u") 'undo))

(setq default-frame-alist '((font . "Menlo-15")))

(leaf *leaf
  :config
  (leaf leaf-convert
    :doc "Convert many format to leaf format"
    :req "emacs-26.1" "leaf-3.6.0" "leaf-keywords-1.1.0" "ppp-2.1"
    :tag "tools" "emacs>=26.1"
    :url "https://github.com/conao3/leaf-convert.el"
    :emacs>= 26.1
    :ensure t)
  (leaf leaf-tree
    :doc "Interactive side-bar feature for init.el using leaf"
    :req "emacs-25.1" "imenu-list-0.8"
    :tag "leaf" "convenience" "emacs>=25.1"
    :url "https://github.com/conao3/leaf-tree.el"
    :emacs>= 25.1
    :ensure t
    :bind ((lisp-mode-map
            ("C-c t" . leaf-tree-mode)))
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'right))))

(leaf doom-themes
  :custom ((doom-themes-enable-italic . t)
           (doom-themes-enable-bold . t))
  :ensure t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf doom-modeline
  :ensure t
  :require t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 3)
  (doom-modeline-height . 25)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-minor-modes . t)
  (doom-modeline-github . nil)
  (doom-modeline-mu4e . nil)
  (doom-modeline-irc . nil))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-virtual-buffers . t)
           (ivy-height . 30)
           (ivy-extra-directories . nil)
           (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
                                      (swiper . ivy--regex-plus)))
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper))
    :custom ((swiper-include-line-number-in-search . t)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf)
           ("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2020-12-01"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/yasnippets"))
  :global-minor-mode yas-global-mode)

(leaf neotree
  :doc "A tree plugin like NerdTree for Vim"
  :req "cl-lib-0.5"
  :added "2020-11-27"
  :url "https://github.com/jaypei/emacs-neotree"
  :ensure t
  :bind (("C-c n" . neotree-show)
         ("C-c h" . neotree-hide))
  :custom ((neo-theme . "ascii")
           (neo-persist-show . t)
           (neo-smart-open . t)))

(leaf highlight-indent-guides
  :doc "Minor mode to highlight indentation"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2020-12-02"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :emacs>= 24.1
  :ensure t
  :custom ((highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-method . 'character))
  :hook ((yaml-mode-hook . highlight-indent-guides-mode)
         (python-mode-hook . highlight-indent-guides-mode)
         (emacs-lisp-mode-hook . highlight-indent-guides-mode)
         (web-mode-hook . highlight-indent-guides-mode)))

(leaf git-gutter
  :bind (("C-c g" . hydra-git-gutter/body))
  :custom ((git-gutter:ask-p . nil)
           (git-gutter:modified-sign . "~")
           (git-gutter:added-sign . "+")
           (git-gutter:deleted-sign . "-"))
  :custom-face ((git-gutter:modified quote((t(:background "#f1fa8c"))))
                (git-gutter:added quote((t(:background "#50fa7b"))))
                (git-gutter:deleted quote((t(:background "#ff79c6")))))
  :global-minor-mode global-git-gutter-mode
  :config
  (defhydra hydra-git-gutter nil
    "git hunk"
    ("p" git-gutter:previous-hunk "previous")
    ("n" git-gutter:next-hunk "next")
    ("s" git-gutter:stage-hunk "stage")
    ("r" git-gutter:revert-hunk "revert")))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :hook ((python-mode-hook . lsp)
         (rust-mode-hook . lsp))
  :custom (lsp-rust-server 'rls)
  :config
  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2020-11-30"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode markdown-mode
    :custom ((lsp-ui-doc-enable . t)
             (lsp-ui-doc-header . t)
             (lsp-ui-doc-include-signature . t)
             (lsp-ui-doc-position . 'top)
             (lsp-ui-doc-max-width . 150)
             (lsp-ui-doc-max-height . 25)
             (lsp-ui-flycheck-enable . nil))))

(leaf *org
  :custom ((org-todo-keywords . '((sequence "TODO(t)" "IN_DRAFT(i)" "REMIND(r)" "|" "DONE(d)"))))
  :config
  (leaf org-agenda
    :doc "Dynamic task and appointment lists for Org"
    :tag "out-of-MELPA" "wp" "calendar" "hypermedia" "outlines"
    :url "https://orgmode.org"
    :bind (("C-c a a" . org-agenda)
           ("C-c a t" . org-todo-list)
           ("C-c a s" . org-tags-view))
    :custom ((org-agenda-files . '("~/Orgs/remind.org"
                                   "~/Orgs/project"))))
  (leaf org-capture
    :doc "Fast note taking in Org"
    :tag "out-of-MELPA" "wp" "calendar" "hypermedia" "outlines"
    :added "2020-12-08"
    :url "https://orgmode.org"
    :bind (("C-c a c" . org-capture))
    :custom ((org-capture-templates . '(("t" "Todo" entry (file+headline "~/Orgs/remind.org" "Capture") "* REMIND %? (wrote on %U)")))))
  (leaf org-pomodoro
    :doc "Pomodoro implementation for org-mode."
    :req "alert-0.5.10" "cl-lib-0.5"
    :added "2020-12-01"
    :url "https://github.com/lolownia/org-pomodoro"
    :ensure t
    :bind ((org-agenda-mode-map
            ("S-p" . org-pomodoro))))
  (leaf ox-gfm
    :doc "Github Flavored Markdown Back-End for Org Export Engine"
    :tag "github" "markdown" "wp" "org"
    :added "2021-09-17"
    :ensure t))

(leaf *webs
  :config
  (leaf web-mode
    :doc "major mode for editing web templates"
    :req "emacs-23.1"
    :tag "languages" "emacs>=23.1"
    :url "https://web-mode.org"
    :added "2022-03-16"
    :emacs>= 23.1
    :ensure t
    :mode "\\.js\\'" "\\.ts\\'" "\\.jsx\\'" "\\.tsx\\'"
    :config
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2))
  (prog1 'web-mode
    (autoload #'web-mode "web-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))
  (leaf emacs-prisma-mode
    :load-path "~/.emacs.d/elisp/emacs-prisma-mode/prisma-mode.el"
    :require t))

(leaf *python
  :config
  (leaf pyvenv
    :doc "Python virtual environment interface"
    :tag "tools" "virtualenv" "python"
    :url "http://github.com/jorgenschaefer/pyvenv"
    :added "2021-10-13"
    :ensure t)
  (leaf virtualenvwrapper
    :doc "a featureful virtualenv tool for Emacs"
    :req "dash-1.5.0" "s-1.6.1"
    :tag "virtualenvwrapper" "virtualenv" "python"
    :added "2021-04-01"
    :url "http://github.com/porterjamesj/virtualenvwrapper.el"
    :ensure t)
  (leaf auto-virtualenvwrapper
    :doc "Lightweight auto activate python virtualenvs"
    :req "cl-lib-0.6" "s-1.10.0" "virtualenvwrapper-0"
    :tag "tools" "virtualenv" "python"
    :url "http://github.com/jorgenschaefer/pyvenv"
    :added "2021-11-24"
    :ensure t)
  (leaf py-autopep8
    :doc "Use autopep8 to beautify a Python buffer"
    :url "http://paetzke.me/project/py-autopep8.el"
    :ensure t
    :hook (python-mode-hook . py-autopep8-enable-on-save)
    :setq ((py-autopep8-options quote
                                ("--max-line-length=120")))))

(leaf *yaml
  :config
  (leaf yaml-mode
    :doc "Major mode for editing YAML files"
    :req "emacs-24.1"
    :tag "yaml" "data" "emacs>=24.1"
    :added "2020-11-26"
    :emacs>= 24.1
    :ensure t))

(leaf *rust
  :config
  (leaf rust-mode
    :doc "A major-mode for editing Rust source code"
    :req "emacs-25.1"
    :tag "languages" "emacs>=25.1"
    :added "2021-09-30"
    :url "https://github.com/rust-lang/rust-mode"
    :emacs>= 25.1
    :ensure t
    :custom ((rust-format-on-save . t)))
  (leaf cargo
    :doc "Emacs Minor Mode for Cargo, Rust's Package Manager."
    :req "emacs-24.3" "markdown-mode-2.4"
    :tag "tools" "emacs>=24.3"
    :added "2021-09-30"
    :emacs>= 24.3
    :ensure t
    :hook ((rust-mode-hook . cargo-minor-mode))
    :after markdown-mode))

(leaf *golang
  :config
  (leaf go-mode
    :doc "Major mode for the Go programming language"
    :tag "go" "languages"
    :url "https://github.com/dominikh/go-mode.el"
    :added "2021-10-16"
    :ensure t)
  (leaf company-go
    :doc "company-mode backend for Go (using gocode)"
    :req "company-0.8.0" "go-mode-1.0.0"
    :tag "languages"
    :added "2021-10-16"
    :ensure t
    :after company go-mode))

(leaf *docker
  :config
  (leaf dockerfile-mode
    :doc "Major mode for editing Docker's Dockerfiles"
    :req "emacs-24"
    :tag "docker" "emacs>=24"
    :added "2021-10-12"
    :url "https://github.com/spotify/dockerfile-mode"
    :emacs>= 24
    :ensure t))

(leaf *terraform
  :config
  (leaf terraform-mode
    :doc "Major mode for terraform configuration file"
    :req "emacs-24.3" "hcl-mode-0.3" "dash-2.17.0"
    :tag "emacs>=24.3"
    :url "https://github.com/syohex/emacs-terraform-mode"
    :added "2022-01-05"
    :emacs>= 24.3
    :ensure t
    :after hcl-mode))

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
