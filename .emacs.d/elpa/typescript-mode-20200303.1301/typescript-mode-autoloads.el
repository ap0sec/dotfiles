;;; typescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "typescript-mode" "typescript-mode.el" (24159
;;;;;;  22696 812297 735000))
;;; Generated autoloads from typescript-mode.el
(put 'typescript-indent-level 'safe-local-variable #'integerp)

(autoload 'typescript-mode "typescript-mode" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

;;;***

;;;### (autoloads nil nil ("typescript-mode-pkg.el" "typescript-mode-test-utilities.el")
;;;;;;  (24159 22696 800066 438000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; typescript-mode-autoloads.el ends here
