(push "~/emacs" load-path)

(add-to-list 'load-path "~/emacs/yasnippet")
(require 'yasnippet)
(setq yas/root-directory "~/emacs/yasnippet/snippets")
(yas/load-directory yas/root-directory)

;; untabify and delete trailing whitespace
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
            (untabify (point-min) (point-max))) t nil)

(when (equal system-type 'darwin)
  (setenv "PATH" (concat (expand-file-name "~/bin") ":"
                         (expand-file-name "~/apps/node/bin") ":"
                         (expand-file-name "~/apps/scala/bin") ":"
                         (expand-file-name "~/apps/groovy/bin") ":"
                         (expand-file-name "~/apps/jruby/bin") ":"
                         "/usr/local/bin" ":"
                         "/opt/local/bin" ":"
                         (getenv "PATH")))

  (setenv "DYLD_FALLBACK_LIBRARY_PATH" "/usr/lib:/opt/local/lib:/usr/X11R6/lib:/Users/agleyzer/apps/oracle/product/10.2.0/client_1/lib")

  (setenv "ORACLE_HOME" "/Users/agleyzer/apps/oracle/product/10.2.0/client_1")

  (push "/opt/local/bin" exec-path)
  (push "/usr/local/bin" exec-path)
  (push (expand-file-name "~/bin") exec-path)
  (push (expand-file-name "~/apps/node/bin") exec-path)
  (push (expand-file-name "~/apps/scala/bin") exec-path)
  (push (expand-file-name "~/apps/groovy/bin") exec-path)
  (push (expand-file-name "~/apps/jruby/bin") exec-path)

  (setenv "MANPATH" (concat "/usr/local/man" ":"
                            "/opt/local/man" ":"
                            (getenv "MANPATH")))

  ;; (set-default-font "-apple-consolas-medium-r-normal--22-160-*-*-*-*-*-*")
)

;; No stupid questions
(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-shell ()
  (interactive)
  (delete-other-windows)
  (split-window)
  (other-window 1)
  (shell))

;; Set some keys - that's the way I like it :)
(global-set-key "\M-s" (quote my-shell))
(global-set-key "\M-g" (quote goto-line))
(global-set-key (quote [f1]) (quote manual-entry))
(global-set-key (quote [C-tab]) (quote other-window))
(global-set-key (quote [home]) (quote beginning-of-line))
(global-set-key (quote [end]) (quote end-of-line))
;; aquamacs version
(define-key osx-key-mode-map [home] 'beginning-of-line)
(define-key osx-key-mode-map [end] 'end-of-line)

(global-set-key (quote [C-return]) (quote hippie-expand))
(global-set-key (quote [kp-delete]) (quote delete-char))

(defun my-flymake-goto-next-error ()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

(defun my-flymake-goto-prev-error ()
  (interactive)
  (flymake-goto-prev-error)
  (flymake-display-err-menu-for-current-line))

(global-set-key (quote [s-down]) (quote my-flymake-goto-next-error))
(global-set-key (quote [s-up]) (quote my-flymake-goto-prev-error))


;; DOS - UNIX CR/LF conversions
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching


;; start server
(server-start)

;; scala mode
(add-to-list 'load-path (expand-file-name "~/emacs/ensime/dist/elisp"))
(add-to-list 'load-path (expand-file-name "~/emacs/scamacs/src/elisp/scala"))
;; (add-to-list 'load-path (expand-file-name "~/emacs/scamacs/src/elisp/sbt"))
;; (add-to-list 'load-path (expand-file-name "~/emacs/scamacs/src/elisp/ecb"))

(require 'scala-mode-auto)
(require 'ensime)
;; (require 'sbt)

(require 'cqmirror)

(add-hook 'scala-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)

             ;; revert stupid scala mode mapping
             (define-key scala-mode-map [(control tab)] 'other-window)
             (define-key scala-mode-map [f2] 'ensime-sbt-switch)
             (define-key scala-mode-map [f3]
               (lambda ()
                 (interactive)
                 (progn (ensime-sbt-switch)
                        (ensime-sbt-action "run"))))

             (define-key scala-mode-map [f4]
               (lambda ()
                 (interactive)
                 (ensime-sbt-switch)))

             (ensime-scala-mode-hook)
             ;; (yas/minor-mode-on)
             ))

;; ;; scala build tool - buggy

;; (load "sbt")
;; (defun me-turn-off-indent-tabs-mode ()
;;  (setq indent-tabs-mode nil))
;; (add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)

;; ensime
;; (add-to-list 'load-path (expand-file-name "~/emacs/ensime/elisp/"))
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; javascript mode
;; (autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(require 'markdown-mode)

(load (expand-file-name "~/emacs/nxhtml/autostart.el"))

(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; (require 'real-auto-save)
;; (setq real-auto-save-interval 5) ;; in seconds

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(require 'flymake-jslint-local)

(require 'unit-test)
(define-key global-map [f3] 'run-unit-tests)
(define-key global-map [f4] 'open-unit-test-file)


(define-key comint-mode-map [f4]
  (lambda ()
    (interactive)
    ;; (switch-to-buffer-other-window (other-buffer))))
    (delete-window)))
    ;;(other-window 1)
    ;;(delete-other-windows)))


(define-key comint-mode-map [f8]
  (lambda ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (comint-send-input t))))

;; toggle all lines that are indented deeper than the current one
(defun my-toggle-selective-display ()
  (interactive)

  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "[:blank:]")
    (set-selective-display (if selective-display nil (+ (current-column) 1)))))

(global-set-key [f8] 'my-toggle-selective-display)


(defun my-js-unit-test-command ()
  (interactive)
  (let* ((test-file (my-js-unit-test-file buffer-file-name)))
    (if test-file
        (zerop (shell-command (concat "node " test-file) "*unit-test-output*" "*unit-test-output*"))
      (progn
        (message "no unit test for %s" buffer-file-name)
        nil))))

(defun my-js-compile-run-unit-test ()
  ;; use compile mode to run node with test file or buffer file
  (interactive)
  (compile (concat "node "
                   (let ((test-file (my-js-unit-test-file buffer-file-name)))
                     (if test-file
                         test-file
                       buffer-file-name)))))

(defun my-js-unit-test-file (file)
  (interactive "f")
  (if (my-js-is-unit-test-file file)
      file
    (let ((test-file (concat (file-name-sans-extension file) "_test.js")))
      (if (file-exists-p test-file) test-file nil))))

(defun my-js-is-unit-test-file (file)
  (interactive "f")
  (integerp (string-match ".+_test.js" file)))

;; compilation regexp for jslint & node.js
(add-to-list 'compilation-error-regexp-alist '("^[ \t]*at \\([a-zA-Z0-9\\/\\.\\-]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist '("^[ \t]*at .+ (\\([a-zA-Z0-9\\/\\.\\-]+\\):\\([0-9]+\\):\\([0-9]+\\))$" 1 2 3))

(defun my-js-mode-hook ()
  ;; espresso mode overrides standard M-., I want it back.
  (define-key js-mode-map [(meta ?.)] #'find-tag)

  (define-key js-mode-map [f2] 'my-js-compile-run-unit-test)

  (define-key js-mode-map [M-up] 'beginning-of-defun)
  (define-key js-mode-map [M-down] 'end-of-defun)

  ;; (turn-on-real-auto-save)
  (flymake-mode t)
  (setq unit-test-command 'my-js-unit-test-command)
  (setq unit-test-file-fn 'my-js-unit-test-file)
  (add-hook 'before-save-hook
            (lambda ()
              (untabify (point-min) (point-max))) t t)
  (add-hook 'after-save-hook 'run-unit-tests t t)

  ;; do not expand if we're in the middle of a word
  (setq yas/buffer-local-condition
        '(not (char-equal (char-syntax (char-after (point))) ?w)))

  (yas/minor-mode))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; groovy mode
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(autoload 'groovy-mode "groovy" nil t)

(defun my-find-interpreter (full-file-name)
  (let* ((file (file-name-nondirectory full-file-name))
         (extension (file-name-extension file))
         (interpreter (cond
                       ((string= "scala" extension) "fsc")
                       ((string= "c" extension) "gcc")
                       ((string= "pl" extension) "perl")
                       ((string= "py" extension) "python")
                       ((string= "groovy" extension) "groovy")
                       ((string= "js" extension) "node"))))
    (if interpreter (concat interpreter " \"" full-file-name "\""))))

(defun my-compile ()
  (interactive)
  (progn
    (save-buffer)
    (if (local-variable-p 'compile-command) (recompile)
      (compile (or (my-find-interpreter (buffer-name))
                   "make -k")))))

(define-key global-map [f2] 'my-compile)

;; Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-some-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and (not (string-equal name ""))
           (not (string-equal name "*Messages*"))
          ;; (not (string-equal name "*Buffer List*"))
           (not (string-equal name "*buffer-selection*"))
           (not (string-equal name "*Shell Command Output*"))
           (not (string-equal name "*scratch*"))
           (/= (aref name 0) ? )
           (if (buffer-modified-p buffer)
               (if (yes-or-no-p
                    (format "Buffer %s has been edited. Kill? " name))
                   (kill-buffer buffer))
             (kill-buffer buffer))))
    (setq list (cdr list))))

;; Kills all them buffers except scratch
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
          (buffer-list))
  (delete-other-windows))

;; org-mode

(setq org-mobile-directory (expand-file-name "~/Dropbox/orgfiles"))
(add-to-list 'load-path (expand-file-name "~/emacs/org/lisp"))
(require 'org-install)
(add-to-list 'load-path (expand-file-name "~/emacs/org/contrib/lisp"))
(require 'org-export-generic)

(global-set-key (quote [f5]) (quote nuke-all-buffers))

(add-to-list 'load-path (expand-file-name "~/emacs/color-theme/"))
(require 'color-theme)
;; (require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;; modified theme in place to have red cursor
     (color-theme-charcoal-black)))

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; Python flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    ;; Make sure it's not a remote buffer or flymake would not work
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "/opt/local/bin/pyflakes-2.6" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))


(defun my-python-mode-hook ()
  ;;(turn-on-real-auto-save)
  (flymake-mode t))

(add-hook 'python-mode-hook 'my-python-mode-hook)


(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))


;; automatically save buffer on move to another buffer.. could be contraversial
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))


(defun mongo-shell ()
  (interactive)
  (let* ((buffer (get-buffer-create "*mongo*")))
    (progn
      (pop-to-buffer buffer)
      (make-comint-in-buffer "mongo" buffer "/Users/agleyzer/apps/mongodb/bin/mongo" nil "localhost:57406/etf"))))

(defun mongo-send-region (start end)
  "Send a region to the mongo process."
  (interactive "r")
  (let ((mongo-buffer (get-buffer "*mongo*")))

    (if (buffer-live-p mongo-buffer)
        (save-excursion
          (comint-send-region mongo-buffer start end)
          (if (string-match "\n$" (buffer-substring start end))
              ()
            (comint-send-string sql-buffer "\n"))
          (message "Sent string to buffer %s." (buffer-name mongo-buffer))
          (pop-to-buffer mongo-buffer)
          (display-buffer mongo-buffer))
      (message "No mongo process started."))))


(defun mongo-send-buffer ()
  "Send the buffer contents to the mongo process."
  (interactive)
  (mongo-send-region (point-min) (point-max)))


;; (add-to-list 'load-path "~/emacs/textmate.el")
;; (require 'textmate)
;; (textmate-mode)



;; (global-set-key (quote [f7]) (quote mongo-send-buffer))
;; (global-set-key (quote [f8]) (quote mongo-send-region))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(after-save-hook (quote (cqmirror-update-this-file-maybe)))
 '(backup-directory-alist (quote (("." . "~/.backups"))))
 '(c-basic-offset 4)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-search-path (quote ("/Users/agleyzer/6/src/com/cnp/bunsen/importer/" nil)))
 '(delete-selection-mode t nil (delsel))
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.2129032258064516 . 0.28846153846153844) (ecb-sources-buffer-name 0.2129032258064516 . 0.23076923076923078) (ensime-type-inspector-tree-buffer-name 0.2129032258064516 . 0.28846153846153844) (ecb-history-buffer-name 0.2129032258064516 . 0.17307692307692307)))))
 '(ecb-options-version "2.40")
 '(flymake-gui-warnings-enabled nil)
 '(groovy-indent-level 4)
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill)))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(kill-whole-line t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-yank-at-point t)
 '(mumamo-chunk-coloring 3)
 '(ns-pop-up-frames nil)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-syntax-highlight-flag t)
 '(org-agenda-files (quote ("plans.org")))
 '(org-directory "~/orgfiles")
 '(org-export-author-info nil)
 '(org-export-creator-info nil)
 '(org-export-email-info nil)
 '(org-export-htmlize-output-type (quote css))
 '(org-export-time-stamp-file nil)
 '(org-export-with-timestamps nil)
 '(pc-selection-mode t nil (pc-select))
 '(safe-local-variable-values (quote ((cqmirror-update-on-save . t) (cqmirror-target-dir . "/Volumes/localhost"))))
 '(save-place t nil (saveplace))
 '(scroll-step 1)
 '(show-paren-mode t nil (paren))
 '(speedbar-show-unknown-files t)
 '(speedbar-sort-tags t)
 '(speedbar-tag-split-minimum-length 200)
 '(speedbar-use-images nil)
 '(split-width-threshold 1600)
 '(sql-oracle-program "/Users/agleyzer/apps/oracle_instantclient_10_2/sqlplus")
 '(sql-postgres-program "/Library/PostgreSQL8/bin/psql")
 '(sql-user "amg_user/amg_user@165.193.222.4:1521/CND01")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
 '(vc-follow-symlinks nil)
 '(vc-path (quote ("/usr/local/bin"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 240 :width normal :foundry "apple" :family "Inconsolata"))))
 '(bold ((t (:underline nil :weight thin)))))


(put 'erase-buffer 'disabled nil)
(put 'suspend-frame 'disabled t)


;; ;; remove bold from all faces
;; (mapc
;;  (lambda (face)
;;    (set-face-attribute face nil :weight 'normal :underline nil))
;;  (face-list))
;;
;; (set-face-bold-p 'bold nil)
