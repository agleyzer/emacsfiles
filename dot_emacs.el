(setq load-path (cons "~/emacs" load-path))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setenv "PATH" (concat (expand-file-name "~/bin") ":"
                       (expand-file-name "~/apps/scala/bin") ":"
                       (expand-file-name "~/apps/groovy/bin") ":"
                       "/usr/local/bin" ":"
                       "/opt/local/bin" ":"
                       (getenv "PATH")))

(setenv "MANPATH" (concat "/usr/local/man" ":"
                          "/opt/local/man" ":"
                          (getenv "MANPATH")))

;; basic shit
(set-default-font "-apple-consolas-medium-r-normal--22-160-*-*-*-*-*-*")

;; No stupid questions
(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-shell ()
  (interactive)
  (delete-other-windows)
  (split-window)
  (other-window 1)
  (shell)
)

;; Set some keys - that's the way I like it :)
(global-set-key "\M-s" (quote my-shell))
(global-set-key "\M-g" (quote goto-line))
(global-set-key (quote [f1]) (quote manual-entry))
(global-set-key (quote [C-tab]) (quote other-window))
(global-set-key (quote [home]) (quote beginning-of-line))
(global-set-key (quote [end]) (quote end-of-line))
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

;; ;; scala mode
;; (add-to-list 'load-path (expand-file-name "~/emacs/scala"))
;; (require 'scala-mode-auto)
;; ;; scala build tool - buggy

;; (load "sbt")
;; (defun me-turn-off-indent-tabs-mode ()
;;  (setq indent-tabs-mode nil))
;; (add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)

;; ;; ensime
;; (add-to-list 'load-path (expand-file-name "~/emacs/ensime/src/main/elisp/"))
;; (require 'ensime)
;; (require 'ensime-test)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;; javascript mode
;; (autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;; (require 'real-auto-save)
;; (setq real-auto-save-interval 5) ;; in seconds

(require 'flymake-jslint-local)

(require 'unit-test)
(define-key global-map [f3] 'run-unit-tests)
(define-key global-map [f4] 'open-unit-test-file)

(defun my-js-unit-test-command ()
  (interactive)
  (let* ((test-file (my-js-unit-test-file buffer-file-name)))
    (if test-file
        (zerop (shell-command (concat "node " test-file) "*unit-test-output*" "*unit-test-output*"))
      (progn
        (message "no unit test for %s" buffer-file-name)
        nil))))

(defun my-js-compile-run-unit-test ()
  (interactive)
  (let* ((test-file (my-js-unit-test-file buffer-file-name)))
    (if test-file
        (compile (concat "node " test-file))
      (progn
        (message "no unit test for %s" buffer-file-name)
        nil))))

(defun my-js-unit-test-file (file)
  (interactive "f")
  (if (my-js-is-unit-test-file file)
      file
    (let ((test-file (concat (file-name-sans-extension file) "_test.js")))
      (if (file-exists-p test-file) test-file nil))))

(defun my-js-is-unit-test-file (file)
  (interactive "f")
  (integerp (string-match ".+_test.js" file)))

(defun my-js-mode-hook ()
  ;; compilation regexp for jslint & node.js
  (setq compilation-error-regexp-alist
        '(("^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\)) JSLINT: \\(.+\\)$" 1 2 3)
          ("^[     ]*.+(\\(.+\\):\\([0-9]+\\):\\([0-9]+\\))$" 1 2 3)))

  ;; espresso mode overrides standard M-., I want it back.
  (define-key espresso-mode-map [(meta ?.)] #'find-tag)

  (define-key espresso-mode-map [f2] 'my-js-compile-run-unit-test)

  ;; (turn-on-real-auto-save)
  (flymake-mode t)
  (setq unit-test-command 'my-js-unit-test-command)
  (setq unit-test-file-fn 'my-js-unit-test-file)
  (add-hook 'before-save-hook
            (lambda ()
              (untabify (point-min) (point-max))))
  (add-hook 'after-save-hook 'run-unit-tests t t))

(add-hook 'espresso-mode-hook 'my-js-mode-hook)

;; groovy mode
;; (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
;; (autoload 'groovy-mode "groovy" nil t)

(define-key global-map [f2]
  (lambda ()
    (interactive)
    (progn
      (save-buffer)
      (compile
       (concat
        (cond
         ((string= "scala" (file-name-extension (buffer-file-name))) "fsc")
         ((string= "py" (file-name-extension (buffer-file-name))) "python")
         ((string= "groovy" (file-name-extension (buffer-file-name))) "groovy")
         ((string= "js" (file-name-extension (buffer-file-name))) "node")
         (t "echo no compiler for "))
        " \"" (buffer-file-name) "\"")))))

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


(global-set-key (quote [f5]) (quote nuke-all-buffers))


(add-to-list 'load-path (expand-file-name "~/emacs/color-theme/"))
(require 'color-theme)
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
        (list "/opt/local/bin/pyflakes" (list local-file)))))
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
      (make-comint-in-buffer "mongo" buffer "/opt/mongodb/bin/mongo" nil "etf"))))

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


(global-set-key (quote [f7]) (quote mongo-send-buffer))
(global-set-key (quote [f8]) (quote mongo-send-region))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.backups"))))
 '(c-basic-offset 4)
 '(case-fold-search t)
 '(column-number-mode t)
 '(delete-selection-mode t nil (delsel))
 '(groovy-indent-level 4)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-yank-at-point t)
 '(ns-pop-up-frames nil)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-syntax-highlight-flag t)
 '(pc-selection-mode t nil (pc-select))
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
 '(use-dialog-box nil)
 '(vc-path (quote ("/usr/local/bin"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(put 'erase-buffer 'disabled nil)

