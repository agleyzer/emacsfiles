;; adapted from http://www.emacswiki.org/emacs/FlymakeJavaScript
;;
;; Installation:
;;
;; Put this in your load-path, then add the following to your .emacs.
;; You substitude espresso-mode-hook for javascript-mode-hook if you
;; use espresso.
;;
;;     (require 'flymake-jslint)
;;     (add-hook 'javascript-mode-hook
;;         (lambda () (flymake-mode t)))
;;
;; Do M-x customize-group flymake-jslint to customize paths and port.
;;
;; Run M-x lintnode-start once to start the server before invoking flymake.

(require 'flymake)

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "/usr/local/bin/jslint" (list local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
	      flymake-jslint-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\)) JSLINT: \\(.+\\)$"
	      1 2 3 4)
	    flymake-err-line-patterns))

(provide 'flymake-jslint-local)
