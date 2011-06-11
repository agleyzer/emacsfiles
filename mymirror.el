;; Maintains a mirror of the file in another directory

;; For example, when using WebDAV to access CQ5, it seems that editing
;; files directly in the mounted volume presents all kinds of
;; problems. But if one copies files on a local drive and sets up
;; mirroring, it seems to be working much better.

;; This code expects two variables setup as "directory local vars" in
;; a file in the root of the local copy. The file might look something
;; like this:

;; ((nil . ((mymirror-target-dir . "/Volumes/localhost/apps")
;;         (mymirror-update-on-save . t))))

;; This means that for all files under this directory, the two
;; variables will be set as buffer-local. Once these vars are set,
;; mirroring kicks in on every save.

;; The first variable `mymirror-target-dir' establishes the root of
;; the remote directory tree.

;; If `mymirror-update-on-save' is not set as t, one can still mirror
;; by hand, calling `mymirror-update-this-file' directly.

;; To start using simply add (require 'mymirror) to your .emacs

(defvar mymirror-update-on-save nil
  "Automatically update the remote mirror whenever this file is saved.")
(make-variable-buffer-local 'mymirror-update-on-save)

(defvar mymirror-target-dir nil
  "Target directory for mirroring.")
(make-variable-buffer-local 'mymirror-target-dir)

(defun mymirror-relative-name (file)
  "Returns file name relative to the project dir."
  (let* ((dlf (dir-locals-find-file file))
         (dir (cond ((consp dlf) (nth 0 dlf))
                    ((stringp dlf) dlf))))
    (file-relative-name file dir)))

(defun mymirror-update-this-file ()
  "Copies current buffer's file into a mirrored file."
  (interactive)
  (if mymirror-target-dir
      (let* ((src (buffer-file-name))
             (dst (expand-file-name (mymirror-relative-name src)
                                    mymirror-target-dir)))
        (copy-file src dst t t)
        (message "Copied to %s" dst))))

(defun mymirror-update-this-file-maybe ()
  "Update the remote mirror iff local variable `mymirror-this-file' is t."
  (condition-case error
      ;; double check in case we have turned this off temporarily with
      ;; M-x set-variable
      (when mymirror-update-on-save
        (mymirror-update-this-file))
    (error t)))

(defun mymirror-check-for-mirror-this-file ()
  (condition-case error
      (when mymirror-update-on-save ;; local-variable
        (add-hook 'after-save-hook 'mymirror-update-this-file-maybe t t))
    (error t)))

(add-hook 'find-file-hook 'mymirror-check-for-mirror-this-file t nil)

(provide 'mymirror)

