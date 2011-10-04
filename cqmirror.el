;; Maintains a mirror of the file in another directory

;; For example, when using WebDAV to access CQ5, it seems that editing
;; files directly in the mounted volume presents all kinds of
;; problems. But if one copies files on a local drive and sets up
;; mirroring, it seems to be working much better.

;; This code expects two variables setup as "directory local vars" in
;; a file in the root of the local copy. The file might look something
;; like this:

;; ((nil . ((cqmirror-target-dir . "/Volumes/localhost/apps")
;;         (cqmirror-update-on-save . t))))

;; This means that for all files under this directory, the two
;; variables will be set as buffer-local. Once these vars are set,
;; mirroring kicks in on every save.

;; The first variable `cqmirror-target-dir' establishes the root of
;; the remote directory tree.

;; If `cqmirror-update-on-save' is not set as t, one can still mirror
;; by hand, calling `cqmirror-update-this-file' directly.

;; To start using simply add (require 'cqmirror) to your .emacs


(defvar cqmirror-update-on-save nil
  "Automatically update the remote mirror whenever this file is saved.")
(make-variable-buffer-local 'cqmirror-update-on-save)
(put 'cqmirror-update-on-save 'permanent-local t)


(defvar cqmirror-target-dir nil
  "Target directory for mirroring.")
(make-variable-buffer-local 'cqmirror-target-dir)
(put 'cqmirror-target-dir 'permanent-local t)


(defun cqmirror-relative-name (file)
  "Returns file name relative to the project dir."
  (let* ((dlf (dir-locals-find-file file))
         ;; dir-locals-find-file returns a weird data structure or a string..
         ;; it could contains either parent dir or the .dir-locals.el file itself
         (dlf-file-or-dir (cond ((consp dlf) (nth 0 dlf))
                                ((stringp dlf) dlf)))
         ;; if it was a file, extract parent dir
         (dlf-dir (if (file-regular-p dlf-file-or-dir)
                      (file-name-directory dlf-file-or-dir)
                    dlf-file-or-dir)))
    (file-relative-name file dlf-dir)))


(defun cqmirror-update-this-file ()
  "Copies current buffer's file into a mirrored file."
  (interactive)
  (if cqmirror-target-dir
      (let* ((src (buffer-file-name))
             (dst (expand-file-name (cqmirror-relative-name src)
                                    cqmirror-target-dir)))
        (message "Destination: %s" dst)
        ;; CQ5's WebDAV when mounted on OSX seems to report that any
        ;; file is present, but still it's nice to check if dst exists
        (if (or (file-exists-p dst)
                (y-or-n-p "Destination file doesn't exist. Create?"))
            (progn
              (copy-file src dst t t)
              (message "Copied to %s" dst))))))


(defun cqmirror-update-this-file-maybe ()
  "Update the remote mirror iff local variable `cqmirror-this-file' is t."
  (when cqmirror-update-on-save
    (cqmirror-update-this-file)))


(add-hook 'after-save-hook 'cqmirror-update-this-file-maybe t t)


(provide 'cqmirror)

