;;; early-init.el - loaded before our init.el; lets us switch package
;;; managers with relative ease.
;;;
;;; Requires Emacs 27.1 and above!

;; And while we're here, we might as well defer GCing for a bit to
;; (significantly) improve boot time.

(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer
 5 nil
 (lambda () (setq gc-cons-threshold (* 100 1024))))

;; Replace package.el with straight.el
;; (setq package-enable-at-startup nil)

(setq straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
