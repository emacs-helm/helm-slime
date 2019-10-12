;;; helm-slime.el --- helm-sources and some utilities for SLIME. -*- lexical-binding: t -*-

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;;               2012 Michael Markert <markert.michael@googlemail.com>
;;               2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2016 Syohei Yoshida <syohex@gmail.com>
;;               2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;; Author: Takeshi Banse <takebi@laafc.net>
;; URL: https://github.com/emacs-helm/helm-slime
;; Version: 0.4.0
;; Keywords: convenience, helm, slime, sly
;; Package-Requires: ((emacs "24") (helm "3.2") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; A Helm for using SLIME / Sly.
;;
;; The complete command list:
;;
;;  `helm-lisp-list-connections'
;;    Yet another Lisp connection list with `helm'.
;;  `helm-lisp-apropos'
;;    Yet another `slime-apropos' with `helm'.
;;  `helm-lisp-repl-history'
;;    Select an input from the SLIME repl's history and insert it.
;;    Sly can either use this function or directly `helm-comint-input-ring'.
;;  `helm-lisp-mini'
;;    Like ~helm-lisp-list-connections~, but include an extra source of
;;    Lisp-related buffers, like the events buffer or the scratch buffer.

;;; Installation:
;;
;; Add helm-slime.el to your load-path.
;; Set up SLIME or Sly properly.
;;
;; If you use SLIME, call `slime-setup' and include 'helm-slime as the
;; arguments:
;;
;;   (slime-setup '([others contribs ...] helm-slime))
;;
;; or simply require helm-slime in some appropriate manner.
;;
;; To use Helm instead of the Xref buffer, enable `global-helm-slime-mode'.
;;
;; To enable Helm for completion, install `helm-company'
;; (https://github.com/Sodel-the-Vociferous/helm-company).  With SLIME, you'll
;; also need `slime-company' (https://github.com/anwyn/slime-company/).  Then:
;;
;; - SLIME:
;;  (slime-setup '(slime-company))
;;  (require 'helm-company)
;;  (define-key slime-repl-mode-map (kbd "<tab>") 'helm-company)
;;
;; - Sly:
;;  (add-hook 'sly-mrepl-hook #'company-mode)
;;  (require 'helm-company)
;;  (define-key sly-mrepl-mode-map (kbd "<tab>") 'helm-company)

;;; Code:

(require 'helm)
(require 'helm-buffers)
(unless (require 'sly nil 'noerror)
  (require 'slime)
  (require 'slime-c-p-c)
  (require 'slime-fuzzy)
  (require 'slime-repl))
(require 'cl-lib)

(defun helm-lisp-sly-p ()
  "Return non-nil if Sly is active."
  (require 'sly nil 'noerror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar helm-lisp-connections-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-D") 'helm-lisp-run-delete-buffers)
    (define-key map (kbd "M-R") 'helm-lisp-run-rename-connection-buffer)
    map)
  "Keymap for Lisp connection source in Helm.")

(defun helm-lisp-output-buffer (&optional connection)
  (if (helm-lisp-sly-p)
      (sly-mrepl--find-buffer (or connection (sly-current-connection)))
    (let ((slime-dispatching-connection (or connection
                                            slime-dispatching-connection)))
      (slime-output-buffer))))

(defun helm-lisp-go-to-repl (_candidate)
  "Switched to marked REPL(s)."
  (helm-window-show-buffers
   (mapcar (lambda (candidate)
             (let ((buffer (nth 1 candidate))
                   (connection (nth 0 candidate)))
               (unless buffer
                 (if (helm-lisp-sly-p)
                     (sly-mrepl-new connection)
                   (let ((slime-dispatching-connection connection))
                     (slime-new-mrepl))))
               buffer))
           (helm-marked-candidates))))
(put 'helm-lisp-go-to-repl 'helm-only t)

(defun helm-lisp-process (connection)
  (if (helm-lisp-sly-p)
      (sly-process connection)
    (slime-process connection)))

(defun helm-lisp-connection-number (connection)
  (if (helm-lisp-sly-p)
      (sly-connection-number connection)
    (slime-connection-number connection)))

(defun helm-lisp-connection-name (connection)
  (if (helm-lisp-sly-p)
      (sly-connection-name connection)
    (slime-connection-name connection)))

(defun helm-lisp-pid (connection)
  (if (helm-lisp-sly-p)
      (sly-pid connection)
    (slime-pid connection)))

(defun helm-lisp-implementation-type (connection)
  (if (helm-lisp-sly-p)
      (sly-lisp-implementation-type connection)
    (slime-lisp-implementation-type connection)))

(defun helm-lisp-debug-buffers (connection)
  (if (helm-lisp-sly-p)
      (sly-db-buffers connection)
    (sldb-buffers connection)))

(defun helm-lisp-buffer-connection (buffer)
  (when (bufferp buffer)
    (with-current-buffer buffer
      (if (helm-lisp-sly-p)
          sly-buffer-connection
        slime-buffer-connection))))

(defun helm-lisp-go-to-inferior (_candidate)
  "Switched to inferior Lisps associated with the marked connections."
  (helm-window-show-buffers
   (cl-loop for c in (helm-marked-candidates)
            collect (process-buffer (helm-lisp-process
                                     (helm-lisp-buffer-connection c))))))
(put 'helm-lisp-go-to-inferior 'helm-only t)

(defun helm-lisp-go-to-debug (_candidate)
  "Switched to debug buffers associated with the marked connections."
  (helm-window-show-buffers
   (cl-loop for c in (helm-marked-candidates)
            append (helm-lisp-debug-buffers (car c)))))
(put 'helm-lisp-go-to-debug 'helm-only t)

(defun helm-lisp-run-delete-buffers ()
  "Run `helm-lisp-delete-buffers' action from `helm-lisp--c-source-connection'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-lisp-delete-buffers)))
(put 'helm-lisp-run-delete-buffers 'helm-only t)

(defun helm-lisp-set-default-connection (candidate)
  "Set connection to use by default to that of candidate buffer."
  (let ((connection (car candidate)))
    (if (helm-lisp-sly-p)
        (sly-select-connection connection)
      (slime-select-connection connection))))
(put 'helm-lisp-rename-connection-buffer 'helm-only t)

(defun helm-lisp--net-processes ()
  (if (helm-lisp-sly-p)
      sly-net-processes
    slime-net-processes))

(defun helm-lisp-delete-buffers (&optional _candidate)
  "Kill marked REPL(s) and their inferior Lisps if they are the
last buffer connected to it."
  (let ((connections (helm-lisp--net-processes))
        (repl-buffers (helm-lisp--repl-buffers)))
    (dolist (c (helm-marked-candidates))
      (let ((last-connection?
             (not (memq (car c)
                        (mapcar #'helm-lisp-buffer-connection
                                (delete (cadr c) repl-buffers))))))
        (when last-connection?
          (if (helm-lisp-sly-p)
              (let ((sly-dispatching-connection (car c)))
                (sly-quit-lisp t))
            (let ((slime-dispatching-connection (car c)))
              (slime-repl-quit))))
        (kill-buffer (cadr c))))))
(put 'helm-lisp-delete-buffers 'helm-only t)

(defun helm-lisp-restart-connections (_candidate)
  "Restart marked REPLs' inferior Lisps."
  (dolist (c (helm-marked-candidates))
    (if (helm-lisp-sly-p)
        (sly-restart-connection-at-point (helm-lisp-buffer-connection c))
      (slime-restart-connection-at-point (helm-lisp-buffer-connection c)))))
(put 'helm-lisp-restart-connections 'helm-only t)

(defun helm-lisp-run-rename-connection-buffer ()
  "Run `helm-lisp-rename-connection-buffer' action from `helm-lisp--c-source-connection'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-lisp-rename-connection-buffer)))
(put 'helm-lisp-run-rename-connection 'helm-only t)

(defun helm-lisp-rename-connection-buffer (candidate)
  "Rename REPL buffer."
  (let* ((slime-dispatching-connection (car candidate)))
    (when (cadr candidate)
      (with-current-buffer (cadr candidate)
        (rename-buffer (helm-read-string "New name: " (buffer-name)))))))
(put 'helm-lisp-rename-connection-buffer 'helm-only t)

(defcustom helm-lisp-connection-actions
  `(("Go to REPL" . helm-lisp-go-to-repl)
    ("Set default" . helm-lisp-set-default-connection)
    ("Restart" . helm-lisp-restart-connections)
    (,(substitute-command-keys "Rename REPL buffer \\<helm-lisp-connections-map>`\\[helm-lisp-run-rename-connection-buffer]'")
     . helm-lisp-rename-connection-buffer)
    (,(substitute-command-keys "Quit \\<helm-lisp-connections-map>`\\[helm-lisp-run-delete-buffers]'")
     . helm-lisp-delete-buffers)
    ("Go to inferior Lisp" . helm-lisp-go-to-inferior)
    ("Go to debug buffers" . helm-lisp-go-to-debug))
  "Actions for `helm-lisp-list-connections`."
  :group 'helm-lisp
  :type '(alist :key-type string :value-type function))

(defun helm-lisp--connection-candidates (p &optional buffer)
  "Return (DISPLAY-VALUE . REAL-VALUE) for connection P.
The REAL-VALUE is (P BUFFER)."
  (setq buffer (or buffer
                   (helm-lisp-output-buffer p)))
  (let ((fstring "%s%2s  %-10s  %-17s  %-7s %-s %s"))
    (cons
     (format fstring
             (if (eq (if (helm-lisp-sly-p)
                         sly-default-connection
                       slime-default-connection)
                     p)
                 "*"
               " ")
             (helm-lisp-connection-number p)
             (helm-lisp-connection-name p)
             (or (process-id p) (process-contact p))
             (helm-lisp-pid p)
             (helm-lisp-implementation-type p)
             buffer)
     (list p buffer))))

(defun helm-lisp--repl-buffers (&optional connection thread)
  ;; Inspired by `sly-mrepl--find-buffer'.
  (cl-remove-if-not
   (lambda (x)
     (with-current-buffer x
       (and (memq major-mode '(sly-mrepl-mode
                               slime-repl-mode
                               slime-mrepl-mode))
            (or (not connection)
                (eq (if (helm-lisp-sly-p)
                        sly-buffer-connection
                      slime-buffer-connection)
                    connection))
            (or (not thread)
                (eq thread (if (boundp 'sly-current-thread)
                               sly-current-thread
                             slime-current-thread))))))
   (buffer-list)))

(defun helm-lisp--repl-buffer-candidates ()
  "Return buffer/connection candidates.
It returns all REPL buffer candidates + connections without buffers."
  (let* ((repl-buffers (helm-lisp--repl-buffers))
         (buffer-connections (cl-delete-duplicates
                              (mapcar #'helm-lisp-buffer-connection
                                      repl-buffers))))
    (append (mapcar (lambda (b)
                      (helm-lisp--connection-candidates
                       (helm-lisp-buffer-connection b)
                       b))
                    repl-buffers)
            (mapcar #'helm-lisp--connection-candidates
                    (cl-set-difference
                     (reverse (helm-lisp--net-processes))
                     buffer-connections)))))

(defun helm-lisp--c-source-connection ()
  (helm-build-sync-source "Lisp connections"
    :candidates (helm-lisp--repl-buffer-candidates)
    :action helm-lisp-connection-actions
    :keymap helm-lisp-connections-map))

;;;###autoload
(defun helm-lisp-list-connections ()
  "List Lisp connections with Helm."
  (interactive)
  (helm :sources (list (helm-lisp--c-source-connection))
        :buffer "*helm-lisp-list-connections*"))

(defun helm-lisp--buffer-candidates ()
  "Collect Lisp-related buffers, like the `events' buffer.
If the buffer does not exist, we use the associated function to generate it.

The list is in the (DISPLAY . REAL) form.  Because Helm seems to
require that REAL be a string, we need to (funcall (intern
\"function\")) in `helm-lisp-switch-buffers' to generate the
buffer."
  (if (helm-lisp-sly-p)
      (list (cons (sly-buffer-name :events :connection (sly-current-connection))
                  "sly-pop-to-events-buffer")
            (cons (sly-buffer-name :threads :connection (sly-current-connection))
                  "sly-list-threads")
            (cons (sly-buffer-name :scratch :connection (sly-current-connection))
                  "sly-scratch"))
    (list (cons slime-event-buffer-name "slime-events-buffer")
            (cons slime-threads-buffer-name "slime-list-threads")
            (cons (slime-buffer-name :scratch) "slime-scratch-buffer"))))

(defun helm-lisp-switch-buffers (_candidate)
  "Switch to buffer candidates and replace current buffer.

If more than one buffer marked switch to these buffers in separate windows.
If a prefix arg is given split windows vertically."
  (helm-window-show-buffers
   (cl-loop for b in (helm-marked-candidates)
            collect (funcall (intern b)))))

(defun helm-lisp-build-buffers-source ()
  (helm-build-sync-source "Lisp buffers"
    :candidates (helm-lisp--buffer-candidates)
    :action `(("Switch to buffer(s)" . helm-lisp-switch-buffers))))

(defun helm-lisp-new-repl (name)
  (if (helm-lisp-sly-p)
      ;; TODO: Set new Sly connection?
      (sly-mrepl-new (sly-current-connection) name)
    (cl-flet ((slime-repl-buffer (&optional create _connection)
                                 (funcall (if create #'get-buffer-create #'get-buffer)
                                          (format "*slime-repl %s*" ;; (slime-connection-name connection)
                                                  name))))
      ;; TODO: Set REPL buffer name to *slime-repl NAME*.
      ;; The following does not work.
      ;; (rename-buffer (format "*slime-repl %s*" name))
      (if (fboundp 'slime-new-mrepl)
          (slime-new-mrepl)
        (slime)))))

(defun helm-lisp-new-repl-choose-lisp (name)
  "Fetch URL and render the page in a new buffer.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (let ((current-prefix-arg '-))
    (helm-lisp-new-repl name)))

(defvar helm-lisp-new
  (helm-build-dummy-source "Open new REPL"
    :action (helm-make-actions
             "Open new REPL" 'helm-lisp-new-repl
             "Open new REPL with chosen Lisp" 'helm-lisp-new-repl-choose-lisp)))

(defun helm-lisp-mini ()
  "Helm for Lisp connections and buffers."
  (interactive)
  (helm :sources (list (helm-lisp--c-source-connection)
                       helm-lisp-new
                       (helm-lisp-build-buffers-source))
        :buffer "*helm-lisp-mini*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass helm-lisp-apropos-type (helm-source-sync)
  ((action :initform `(("Describe SYMBOL" . ,(if (helm-lisp-sly-p)
                                                 'sly-describe-symbol
                                               'slime-describe-symbol))
                       ("Edit definition" . ,(if (helm-lisp-sly-p)
                                                 'sly-edit-definition
                                               'slime-edit-definition))))
   (persistent-action :initform (if (helm-lisp-sly-p)
                                    #'sly-describe-symbol
                                  #'slime-describe-symbol))
   ;;(volatile :initform t)
   (requires-pattern :initform 2))
  "Lisp apropos.")

(defun helm-lisp--format-designator (designator)
  (if (helm-lisp-sly-p)
      (let ((package (cadr designator))
            (name (car designator)))
        (format "%s:%s" package name))
      designator))

(defun helm-lisp--apropos-source (name external-only case-sensitive current-package)
  "Build source that provides Helm completion against `apropos'."
  (helm-make-source name 'helm-lisp-apropos-type
    :candidates `(lambda ()
                   (with-current-buffer helm-current-buffer
                     (cl-loop for plist in (if (helm-lisp-sly-p)
                                               (sly-eval (list 'slynk-apropos:apropos-list-for-emacs
                                                               helm-pattern
                                                               ,(not (null external-only))
                                                               ,(not (null case-sensitive))
                                                               (when ,current-package
                                                                 (helm-lisp-current-package))))
                                             (slime-eval (list 'swank:apropos-list-for-emacs
                                                               helm-pattern
                                                               ,(not (null external-only))
                                                               ,(not (null case-sensitive))
                                                               (when ,current-package
                                                                 (helm-lisp-current-package)))))
                              collect (helm-lisp--format-designator (plist-get plist :designator)))))))

(defun helm-lisp-current-package ()
  (if (helm-lisp-sly-p)
      (or sly-buffer-package
          (sly-current-package))
    (or slime-buffer-package
        (slime-current-package))))

(defvar helm-lisp--c-source-apropos-symbol-current-package
  (helm-lisp--apropos-source
   "Apropos (current package)"
   nil
   nil
   (helm-lisp-current-package)))

(defvar helm-lisp--c-source-apropos-symbol-current-external-package
  (helm-lisp--apropos-source
   "Apropos (current external package)"
   'external-only
   nil
   (helm-lisp-current-package)))

(defvar helm-lisp--c-source-apropos-symbol-all-external-package
  (helm-lisp--apropos-source
   "Apropos (all external packages)"
   'external-only
   nil
   nil))

(defvar helm-lisp--c-source-apropos-symbol-all-package
  (helm-lisp--apropos-source
   "Apropos (all packages)"
   nil
   nil
   nil))

(defvar helm-lisp-apropos-sources
  '(helm-lisp--c-source-apropos-symbol-current-package
    helm-lisp--c-source-apropos-symbol-current-external-package
    helm-lisp--c-source-apropos-symbol-all-external-package
    helm-lisp--c-source-apropos-symbol-all-package)
  "List of Helm sources for `helm-lisp-apropos'.")

;;;###autoload
(defun helm-lisp-apropos ()
  "Yet another Apropos with `helm'."
  (interactive)
  (helm :sources helm-lisp-apropos-sources
        :buffer "*helm lisp apropos*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun helm-lisp-repl-input-history-action (candidate)
  "Default action for `helm-lisp-repl-history'."
  (slime-repl-history-replace 'backward
                              (concat "^" (regexp-quote candidate) "$")))

(defgroup helm-lisp nil
  "SLIME for Helm."
  :group 'helm)

(defcustom helm-lisp-history-max-offset 400
  "Max number of chars displayed per candidate in `helm-lisp-repl-history'.
When `t', don't truncate candidate, show all.
By default it is approximatively the number of bits contained in five lines
of 80 chars each i.e 80*5.
Note that if you set this to nil multiline will be disabled, i.e you
will not have anymore separators between candidates."
  :type '(choice (const :tag "Disabled" t)
          (integer :tag "Max candidate offset"))
  :group 'helm-lisp)

(defvar helm-lisp-source-repl-input-history
  (helm-build-sync-source "REPL history"
    :candidates (lambda ()
                  (with-helm-current-buffer
                    slime-repl-input-history))
    :action 'helm-lisp-repl-input-history-action
    :multiline 'helm-lisp-history-max-offset)
  "Source that provides Helm completion against `slime-repl-input-history'.")

;;;###autoload
(defun helm-lisp-repl-history ()
  "Select an input from the REPL's history and insert it.
MREPL use the buffer local history as per comint mode.
SLIME REPL uses its own global history."
  (interactive)
  (if (or (helm-lisp-sly-p)
          (derived-mode-p 'slime-mrepl-mode))
      (helm-comint-input-ring)
    (when (derived-mode-p 'slime-repl-mode)
      (helm :sources 'helm-lisp-source-repl-input-history
            :input (buffer-substring-no-properties (point) slime-repl-input-start-mark)
            :buffer "*helm lisp repl history*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun helm-lisp-normalize-xrefs (xref-alist)
  "Like `slime-insert-xrefs' but return a formatted list of strings instead.
The strings are formatted as \"GROUP: LABEL\"."
  (cl-loop for (group . refs) in xref-alist
           append
           (cl-loop for (label location) in refs
                    collect
                    (list group label location))))

(defun helm-lisp-xref-lineno (location)
  "Return 0 if there is location does not refer to a proper file."
  (or
   (ignore-errors
     (save-window-excursion
       (if (helm-lisp-sly-p)
           (sly--pop-to-source-location location
                                        'sly-xref)
         (slime-goto-location-buffer (nth 1 location)))
       (line-number-at-pos
        (car (alist-get :position (cdr location))))))
   0))

(defun helm-lisp-xref-transformer (candidates)
  "Transform CANDIDATES (a list of (GROUP LABEL LOCATION) as per
`helm-lisp-normalize-xrefs') to \"GROUP: LABEL\"."
  (cl-loop for (group label location) in candidates
           collect (cons (concat (propertize (abbreviate-file-name group)
                                             'face 'helm-grep-file)
                                 ":"
                                 (propertize
                                  (number-to-string (helm-lisp-xref-lineno location))
                                  'face 'helm-grep-lineno)
                                 ":"
                                 (if (helm-lisp-sly-p)
                                     (sly-one-line-ify label)
                                   (slime-one-line-ify label)))
                         (list group label location))))

(defun helm-lisp-xref-goto (candidate)
  (let ((location (nth 2 candidate)))
    (switch-to-buffer
     (if (helm-lisp-sly-p)
         (save-window-excursion
           (sly--pop-to-source-location location 'sly-xref))
       (save-window-excursion
         (slime-show-source-location location t 1)
         (slime-goto-location-buffer (nth 1 location)))))))

(defun helm-lisp-build-xref-source (xrefs)
  (helm-build-sync-source "Lisp xrefs"
    :candidates (helm-lisp-normalize-xrefs xrefs)
    :candidate-transformer 'helm-lisp-xref-transformer
    :action `(("Switch to buffer(s)" . helm-lisp-xref-goto))))

(defun helm-lisp-show-xref-buffer (xrefs _type _symbol _package &optional _method)
  "See `sly-xref--show-results'."
  (helm :sources (list (helm-lisp-build-xref-source xrefs))
        :buffer "*helm-lisp-xref*"))

;;;###autoload
(define-minor-mode helm-lisp-mode
  "Use Helm for Lisp xref selections.
Note that the local minor mode has a global effect, thus making
`global-helm-lisp-mode' and `helm-lisp-mode' equivalent."
  ;; TODO: Is it possible to disable the local minor mode?
  :init-value nil
  (let ((target (if (helm-lisp-sly-p)
                    'sly-xref--show-results
                  'slime-show-xref-buffer)))
    (if (advice-member-p 'helm-lisp-show-xref-buffer target)
        (advice-remove target 'helm-lisp-show-xref-buffer)
      (advice-add target :override 'helm-lisp-show-xref-buffer))))

;;;###autoload
(define-globalized-minor-mode global-helm-lisp-mode
  helm-lisp-mode
  helm-lisp-mode)

(provide 'helm-slime)
;;; helm-slime.el ends here
