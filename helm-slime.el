;;; helm-slime.el --- helm-sources and some utilities for SLIME. -*- lexical-binding: t -*-

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;;               2012 Michael Markert <markert.michael@googlemail.com>
;;               2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;               2016 Syohei Yoshida <syohex@gmail.com>
;;               2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;; Author: Takeshi Banse <takebi@laafc.net>
;; URL: https://github.com/emacs-helm/helm-slime
;; Version: 0.0.1
;; Keywords: convenience, helm, slime
;; Package-Requires: ((emacs "24") (helm-core "1.9.8") (slime "2.18") (cl-lib "0.5"))

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
;; Some Helm and SLIME Configurations for using SLIME within the
;; Helm interface.
;;
;; The complete command list:
;;
;;  `helm-slime-complete'
;;    Select a symbol from the SLIME completion systems.
;;  `helm-slime-list-connections'
;;    Yet another `slime-list-connections' with `helm'.
;;  `helm-slime-apropos'
;;    Yet another `slime-apropos' with `helm'.
;;  `helm-slime-repl-history'
;;    Select an input from the SLIME repl's history and insert it.

;;; Installation:
;;
;; Put the helm-slime.el, helm.el to your load-path.
;; Set up SLIME properly.
;; Call `slime-setup' and include 'helm-slime as the arguments:
;;
;;   (slime-setup '([others contribs ...] helm-slime))
;;
;; or simply require helm-slime in some appropriate manner.

;;; Code:

(require 'helm)
(require 'helm-ring)
(require 'slime)
(require 'slime-c-p-c)
(require 'slime-fuzzy)
(require 'slime-repl)
(require 'cl-lib)

(defvar helm-slime--complete-target "")

(defun helm-slime--insert (candidate)
  (let ((pt (point)))
    (when (and (search-backward helm-slime--complete-target nil t)
               (string= (buffer-substring (point) pt) helm-slime--complete-target))
      (delete-region (point) pt)))
  (insert candidate))

(cl-defun helm-slime--symbol-position-funcall
    (f &optional (end-pt (point)) (beg-pt (slime-symbol-start-pos)))
  (let* ((end (move-marker (make-marker) end-pt))
         (beg (move-marker (make-marker) beg-pt)))
    (unwind-protect
        (funcall f beg end)
      (set-marker end nil)
      (set-marker beg nil))))

(defclass helm-slime-complete-type (helm-source-in-buffer)
  ((action :initform '(("Insert" . helm-slime--insert)
                       ("Describe symbol" . slime-describe-symbol)
                       ("Edit definition" . slime-edit-definition)))
   (persistent-action :initform #'slime-describe-symbol)
   (volatile :initform t)
   (get-line :initform #'buffer-substring))
  "SLIME complete.")

(defun helm-slime--asc-init-candidates-buffer-base (complete-fn insert-fn)
  (let ((put-text-property1 (lambda (s)
                              (put-text-property (point-at-bol 0)
                                                 (point-at-eol 0)
                                                 'helm-realvalue
                                                 s))))
    (let* ((completion-result (with-current-buffer helm-current-buffer
                                (funcall complete-fn)))
           (completions (if (listp (cl-first completion-result))
                            (cl-first completion-result)
                          completion-result))
           (base  (slime-symbol-at-point)))
      (with-current-buffer (helm-candidate-buffer 'global)
        (funcall insert-fn completions base put-text-property1)))))
(defun helm-slime--asc-init-candidates-buffer-basic-insert-function (completions base put-text-property1)
  (let ((len (length base)))
    (dolist (c completions)
      (let ((start (point)))
        (insert c)
        (put-text-property start (+ start len) 'face 'bold)
        (insert "\n")
        (funcall put-text-property1 c)))))
(defun helm-slime--asc-simple-init ()
  (helm-slime--asc-init-candidates-buffer-base
   (slime-curry 'slime-simple-completions helm-slime--complete-target)
   'helm-slime--asc-init-candidates-buffer-basic-insert-function))
(defun helm-slime--asc-compound-init ()
  (helm-slime--asc-init-candidates-buffer-base
   (slime-curry 'helm-slime--symbol-position-funcall 'slime-contextual-completions)
   'helm-slime--asc-init-candidates-buffer-basic-insert-function))
(cl-defun helm-slime--asc-fuzzy-init (&optional
                                (insert-choice-fn
                                 'slime-fuzzy-insert-completion-choice))
  (helm-slime--asc-init-candidates-buffer-base
   (slime-curry 'slime-fuzzy-completions helm-slime--complete-target)
   (lambda (completions _ put-text-property1)
     (with-current-buffer (helm-candidate-buffer 'global)
       (let ((max-len (cl-loop for (x _) in completions maximize (length x))))
         (dolist (c completions)
           (funcall insert-choice-fn c max-len)
           (funcall put-text-property1 (car c))))))))

(defvar helm-slime-simple-complete-source
  (helm-make-source "SLIME simple complete" 'helm-slime-complete-type
    :init #'helm-slime--asc-simple-init))
(defvar helm-slime-compound-complete-source
  (helm-make-source "SLIME compound complete" 'helm-slime-complete-type
    :init #'helm-slime--asc-compound-init))
(defvar helm-slime-fuzzy-complete-source
  (helm-make-source "SLIME fuzzy complete" 'helm-slime-complete-type
    :init #'helm-slime--asc-fuzzy-init))
(defvar helm-slime-complete-sources
  '(helm-slime-simple-complete-source
    helm-slime-fuzzy-complete-source
    helm-slime-compound-complete-source))

(defun helm-slime--helm-complete (sources target &optional limit input-idle-delay target-is-default-input-p)
  (let ((helm-candidate-number-limit (or limit helm-candidate-number-limit))
        (helm-input-idle-delay (or input-idle-delay helm-input-idle-delay))
        (helm-execute-action-at-once-if-one t)
        (helm-slime--complete-target target)
        (enable-recursive-minibuffers t)
        helm-full-frame)
    (helm :sources sources
          :input (if target-is-default-input-p target nil)
          :buffer "*helm complete*")))

;;;###autoload
(defun helm-slime-complete ()
  "Select a symbol from the SLIME's completion systems."
  (interactive)
  (helm-slime--helm-complete helm-slime-complete-sources
                       (helm-slime--symbol-position-funcall
                        #'buffer-substring-no-properties)))

(defvar helm-slime--c-source-slime-connection
  '((name . "SLIME connections")
    (candidates
     . (lambda ()
         (let ((fstring "%s%2s  %-10s  %-17s  %-7s %-s")
               (collect (lambda (p)
                          (cons
                           (format fstring
                                   (if (eq slime-default-connection p) "*" " ")
                                   (slime-connection-number p)
                                   (slime-connection-name p)
                                   (or (process-id p) (process-contact p))
                                   (slime-pid p)
                                   (slime-lisp-implementation-type p))
                           p))))
           (mapcar collect (reverse slime-net-processes)))))
    (action
     . (("Go to repl"
         . (lambda (p)
             (let ((slime-dispatching-connection p))
               (switch-to-buffer (slime-output-buffer)))))
        ("Set default" . slime-select-connection)
        ("Restart" . slime-restart-connection-at-point)
        ("Quit" . slime-quit-connection-at-point)))))

;;;###autoload
(defun helm-slime-list-connections ()
  "Yet another `slime-list-connections' with `helm'."
  (interactive)
  (helm 'helm-slime--c-source-slime-connection))

(defadvice helm-slime-update-connection-list (around ignore activate)
  "Don't call slime-update-connection-list if helming. (This is iffy.)"
  (when (not helm-source-name)
    ad-do-it))

(defclass helm-slime-apropos-type (helm-source-sync)
  ((action :initform '(("Describe symbol" . slime-describe-symbol)
                       ("Edit definition" . slime-edit-definition)))
   (persistent-action :initform #'slime-describe-symbol)
   ;;(volatile :initform t)
   (requires-pattern :initform 2))
  "SLIME apropos.")

(defun helm-slime--apropos-source (name slime-expressions)
  (helm-make-source name 'helm-slime-apropos-type
    :candidates `(lambda ()
                   (with-current-buffer helm-current-buffer
                     (cl-loop for plist in (slime-eval ,slime-expressions)
                              collect (plist-get plist :designator))))))
(defvar helm-slime--c-source-slime-apropos-symbol-current-package
  (helm-slime--apropos-source "SLIME apropos (current package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,helm-pattern
                           nil
                           nil
                           ,(or slime-buffer-package
                                (slime-current-package))))))
(defvar helm-slime--c-source-slime-apropos-symbol-current-external-package
  (helm-slime--apropos-source "SLIME apropos (current external package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,helm-pattern
                           t
                           nil
                           ,(or slime-buffer-package
                                (slime-current-package))))))
(defvar helm-slime--c-source-slime-apropos-symbol-all-external-package
  (helm-slime--apropos-source "SLIME apropos (all external package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,helm-pattern
                           t
                           nil
                           nil))))
(defvar helm-slime--c-source-slime-apropos-symbol-all-package
  (helm-slime--apropos-source "SLIME apropos (all package)"
                        (quote
                         `(swank:apropos-list-for-emacs
                           ,helm-pattern
                           nil
                           nil
                           nil))))
(defvar helm-slime-apropos-sources
  '(helm-slime--c-source-slime-apropos-symbol-current-package
    helm-slime--c-source-slime-apropos-symbol-current-external-package
    helm-slime--c-source-slime-apropos-symbol-all-external-package
    helm-slime--c-source-slime-apropos-symbol-all-package))

;;;###autoload
(defun helm-slime-apropos ()
  "Yet another `slime-apropos' with `helm'."
  (interactive)
  (helm :sources helm-slime-apropos-sources
        :buffer "*helm SLIME apropos*"))

(defun helm-slime-repl-input-history-action (candidate)
  (slime-repl-history-replace 'backward
                              (concat "^" (regexp-quote candidate) "$")))

(defvar helm-slime-source-repl-input-history
  (helm-build-sync-source "REPL history"
    :candidates (lambda ()
                  (with-helm-current-buffer
                    slime-repl-input-history))
    :action 'helm-slime-repl-input-history-action
    :multiline 'helm-kill-ring-max-offset)
  "Source that provides Helm completion against `slime-repl-input-history'.")

;;;###autoload
(defun helm-slime-repl-history ()
  "Select an input from the SLIME repl's history and insert it."
  (interactive)
  (when (derived-mode-p 'slime-repl-mode)
    (helm :sources 'helm-slime-source-repl-input-history
          :input (buffer-substring-no-properties (point) slime-repl-input-start-mark)
          :buffer "*helm SLIME history*")))

(provide 'helm-slime)
;;; helm-slime.el ends here
