;;; jujutsu-status.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 19, 2024
;; Modified: August 19, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/jujutsu-status
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'transient)
(require 'dash)
(require 's)

(require 'jujutsu-formatting)
(require 'jujutsu-core)
(require 'jujutsu-log)

(defun jujutsu-status--format-status-line (data)
  "Format a status line using DATA with fontification."
  (-let* [((&hash 'change-id-short chids
                  'change-id-shortest chidss
                  'commit-id-short coids
                  'commit-id-shortest coidss
                  'branches branches
                  'empty empty
                  'description desc)
           data)
          (empty (if (s-equals? empty "true")
                     (propertize "(empty) " 'face 'warning)
                   ""))
          (branches (if branches
                        (s-concat (propertize branches 'face 'magit-branch-local)
                                " | ")
                      ""))
          (change-id (jujutsu-formatting--format-id chids chidss))
          (commit-id (jujutsu-formatting--format-id coids coidss))
          (desc (if desc
                    (propertize desc 'face 'jujutsu-description-face)
                  (propertize "(no description set)" 'face 'warning)))]
    (format "%s %s %s%s%s" change-id commit-id branches empty desc)))

(defun jujutsu-status--format-status-header (wc-status p-status)
  "Format the status header with working copy and parent commit information."
  (list (format "%s %s\n"
                (propertize "Working copy :" 'face 'font-lock-type-face)
                (jujutsu-status--format-status-line wc-status))
        (format "%s %s\n"
                (propertize "Parent commit:" 'face 'font-lock-type-face)
                (jujutsu-status--format-status-line p-status))
        "\n"))

(defun jujutsu-status--format-single-file-change (file change-type)
  "Format a single file change for FILE with CHANGE-TYPE."
  (let ((face (cond ((string= change-type "A") 'magit-diffstat-added)
                    ((string= change-type "M") 'diff-changed)
                    ((string= change-type "D") 'magit-diffstat-removed))))
    (propertize
     (s-concat
      (propertize " " 'display '(left-fringe jujutsu-fringe-triangle-right))
      (format "%s %s\n%s"
              change-type
              file
              (propertize (format "    (Add actual file details here)\n" file)
                          'invisible t
                          'details t)))
     'face face
     'jj-dispatch-fn (lambda (action dom-id)
                       (message "DISPATCH: action=%s, dom-id=%s" action dom-id)))))

(defun jujutsu-status--format-working-copy-changes (all-files files-added files-modified files-deleted)
  "Format the working copy changes section."
  (cons
   (if (> (length all-files) 0)
       (propertize "Working copy changes:\n" 'face 'font-lock-keyword-face)
     (propertize "The working copy is clean\n" 'face 'font-lock-keyword-face))
   (if (> (length all-files) 0)
       (let ((sorted-changes
              (sort
               (-concat
                (-map (lambda (file) (cons file "A")) files-added)
                (-map (lambda (file) (cons file "M")) files-modified)
                (-map (lambda (file) (cons file "D")) files-deleted))
               (lambda (a b) (string< (car a) (car b))))))
         (-concat
          (-map (lambda (change)
                    (jujutsu-status--format-single-file-change (car change) (cdr change)))
                  sorted-changes)
          '("\n")))
     '("\n"))))

(defun jujutsu-status--format-log-section (revset)
  "Format the log section using REVSET."
  (cons (propertize "Log:\n"
                    'face 'font-lock-keyword-face)
        (jujutsu-log--format-log-entries revset)))

(defun jujutsu-status--hash-string (content)
  "Generate a hash for the given CONTENT."
  (sxhash-equal content))

(defun jujutsu-status--envelope-with-hash (s)
  (propertize s 'jj-dom-id (jujutsu-status--hash-string s)))

(defun jujutsu-status ()
  "Display a summary of the current Jujutsu working copy status."
  (interactive)
  (let* ((wc-status (jujutsu-core--get-status-data "@"))
         (p-status (jujutsu-core--get-status-data "@-"))
         (all-files (-concat (ht-get wc-status 'files-added)
                             (ht-get wc-status 'files-modified)
                             (ht-get wc-status 'files-deleted))))
    (with-current-buffer (get-buffer-create "*jujutsu-status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (->> (list (jujutsu-status--format-status-header wc-status p-status)
                   (jujutsu-status--format-working-copy-changes
                    all-files
                    (ht-get wc-status 'files-added)
                    (ht-get wc-status 'files-modified)
                    (ht-get wc-status 'files-deleted))
                   (jujutsu-status--format-log-section jujutsu-log-revset-fallback))
             -flatten
             (-map #'jujutsu-status--envelope-with-hash)
             (apply #'s-concat)
             insert))
      (goto-char (point-min))
      (jujutsu-status-mode)
      (switch-to-buffer "*jujutsu-status*"))))

(define-derived-mode jujutsu-status-mode special-mode "jujutsu status"
  "Major mode for displaying Jujutsu status."
  :group 'jujutsu
  (setq buffer-read-only t))

(define-key jujutsu-status-mode-map (kbd "g") #'jujutsu-status)

(defun jujutsu-status-squash (args)
  "Run jj squash with ARGS."
  (interactive (list (transient-args 'jujutsu-status-squash-popup)))
  (let ((cmd (concat "squash " (s-join " " args))))
    (jujutsu-core--run-command cmd t)
    (jujutsu-status)))

(defun jujutsu-status-new (args)
  "Run jj new with ARGS."
  (interactive (list (transient-args 'jujutsu-status-new-popup)))
  (let ((cmd (concat "squash " (s-join " " args))))
    (jujutsu-core--run-command cmd t)
    (jujutsu-status)))

(defun jujutsu-status-abandon ()
  "Run `jj abandon'."
  (interactive)
  (jujutsu-core--run-command "abandon" t)
  (jujutsu-status))

(defun jujutsu-status-describe (args)
  "Run jj describe with ARGS."
  (interactive (list (transient-args 'jujutsu-status-describe-popup)))
  (let ((cmd (concat "describe " (s-join " " args))))
    (jujutsu-core--run-command cmd t)
    (jujutsu-status)))

(transient-define-prefix jujutsu-status-describe-popup ()
  "Popup for jj describe options."
  ["Options"
   ("-m" "Message" "-m=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-m ") "\"")))]
  ["Actions"
   ("d" "Describe" jujutsu-status-describe)])

(transient-define-prefix jujutsu-status-squash-popup ()
  "Popup for jj squash options."
  ["Options"
   ("-I" "Ignore immutable" "--ignore-immutable")]
  ["Actions"
   ("s" "Squash" jujutsu-status-squash)])

(transient-define-prefix jujutsu-status-new-popup ()
  "Popup for jj squash options."
  ["Options"
   ("-m" "The change description to use" "--message=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-m ") "\"")))
   ("-N" "Do not edit the newly created change" "--no-edit")
   ("-A" "Insert the new change after the given commit" "--insert-after="
    :reader (lambda (&rest _args) (s-concat "\"" (read-string "--insert-after " nil nil "@") "\"")))
   ("-B" "Insert the new change before the given commit" "--insert-before="
    :reader (lambda (&rest _args) (s-concat "\"" (read-string "--insert-before " nil nil "@") "\"")))]
  ["Actions"
   ("n" "New" jujutsu-status-new)])

(transient-define-prefix jujutsu-status-popup ()
  "Popup for jujutsu actions in status buffer."
  ["Actions"
   ("a" "Abandon change" jujutsu-status-abandon)
   ("s" "Squash change" jujutsu-status-squash-popup)
   ("d" "Describe change" jujutsu-status-describe-popup)
   ("n" "New change" jujutsu-status-popup)])

(define-key jujutsu-status-mode-map (kbd "?")    #'jujutsu-status-popup)
(define-key jujutsu-status-mode-map (kbd "a")    #'jujutsu-status-abandon)
(define-key jujutsu-status-mode-map (kbd "s")    #'jujutsu-status-squash-popup)
(define-key jujutsu-status-mode-map (kbd "d")    #'jujutsu-status-describe-popup)
(define-key jujutsu-status-mode-map (kbd "n")    #'jujutsu-status-new-popup)

(defvar jujutsu-status-action-map
  '((tab . toggle)
    (return . enter))
  "Map of user actions to symbolic action names.")

(defun jujutsu-status-dispatch (&optional action-key)
  "Dispatch function for Jujutsu status buffer based on point location and ACTION-KEY."
  (interactive)
  (let* ((pos (point))
         (dom-id (get-text-property pos 'jj-dom-id))
         (dispatch-fn (get-text-property pos 'jj-dispatch-fn))
         (action (or (alist-get action-key jujutsu-status-action-map)
                     action-key
                     'default)))
    (cond
     (dispatch-fn
      (funcall dispatch-fn action dom-id))
     (dom-id
      (message "No action defined for this line (action=%s, dom-id=%s)" action dom-id))
     (t
      (message "No DOM ID or dispatch-fn at point")))))

(defun jujutsu-status-dispatch-return ()
  (interactive)
  (jujutsu-status-dispatch 'return))

(defun jujutsu-status-dispatch-tab ()
  (interactive)
  (jujutsu-status-dispatch 'tab))

(define-key jujutsu-status-mode-map (kbd "RET") #'jujutsu-status-dispatch-return)
(define-key jujutsu-status-mode-map (kbd "TAB") #'jujutsu-status-dispatch-tab)

(provide 'jujutsu-status)
;;; jujutsu-status.el ends here
