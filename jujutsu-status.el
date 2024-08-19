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
(require 'ht)

(require 'nx)
(require 'jujutsu-formatting)
(require 'jujutsu-core)
(require 'jujutsu-log)

(defun jujutsu-status--format-status-line (data)
  "Format a status line using DATA with fontification."
  (-let* [((&hash :change-id-short chids
                  :change-id-shortest chidss
                  :commit-id-short coids
                  :commit-id-shortest coidss
                  :branches branches
                  :empty empty
                  :description desc)
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
              (propertize (format "    (Add actual file details here)\n")
                          'invisible t
                          'details t)))
     'face face
     'jj-dispatch-fn (lambda (action dom-id)
                       (message "DISPATCH: action=%s, dom-id=%s" action dom-id)))))

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

(defun jujutsu-status--make-tree ()
  "Construct the complete Jujutsu status tree structure.
Returns a tree representation of the current Jujutsu repository status,
including working copy status, parent commit status, and log entries."
  (let* ((wc-status (jujutsu-core--get-status-data "@"))
         (p-status (jujutsu-core--get-status-data "@-"))
         (all-files (-concat (ht-get wc-status :files-added)
                             (ht-get wc-status :files-modified)
                             (ht-get wc-status :files-deleted))))
    (nx :root (ht)
     (list
      (nx :status-section (ht)
       (list
        (nx :status-header (ht)
         (list
          (nx :text (ht (:text "Working copy :")
                                  (:face 'font-lock-type-face)))
          (nx :working-copy-status wc-status)
          (nx :newline)
          (nx :text (ht (:text "Parent commit: ")
                                  (:face 'font-lock-type-face)))
          (nx :parent-commit-status p-status)
          (nx :newline)))
        (nx :newline)
        (nx :working-copy-changes-header (ht)
         (list
          (nx :text (ht (:text (if (> (length all-files) 0)
                          "Working copy changes:"
                        "The working copy is clean"))
               (:face 'font-lock-keyword-face)))
          (nx :newline)
          (nx :working-copy-changes (ht)
           (-map (lambda (file)
                   (nx :file-change
                                 (ht (:file file)
                                     (:type (cond
                                             ((member file (ht-get wc-status :files-added)) "A")
                                             ((member file (ht-get wc-status :files-modified)) "M")
                                             ((member file (ht-get wc-status :files-deleted)) "D")))
                                     (:expanded nil))
                                 nil))
                 all-files))))))
      (nx :newline)
      (let* ((lentries (jujutsu-log--get-log-entries jujutsu-log-revset-fallback))
             (includes-root? (-some (lambda (m) (s-equals? (ht-get m :root) "true")) lentries)))
        (nx :log-section (ht)
         (-concat
          (list (nx :log-section-header (ht (:text "Log:")
                                                      (:upcase nil)))
                (nx :newline))
          (-map (lambda (entry)
                  (nx :log-entry entry nil))
                lentries)
          (when (not includes-root?) (list (nx :verbatim "~"))))))))))

(-comment
 (with-current-buffer (get-buffer-create "*jj debug*")
   (fundamental-mode)
   (erase-buffer)
   (->> (jujutsu-status--make-tree)
        jujutsu-dev--ht-to-edn-pp
        insert)
   (clojure-mode)
   (display-buffer "*jj debug*"))
 1)

(defun jujutsu-status--render-tree (tree)
  "Render the Jujutsu TREE structure as a string.
TREE is the root node of the Jujutsu status tree."
  (jujutsu-status--render-node tree ""))

(defun jujutsu-status--render-node (node result)
  "Recursively render a NODE and its children, appending to RESULT.
NODE is the current node being rendered.
RESULT is the accumulated string of rendered nodes."
  (-let* [((&hash :type type :props props :children children) node)
          (content
           (pcase type
             (:working-copy-status
              (jujutsu-status--format-status-line props))
             (:parent-commit-status
              (jujutsu-status--format-status-line props))
             (:newline "\n")
             (:verbatim props)
             (:text
              (-let* [((&hash :text text :face face) props)]
                (propertize text 'face face)))
             (:log-section-header
              (-let* [((&hash :upcase upcase :text text) props)
                      (text (if upcase (s-upcase text) text))]
                (propertize text 'face 'font-lock-keyword-face)))
             (:file-change
              (-let* [((&hash :file file :type change-type) props)]
                (jujutsu-status--format-single-file-change file change-type)))
             (:log-entry
              (jujutsu-log--format-log-entry props))
             (_ "")))
          (updated-result
           (concat
            result
            (propertize content 'jj-dom-id (sxhash-equal node))))]
    (if children
        (-reduce-from (lambda (acc child) (jujutsu-status--render-node child acc))
                      updated-result
                      children)
      updated-result)))

(defun jujutsu-status--update-node (node dom-id action)
  "Recursively update a NODE or its children based on DOM-ID and ACTION.
NODE is the current node being checked.
DOM-ID is the unique identifier of the node to update.
ACTION is the user action to apply to the node."
  (let ((node-id (sxhash-equal node)))
    (if (equal node-id dom-id)
        (jujutsu-status--update-node-by-action node action)
      (-let* [((&hash :children children) node)
              (updated-children (-map (lambda (child) (jujutsu-status--update-node child dom-id action)) children))]
        (if (equal updated-children children)
            node
          (ht-set node :children updated-children)
          node)))))

(defun jujutsu-status--update-tree (tree dom-id action)
  "Update the Jujutsu TREE based on user ACTION at DOM-ID.
TREE is the current Jujutsu status tree.
DOM-ID is the unique identifier of the node to update.
ACTION is the user action to apply to the node."
  (jujutsu-status--update-node tree dom-id action))


(defun jujutsu-status--update-node-by-action (node action)
  "Apply ACTION to NODE, updating its state accordingly.
NODE is the node to update.
ACTION is the user action to apply."
  (-let* [((&hash :type type :props props) node)]
    (message "DEBUG: type=%s, props=%s" type props)
     (cond
      ((and (eq type :file-change) (eq action 'toggle))
       (ht-set props :expanded (not (ht-get props :expanded)))
       node)
      ((and (eq type :log-section-header) (eq action 'toggle))
       (-let* [((&hash :upcase upcase) props)]
         (ht-set props :upcase (not upcase)))
       node)
      (t node))))

(defvar-local jujutsu-status-tree nil
  "Buffer-local variable to store the tree state.")

(defun jujutsu-status ()
  "Display the status of the current Jujutsu repository.

This command opens a new buffer named *jujutsu-status* showing:
- The working copy status
- The parent commit status
- A list of changes in the working copy
- A log of recent commits

The status buffer is interactive, allowing you to:
- Toggle file details with TAB
- Perform actions on commits or files with RET
- Use `g' to refresh the status
- Access additional commands via the `?' key

This provides a comprehensive overview of your repository's current state."
  (interactive)
  (with-current-buffer (get-buffer-create "*jujutsu-status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jujutsu-status-mode)
      (setq-local jujutsu-status-tree (jujutsu-status--make-tree))  ; Set the buffer-local variable
      (insert (jujutsu-status--render-tree jujutsu-status-tree)))
    (goto-char (point-min))
    (setq-local jujutsu-status-current-dom-id nil)  ; Initialize the buffer-local variable
    (switch-to-buffer "*jujutsu-status*")))

;; Action map using hash table
(defvar jujutsu-status-action-map
  (ht ('tab 'toggle)
      ('return 'enter)))

(defvar-local jujutsu-status-current-dom-id nil
  "Buffer-local variable to store the current dom-id.")

;; Dispatch function for user actions
(defun jujutsu-status-dispatch (action-key)
  "Dispatch a user action based on ACTION-KEY in the Jujutsu status buffer.
ACTION-KEY is the key representing the user action (e.g., `tab, `return)."
  (interactive)
  (let* ((pos (point))
         (dom-id (get-text-property pos 'jj-dom-id))
         (action (ht-get jujutsu-status-action-map action-key)))
    (when (and dom-id action)
      (message "DEBUG: action=%s, action-key=%s, dom-id=%s" action action-key dom-id)
      (setq jujutsu-status-current-dom-id dom-id)
      (setq-local jujutsu-status-tree (jujutsu-status--update-tree jujutsu-status-tree dom-id action))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (jujutsu-status--render-tree jujutsu-status-tree))
        (goto-char (point-min))
        (if-let ((new-pos (text-property-any (point-min) (point-max) 'jj-dom-id jujutsu-status-current-dom-id)))
            (goto-char new-pos)
          (message "Couldn't find the original position"))))))

(define-key jujutsu-status-mode-map (kbd "TAB") (lambda () (interactive) (jujutsu-status-dispatch 'tab)))
(define-key jujutsu-status-mode-map (kbd "RET") (lambda () (interactive) (jujutsu-status-dispatch 'return)))

(provide 'jujutsu-status)
;;; jujutsu-status.el ends here
