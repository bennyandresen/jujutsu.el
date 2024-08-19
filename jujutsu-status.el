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

;; Define a node structure
(defun make-jj-node (type &optional props children)
  (ht (:type type)
      (:props props)
      (:children children)))

;; Create the initial tree structure
(defun make-jj-tree ()
  (let* ((wc-status (jujutsu-core--get-status-data "@"))
         (p-status (jujutsu-core--get-status-data "@-"))
         (all-files (-concat (ht-get wc-status :files-added)
                             (ht-get wc-status :files-modified)
                             (ht-get wc-status :files-deleted))))
    (make-jj-node
     :root
     nil
     (list
      (make-jj-node
       :status-section
       nil
       (list
        (make-jj-node
         :status-header
         nil
         (list
          (make-jj-node :text (propertize "Working copy : " 'face 'font-lock-type-face))
          (make-jj-node :working-copy-status wc-status)
          (make-jj-node :newline)
          (make-jj-node :text (propertize "Parent commit: " 'face 'font-lock-type-face))
          (make-jj-node :parent-commit-status p-status)
          (make-jj-node :newline)))
        (make-jj-node :newline)
        (make-jj-node
         :working-copy-changes-header
         nil
         (list
          (make-jj-node
           :text
           (if (> (length all-files) 0)
               (propertize "Working copy changes:\n" 'face 'font-lock-keyword-face)
             (propertize "The working copy is clean\n" 'face 'font-lock-keyword-face)))
          (make-jj-node
           :working-copy-changes
           nil
           (-map (lambda (file)
                   (make-jj-node :file-change
                                 (ht (:file file)
                                     (:type (cond
                                             ((member file (ht-get wc-status :files-added)) "A")
                                             ((member file (ht-get wc-status :files-modified)) "M")
                                             ((member file (ht-get wc-status :files-deleted)) "D")))
                                     (:expanded nil))
                                 nil))
                 all-files))))))
      (make-jj-node :newline)
      (let* ((lentries (jujutsu-log--get-log-entries jujutsu-log-revset-fallback))
             (includes-root? (-some (lambda (m) (s-equals? (ht-get m :root) "true")) lentries)))
        (make-jj-node
         :log-section
         nil
         (-concat
          (list (make-jj-node :log-section-header (ht (:text "Log:")
                                                      (:upcase nil)))
                (make-jj-node :newline))
          (-map (lambda (entry)
                  (make-jj-node :log-entry entry nil))
                lentries)
          (when (not includes-root?) (list (make-jj-node :text "~"))))))))))

(-comment
 (with-current-buffer (get-buffer-create "*jj debug*")
   (erase-buffer)
   (->> (make-jj-tree)
        jujutsu-dev--ht-to-edn-pp
        insert)
   (display-buffer "*jj debug*"))
 1)

(defun render-jj-tree (tree)
  (render-node tree ""))

(defun render-node (node result)
  (-let* [((&hash :type type
                  :props props
                  :children children)
           node)
          (content
                   (cond
                    ((eq type :working-copy-status)
                     (jujutsu-status--format-status-line props))
                    ((eq type :parent-commit-status)
                     (jujutsu-status--format-status-line props))
                    ((eq type :newline)
                     "\n")
                    ((eq type :text)
                     props) ;; props as text
                    ((eq type :log-section-header)
                     (-let* [((&hash :upcase upcase
                                     :text text)
                              props)
                             (text (if upcase (s-upcase text) text))]
                       (propertize text 'face 'font-lock-keyword-face)))
                    ((eq type :file-change)
                     (-let* [((&hash :file file
                                     :type type)
                              props)]
                       (jujutsu-status--format-single-file-change
                        file
                        type)))
                    ((eq type :log-entry)
                     (jujutsu-log--format-log-entry props))
                    (t "")))
           (updated-result
            (concat
             result
             (propertize content
                         'jj-dom-id (sxhash-equal node))))]
    (if children
        (-reduce-from (lambda (acc child) (render-node child acc))
                      updated-result
                      children)
      updated-result)))

;; Update the tree based on user action
(defun update-jj-tree (tree dom-id action)
  (update-node tree dom-id action))

(defun update-node (node dom-id action)
  (let ((node-id (sxhash-equal node)))
    (if (equal node-id dom-id)
        (update-node-by-action node action)
      (let* ((children (ht-get node :children))
             (updated-children (-map (lambda (child) (update-node child dom-id action)) children)))
        (if (equal updated-children children)
            node
          (ht-set node :children updated-children)
          node)))))

(defun update-node-by-action (node action)
  (let* ((type (ht-get node :type))
         (props (ht-get node :props)))
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
  (interactive)
  (with-current-buffer (get-buffer-create "*jujutsu-status*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jujutsu-status-mode)
      (setq-local jujutsu-status-tree (make-jj-tree))  ; Set the buffer-local variable
      (insert (render-jj-tree jujutsu-status-tree)))
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
  (interactive)
  (let* ((pos (point))
         (dom-id (get-text-property pos 'jj-dom-id))
         (action (ht-get jujutsu-status-action-map action-key)))
    (when (and dom-id action)
      (message "DEBUG: action=%s, action-key=%s, dom-id=%s" action action-key dom-id)
      (setq jujutsu-status-current-dom-id dom-id)
      (setq-local jujutsu-status-tree (update-jj-tree jujutsu-status-tree dom-id action))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (render-jj-tree jujutsu-status-tree))
        (goto-char (point-min))
        (if-let ((new-pos (text-property-any (point-min) (point-max) 'jj-dom-id jujutsu-status-current-dom-id)))
            (goto-char new-pos)
          (message "Couldn't find the original position"))))))

(define-key jujutsu-status-mode-map (kbd "TAB") (lambda () (interactive) (jujutsu-status-dispatch 'tab)))
(define-key jujutsu-status-mode-map (kbd "RET") (lambda () (interactive) (jujutsu-status-dispatch 'return)))

(provide 'jujutsu-status)
;;; jujutsu-status.el ends here
