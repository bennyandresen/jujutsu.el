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

;; This module provides an interactive Jujutsu status viewer for Emacs.
;; It displays a comprehensive view of the current repository state,
;; including working copy status, parent commit status, file changes,
;; and a log of recent commits.
;;
;; The status buffer is built using a tree-like structure (implemented
;; in nx.el) which allows for efficient updates and rendering. Users
;; can interact with various elements in the buffer, such as expanding
;; file change details or performing actions on specific commits or files.
;;
;; Key features:
;; - Interactive buffer with expandable sections
;; - Integration with transient for command popups
;; - Efficient updates using a diff-based approach
;; - Customizable rendering and behavior
;;
;; Main entry point is the `jujutsu-status' command, which opens the
;; status buffer. Users can then navigate and interact with the buffer
;; using keybindings like TAB (toggle expansion) and RET (perform action).

;;; Code:
(require 'transient)
(require 'dash)
(require 's)
(require 'ht)

(require 'nx)
(require 'jujutsu-formatting)
(require 'jujutsu-core)
(require 'jujutsu-log)
(require 'jujutsu-diff)

(defun jujutsu-status--format-status-line (data)
  "Format a status line using DATA with fontification.

DATA is expected to be a hash table containing the following keys:
  :change-id-short      - Short change ID
  :change-id-shortest   - Shortest unique change ID
  :commit-id-short      - Short commit ID
  :commit-id-shortest   - Shortest unique commit ID
  :bookmarks            - Bookmark information (if any)
  :empty                - Whether the commit is empty
  :description          - Commit description

Returns a formatted string with appropriate text properties."
  (-let* [((&hash :change-id-short chids
                  :change-id-shortest chidss
                  :commit-id-short coids
                  :commit-id-shortest coidss
                  :bookmarks$list bookmarks
                  :empty$bool empty
                  :description desc)
           data)
          (empty (if empty
                     (propertize "(empty) " 'face 'warning)
                   ""))
          (bookmarks (if (> (length bookmarks) 0)
                         (s-concat (propertize (s-join " " bookmarks) 'face 'magit-branch-local)
                                   " | ")
                       ""))
          (change-id (jujutsu-formatting--format-id chids chidss))
          (commit-id (jujutsu-formatting--format-id coids coidss))
          (desc (if desc
                    (propertize desc 'face 'jujutsu-description-face)
                  (propertize "(no description set)" 'face 'warning)))]
    (format "%s %s %s%s%s" change-id commit-id bookmarks empty desc)))

(defun jujutsu-status--format-file-change-header (props)
  "Format a single file change based on the PROPS."
  (-let* [((&hash :type type
                  :filename filename
                  :expanded expanded)
           props)
          (change-type (pcase type (:added "A") (:modified "M") (:deleted "D")))
          (face (cond ((string= change-type "A") 'magit-diffstat-added)
                      ((string= change-type "M") 'diff-changed)
                      ((string= change-type "D") 'magit-diffstat-removed)))
          (fringe-prop-str
           (propertize " "
                       'face 'fringe
                       'display (list 'left-fringe
                                      (if expanded
                                          'jujutsu-fringe-triangle-down
                                        'jujutsu-fringe-triangle-right))))]
    (s-concat
     fringe-prop-str
     (propertize (format "%s %s\n" change-type filename)
                 'face face))))

(-tests
 ;; XXX: -tests can't handle let-bindings yet
 (setq jj--crude-test-props (ht (:filename "jujutsu-diff.el")
                                (:type :modified)
                                (:hunks-headers '("@@ -151,7 +162,8 @@"
                                                  "@@ -22,6 +22,17 @@"))))
 (jujutsu-status--format-file-change-header (ht-merge jj--crude-test-props (ht (:expanded nil))))
 := " M jujutsu-diff.el\n"
 (jujutsu-status--format-file-change-header (ht-merge jj--crude-test-props (ht (:expanded t))))
 := " M jujutsu-diff.el\n@@ -151,7 +162,8 @@\n@@ -22,6 +22,17 @@\n")

(defun jujutsu-status--format-file-change-diff-hunk-header (props)
  "Format a single file change based on the PROPS."
  (-let* [((&hash :type type
                  :header header
                  :expanded expanded)
           props)]
    (s-concat
     (propertize " " 'display (list 'left-fringe
                                    (if expanded
                                        'jujutsu-fringe-triangle-down
                                      'jujutsu-fringe-triangle-right)))
     (propertize header 'face 'magit-diff-hunk-heading-highlight)
     "\n")))

(defun jujutsu-status--format-file-change-diff-hunk-content (props)
  "Format the diff content passed through via PROPS."
  (-let* [((&hash :content content)
           props)]
    (s-join "\n"
            (-concat
             (jujutsu-diff--create-side-by-side-diff content)
             (list "")))))

(define-derived-mode jujutsu-status-mode special-mode "jujutsu status"
  "Major mode for displaying Jujutsu status."
  :group 'jujutsu
  (setq buffer-read-only t))

(define-key jujutsu-status-mode-map (kbd "g") #'jujutsu-status)

(defun jujutsu-status-squash (args)
  "Run jj squash with ARGS and metadata context."
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

(defun jujutsu-status--get-props-at-point (point)
  "Get metadata for the node at POINT in the jujutsu status buffer."
  (let* ((dom-id (get-text-property point 'nx/id))
         (node (when dom-id
                 (jujutsu-status--find-node-by-id jujutsu-status-app-state dom-id))))
    (when node (ht-get node :props))))

(transient-define-prefix jujutsu-status-squash-popup ()
  "Popup for jj squash options, with context from current buffer position."
  :init-value (lambda (obj)
                (let ((metadata (jujutsu-status--get-props-at-point (point))))
                  (oset obj value (list
                                   (when (ht-get metadata :filename)
                                     (format "--file=%s" (ht-get metadata :filename)))
                                   (when (and (eq (ht-get metadata :type) 'hunk)
                                              (ht-get metadata :header))
                                     (format "--hunk=%s" (ht-get metadata :header)))))))
  ["Options"
   ("-I" "Ignore immutable" "--ignore-immutable")
   ("-f" "File" "--file="
    :init-value (lambda (obj)
                  (let ((metadata (jujutsu-status--get-metadata-at-point (point))))
                    (when (ht-get metadata :filename)
                      (oset obj value (ht-get metadata :filename))))))
   ("-h" "Hunk" "--hunk="
    :init-value (lambda (obj)
                  (let ((metadata (jujutsu-status--get-metadata-at-point (point))))
                    (when (and (eq (ht-get metadata :type) 'hunk)
                               (ht-get metadata :header))
                      (oset obj value (ht-get metadata :header))))))]
  ["Actions"
   ("s" "Squash" jujutsu-status-squash)])

(transient-define-prefix jujutsu-status-branch-popup ()
  "Popup for jj branch options."
  ["Actions"
   ;; ("c" "Create" jujutsu-status-branch-create-popup)
   ("s" "Set" jujutsu-status-branch-set-popup)])

(transient-define-prefix jujutsu-status-branch-set-popup ()
  "Popup for \"jj branch set\" options."
  :init-value (lambda (obj)
                (-let* [(node-props (jujutsu-status--get-props-at-point (point)))
                        ((&hash :change-id-shortest chidst) node-props)]
                  (oset obj value (list
                                   (when chidst (format "--revision=%s" chidst))))))
  ["Options"
   ("-r" "Revision" "--revision=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-r ") "\"")))]
  ["Actions"
   ("s" "Set" jujutsu-status-branch-set)])

(defun jujutsu-status-branch-set (args)
  (interactive (list (transient-args 'jujutsu-status-branch-set-popup)))
  (message "NOT IMPLEMENTED YET"))

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
   ("b" "Branch" jujutsu-status-branch-popup)
   ("s" "Squash change" jujutsu-status-squash-popup)
   ("d" "Describe change" jujutsu-status-describe-popup)
   ("n" "New change" jujutsu-status-popup)])

(define-key jujutsu-status-mode-map (kbd "?")    #'jujutsu-status-popup)
(define-key jujutsu-status-mode-map (kbd "a")    #'jujutsu-status-abandon-popup)
(define-key jujutsu-status-mode-map (kbd "b")    #'jujutsu-status-branch-popup)
(define-key jujutsu-status-mode-map (kbd "s")    #'jujutsu-status-squash-popup)
(define-key jujutsu-status-mode-map (kbd "d")    #'jujutsu-status-describe-popup)
(define-key jujutsu-status-mode-map (kbd "n")    #'jujutsu-status-new-popup)

(defun jujutsu-status--make-status-section (wc-status p-status)
  "Create the status section of the tree using WC-STATUS and P-STATUS."
  (nx :status-section (ht)
      (list
       (nx :status-header (ht)
           (list
            (nx :text (ht (:text "Working copy : ")
                          (:face 'font-lock-type-face)))
            (nx :working-copy-status wc-status)
            (nx :newline)
            (nx :text (ht (:text "Parent commit: ")
                          (:face 'font-lock-type-face)))
            (nx :parent-commit-status p-status)
            (nx :newline))))))

(defun jujutsu-status--make-file-change (status-data filename)
  "Create a file change node for FILENAME based on STATUS-DATA.

STATUS-DATA is a hash table containing repository status information.
FILENAME is the name of the file to create a change node for.

Returns an nx node representing the file change."
  (-let* [((&hash :files-added added
                  :files-modified modified
                  :files-deleted deleted
                  :change-id-short chids)
           status-data)
          (type (cond ((member filename added) :added)
                      ((member filename modified) :modified)
                      ((member filename deleted) :deleted)))]
    (nx :file-change (ht (:filename filename)
                         (:change-id-short chids)
                         (:type type))
        (-concat
         (list
          (nx :file-change-header (ht (:filename filename)
                                      (:type type)
                                      (:expanded nil)
                                      (:change-id-short chids))))))))

(-comment
 (->
  (jujutsu-status--make-file-change
   (jujutsu-core--get-status-data "@")
   "nx-test.el")
  jujutsu-dev--display-in-buffer)

 )

(defun jujutsu-status--make-working-copy-changes (wc-status)
  "Create the working copy changes section of the tree using WC-STATUS."
  (-let* [((&hash :files-added added
                  :files-modified modified
                  :files-deleted deleted
                  :change-id-short chids)
           wc-status)
          (all-files (-concat added modified deleted))]
    (nx :working-copy-changes (ht (:change-id-short chids))
        (list
         (nx :working-copy-changes-header (ht)
             (list
              (nx :text (ht (:text (if (> (length all-files) 0)
                                       "Working copy changes:"
                                     "The working copy is clean"))
                            (:face 'font-lock-keyword-face)))))
         (nx :newline)
         (nx :working-copy-changes-content (ht)
             (-map (lambda (filename)
                     (jujutsu-status--make-file-change wc-status filename))
                   all-files))))))

(-comment
 (-> (jujutsu-core--get-status-data "@")
     jujutsu-status--make-working-copy-changes
     jujutsu-dev--display-in-buffer)
 1)

(defun jujutsu-status--make-log-section ()
  "Create the log section of the tree."
  (let* ((lentries (jujutsu-log--get-log-entries jujutsu-log-revset-fallback))
         (includes-root? (-some (lambda (m) (ht-get m :root$bool)) lentries)))
    (nx :log-section (ht)
        (-concat
         (list (nx :log-section-header (ht (:text "Log:")
                                           (:upcase nil)))
               (nx :newline))
         (-map (lambda (entry)
                 (nx :log-entry entry nil))
               lentries)
         (when (not includes-root?) (list (nx :text (ht (:text "~") (:face 'default)))))))))

(defun jujutsu-status--make-tree ()
  "Construct the complete Jujutsu status tree structure."
  (let* ((wc-status (jujutsu-core--get-status-data "@"))
         (p-status (jujutsu-core--get-status-data "@-")))
    (nx :root (ht)
        (list
         (jujutsu-status--make-status-section wc-status p-status)
         (nx :newline)
         (jujutsu-status--make-working-copy-changes wc-status)
         (nx :newline)
         (jujutsu-status--make-log-section)))))

(-comment
 (->> (jujutsu-status--make-tree)
      jujutsu-dev--display-in-buffer)
 1)

(defun jujutsu-status--render-tree (tree)
  "Render the Jujutsu TREE structure as a string.

TREE is the root node of the Jujutsu status tree.

Returns a string representation of the entire tree, with text properties
for interactive functionality."
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
             (:file-change-header
              (jujutsu-status--format-file-change-header props))
             (:file-change-diff-hunk-header
              (jujutsu-status--format-file-change-diff-hunk-header props))
             (:file-change-diff-hunk-content
              (jujutsu-status--format-file-change-diff-hunk-content props))
             (:log-entry
              (jujutsu-log--format-log-entry props))
             (_ "")))
          (updated-result
           (concat
            result
            (propertize content 'nx/id (nx-id node))))]
    (if children
        (-reduce-from (lambda (acc child) (jujutsu-status--render-node child acc))
                      updated-result
                      children)
      updated-result)))

(defun jujutsu-status--render-children (children result)
  "Render CHILDREN nodes, appending to RESULT."
  (-reduce-from (lambda (acc child) (jujutsu-status--render-node child acc))
                result
                children))

(-comment
 (propertize "foo" 'nx/id (nx-id (nx :foo)))

 )

(defun jujutsu-status--update-node (node dom-id action)
  "Recursively update a NODE or its children based on DOM-ID and ACTION.

NODE is the current node being checked.
DOM-ID is the unique identifier of the node to update.
ACTION is the user action to apply to the node. Possible values are:
  'toggle  - Toggle expansion state of the node
  'enter   - Perform the default action for the node type

Returns the updated node or its original state if no update was necessary."
  (let ((node-id (nx-id node)))
    (if (equal node-id dom-id)
        (jujutsu-status--update-node-by-action node action)
      (-let* [((&hash :children children) node)
              (updated-children (-map (lambda (child) (jujutsu-status--update-node child dom-id action)) children))]
        (if (equal updated-children children)
            node
          (ht-set node :children updated-children)
          node)))))

(defun jujutsu-status--update-file-change-header (node action)
  "Update file change header NODE based on ACTION."
  (when (eq action 'toggle)
    (-let* [(props (ht-get node :props))
            ((&hash :type change-type
                    :filename filename
                    :change-id-short chids
                    :expanded expanded
                    :hunks hunks)
             props)
            (expanded-fut (not expanded))]
      (ht-set props :expanded expanded-fut)
      (when (and (null hunks) expanded-fut)
        (ht-set props :hunks (--> (jujutsu-diff--run filename chids)
                                  jujutsu-diff--split-git-diff-by-file
                                  jujutsu-diff--parse-diffs
                                  (ht-get* it filename :diff-content)
                                  jujutsu-diff--split-git-diff-into-hunks
                                  jujutsu-diff--parse-hunks))
        (setq hunks (ht-get props :hunks)))
      (ht-set node :children
              (when expanded-fut
                ;; ht-map would be cleaner, but reverses the order
                (-map (lambda (header)
                        (nx :file-change-diff-hunk-header
                            (ht (:header header)
                                (:contents (ht-get hunks header))
                                (:filename filename)
                                (:type change-type)
                                (:expanded nil))))
                      (reverse (ht-keys hunks)))))
      node)))

(-comment
 (ht-map (lambda (k v) (list "k" k)) (ht (:foo "bar")
                                         (:bar "baz")))

 )

(defun jujutsu-status--update-file-change-diff-hunk-header (node action)
  "Update file change diff hunk header NODE based on ACTION."
  (when (eq action 'toggle)
    (-let* [(props (ht-get node :props))
            ((&hash :contents contents)
             props)
            (expanded-fut (not (ht-get props :expanded)))]
      (ht-set props :expanded expanded-fut)
      (ht-set node :children
              (when expanded-fut
                (list (nx :file-change-diff-hunk-content (ht (:content contents))))))))
  node)

(defun jujutsu-status--update-log-section-header (node action)
  "Update log section header NODE based on ACTION."
  (when (eq action 'toggle)
    (let ((props (ht-get node :props)))
      (ht-set props :upcase (not (ht-get props :upcase)))
      (ht-set node :props props)))
  node)

(defvar jujutsu-status--node-action-handlers
  (ht (:file-change-header
       #'jujutsu-status--update-file-change-header)
      (:file-change-diff-hunk-header
       #'jujutsu-status--update-file-change-diff-hunk-header)
      (:log-section-header
       #'jujutsu-status--update-log-section-header))
  "Dispatch table for node update actions.

This hash table maps node types to their corresponding update
handler functions. When a user action is performed on a node,
the appropriate handler is called based on the node's type.")

(defun jujutsu-status--update-node-by-action (node action)
  "Apply ACTION to NODE, updating its state accordingly.
NODE is the node to update.
ACTION is the user action to apply."
  (let* ((type (ht-get node :type))
         (handler (ht-get jujutsu-status--node-action-handlers type)))
    (if handler
        (funcall handler node action)
      node)) ; Return the node unchanged if no handler is found
  (when (bound-and-true-p jujutsu-dev-dump-user-actions)
    (jujutsu-dev-dump-display (ht (:action action)
                                  (:node node))))
  node)

(defun jujutsu-status--update-tree (tree dom-id action)
  "Update the Jujutsu TREE based on user ACTION at DOM-ID.
TREE is the current Jujutsu status tree.
DOM-ID is the unique identifier of the node to update.
ACTION is the user action to apply to the node."
  (jujutsu-status--update-node tree dom-id action))

(defvar-local jujutsu-status-app-state nil
  "The application state for the jujutsu status buffer.

This variable holds the current tree structure representing the
entire state of the Jujutsu status view. It is updated whenever
the user interacts with the buffer or when the status is refreshed.

The structure is a nested tree of nx nodes, each representing a
different part of the Jujutsu status (e.g., working copy status,
file changes, commit log).")
(defvar-local jujutsu-status-previous-state nil)

(defun jujutsu-status ()
  "Display the status of the current Jujutsu repository.

This command opens a new buffer showing:
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
  (let* ((project-name (jujutsu-core--get-project-name))
         (buffer-name (format "jujutsu: %s" project-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jujutsu-status-mode)
        ;; v Set the initial app-state
        (setq-local jujutsu-status-app-state (jujutsu-status--make-tree))
        (setq-local jujutsu-status-previous-state nil)
        ;; ^ Initialize previous state
        (jujutsu-status-render))
      (goto-char (point-min))
      (setq-local jujutsu-status-current-dom-id nil)
      ;; XXX: not nice
      (when (fboundp 'hl-line-mode)
        (hl-line-mode -1))
      (switch-to-buffer buffer-name))))

(defvar-local jujutsu-status-current-dom-id nil
  "Buffer-local variable to store the current dom-id.")

(defun jujutsu-status-dispatch-event (event-type &rest args)
  "Dispatch an event of EVENT-TYPE with ARGS."
  (pcase event-type
    ('toggle (jujutsu-status-dispatch 'toggle))
    ('refresh (jujutsu-status))))

(defun jujutsu-status-render--from-scratch ()
  "Render the current application state from scratch."
  (let ((inhibit-read-only t)
        (new-state jujutsu-status-app-state))
    (erase-buffer)
    (insert (jujutsu-status--render-tree jujutsu-status-app-state))
    (setq jujutsu-status-previous-state new-state)))

(defun jujutsu-status-render--buffer-reconciliation ()
  "Render the current application state using the diffing algorithm."
  (let* ((inhibit-read-only t)
         (old-state (if (eq jujutsu-status-previous-state nil)
                        (ht)
                      (nx-copy jujutsu-status-previous-state)))
         (new-state (nx-copy jujutsu-status-app-state))
         (diff-ops (nx-diff-trees old-state new-state)))
    (unless (nx? old-state)
      ;; very first insert has to be a full render
      (insert (jujutsu-status--render-tree new-state)))
    (nx-buffer-apply-diff (current-buffer)
                          (ht (:diff-ops diff-ops) (:state new-state))
                          #'jujutsu-status--render-node)
    (setq jujutsu-status-previous-state new-state)))

(defalias 'jujutsu-status-render (symbol-function 'jujutsu-status-render-from-scratch))

(defun jujutsu-status-update-state (updater)
  "Update the application state using UPDATER function."
  (let ((new-state (funcall updater jujutsu-status-app-state)))
    (setq jujutsu-status-app-state new-state)
    (jujutsu-status-render)))

;; Dispatch function for user actions
(defun jujutsu-status-dispatch (action)
  "Dispatch a user action based on ACTION in the Jujutsu status buffer."
  (interactive)
  (let* ((pos (point))
         (dom-id (get-text-property pos 'nx/id)))
    (when dom-id
      (pcase action
        ((or 'toggle 'enter)
         (setq jujutsu-status-current-dom-id dom-id)
         (jujutsu-status-update-state
          (lambda (state)
            (jujutsu-status--update-tree state dom-id action)))
         (if-let ((new-pos (text-property-any
                            (point-min)
                            (point-max)
                            'nx/id
                            jujutsu-status-current-dom-id)))
             (goto-char new-pos)
           (message "Couldn't find the original position")))
        (_ (message "Unknown action: %s" action))))))

(defun jujutsu-status--find-node-by-id (tree id)
  "Find a node in TREE with the given ID."
  (if (equal (nx-id tree) id)
      tree
    (catch 'found
      (dolist (child (nx-children tree))
        (let ((result (jujutsu-status--find-node-by-id child id)))
          (when result
            (throw 'found result))))
      nil)))

(define-key jujutsu-status-mode-map (kbd "TAB") (lambda () (interactive) (jujutsu-status-dispatch 'toggle)))
(define-key jujutsu-status-mode-map (kbd "RET") (lambda () (interactive) (jujutsu-status-dispatch 'enter)))

(-comment
 (ht-get* foobar :children)

 )

(provide 'jujutsu-status)
;;; jujutsu-status.el ends here
