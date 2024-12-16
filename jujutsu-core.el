;;; jujutsu-core.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 19, 2024
;; Modified: August 19, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/jujutsu-core
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 's)
(require 'ht)
(require 'dash)
(require 'dash-x)

(require 'jujutsu-vars)

(defun jujutsu-core--template-list (s)
  "Format string S as a semicolon-joined list in jujutsu template syntax.
Wraps S with .join(\";\") for use in jujutsu templates where multiple
values need to be concatenated into a single string."
  (s-concat s ".join(\\\";\\\")"))

(defun jujutsu-core--find-project-root ()
  "Find the root directory of the Jujutsu project."
  (locate-dominating-file default-directory ".jj"))

(defun jujutsu-core--get-project-name ()
  "Get the directory name of the project root."
  (-> (jujutsu-core--find-project-root)
      file-name-directory
      directory-file-name
      file-name-nondirectory))

(-comment
 (jujutsu-core--get-project-name)
 1)

(defun jujutsu-core--run-command (command &optional use-personal-config)
  "Run a jj COMMAND from the project root and return its output as a string.
If USE-PERSONAL-CONFIG is t, run jj without suppressing the user config."
  (let* ((default-directory (or (jujutsu-core--find-project-root)
                                (error "Not in a Jujutsu project")))
         (jj-cmds (list "jj" "--no-pager" "--color" "never" command))
         (cmd-list (if use-personal-config
                       jj-cmds
                     (-concat '("env" "JJ_CONFIG=/dev/null") jj-cmds)))
         (cmd-string (s-join " " cmd-list)))
    (shell-command-to-string cmd-string)))

(defun jujutsu-core--show-w/template (template &optional rev)
  "Run `jj show' command with a custom TEMPLATE and optional REV.

TEMPLATE is a string containing the custom template for the `jj show' command.
REV is an optional revision specifier. If not provided, it defaults to '@'

This function constructs and executes a `jj show' command with the given
template and revision, returning the command's output as a string."
  (let* ((rev (or rev "@"))
         (formatted (format "show --summary --template \"%s\" %s"
                            template
                            rev)))
    (jujutsu-core--run-command formatted)))

(defun jujutsu-core--log-w/template (template &optional revset)
  "Run `jj log' command with a custom TEMPLATE and optional REVSET.

TEMPLATE is a string containing the custom template for the `jj log' command.

This function constructs and executes a `jj log' command with the given
template, disabling graph output and adding newlines between entries. It returns
the command's output as a string, with each log entry separated by newlines."
  (let* ((revset (or revset jujutsu-log-revset-fallback))
         (formatted (format "log --revisions \"%s\" --no-graph --template \"%s ++ \\\"\\n\\n\\\"\""
                            revset
                            template)))
    (jujutsu-core--run-command formatted)))

(defun jujutsu-core--file-list ()
  "Get the list of files from `jj file list' command."
  (--> "file list"
       jujutsu-core--run-command
       (s-split "\n" it t)))

(defun jujutsu-core--map-to-escaped-string (map)
  "Convert MAP (hash-table) to an escaped string for use as a jj template."
  (let ((k->s (lambda (k) (if (keywordp k)
                              (intern (s-replace "$" "\\$" (substring (symbol-name k) 1)))
                            k))))
    (->> map
         (ht-map (lambda (key value)
                   (format "\\\"%s \\\" ++ %s ++ \\\"\\\\n\\\""
                           (funcall k->s key) value)))
         (s-join " ++ "))))

(-tests
 (let ((m (ht (:foo nil)
              (:foo$bool "hello world")
              (:bar 1))))
   (jujutsu-core--map-to-escaped-string m))
 :=
 "\\\"bar \\\" ++ 1 ++ \\\"\\\\n\\\" ++ \\\"foo \\\" ++ nil ++ \\\"\\\\n\\\"")

(defun jujutsu-core--parse-file-change (line)
  "Parse a file change LINE into a hash-table."
  (-let* [(regex (rx bos (group (any "AMD")) " " (group (+ not-newline)) eos) line)
          ((res m1 m2) (s-match regex line))]
    (when res
      (ht (m1 m2)))))

(defun jujutsu-core--parse-key-value (line)
  "Parse a KEY-VALUE LINE into a hash-table.
If the key contains `:list', the value is split based on `;'."
  (let ((s->kw (lambda (s) (intern (s-prepend ":" s)))))
    (unless (s-matches? (rx bos (any "AMD") " ") line)
      (-when-let* [(regex (rx bos
                              (group (+ (not (any " ")))) ; key
                              " "
                              (optional (group (+ not-newline))) ; optional value
                              eos))
                   ((res m1 m2) (s-match regex line))]
        (let* ((key (funcall s->kw m1))
               (value (cond
                       ((s-ends-with? "$list" (symbol-name key))
                        (s-split ";" m2 t))
                       ((s-ends-with? "$bool" (symbol-name key))
                        (s-equals? m2 "true"))
                       (t m2))))
          (ht (key value)))))))

(-comment
 ;; Regular key-value pair
 (jujutsu-core--parse-key-value "commit-id abc123")
 ;; => #s(hash-table ... (:commit-id "abc123"))

 ;; List key-value pair
 (jujutsu-core--parse-key-value "bookmarks$list main;feature-1;hotfix")
 ;; => #s(hash-table ... (:bookmarks$list ("main" "feature-1" "hotfix")))

 ;; Key with "$list" but no semicolons
 (jujutsu-core--parse-key-value "files$list file1.txt")
 ;; => #s(hash-table ... (:files$list ("file1.txt")))

 ;; Regular key with semicolons (not treated as a list)
 (jujutsu-core--parse-key-value "description Some; text; here")
 ;; => #s(hash-table ... (:description "Some; text; here"))

 ;; Boolean key-value pair
 (jujutsu-core--parse-key-value "empty$bool true")
 ;; => #s(hash-table ... (:empty$bool t))
 (jujutsu-core--parse-key-value "empty$bool false")
 ;; => #s(hash-table ... (:empty$bool nil))

 ,)

(defun jujutsu-core--parse-and-group-file-changes (file-changes)
  "Parse and group FILE-CHANGES by their change type into a hash-table."
  (let ((grouped-changes (ht (:files-added nil)
                             (:files-modified nil)
                             (:files-deleted nil))))
    (when-let ((parsed-changes (-map #'jujutsu-core--parse-file-change file-changes)))
      (dolist (change parsed-changes)
        (-let* [((&hash "A" a-file "M" m-file "D" d-file) change)
                ((&hash :files-added a-coll :files-modified m-coll :files-deleted d-coll) grouped-changes)]
          (when a-file (ht-set! grouped-changes :files-added (-concat a-coll (list a-file))))
          (when m-file (ht-set! grouped-changes :files-modified (-concat m-coll (list m-file))))
          (when d-file (ht-set! grouped-changes :files-deleted (-concat d-coll (list d-file)))))))
    grouped-changes))

(defun jujutsu-core--parse-string-to-map (input-string)
  "Parse INPUT-STRING into a hash-table and an organized list of file change."
  (let* ((lines (s-split "\n" input-string t))
         (file-change-lines (-filter #'jujutsu-core--parse-file-change lines))
         (grouped-file-changes (jujutsu-core--parse-and-group-file-changes file-change-lines))
         (key-values (-keep #'jujutsu-core--parse-key-value lines))
         (result-map (apply #'ht-merge key-values)))
    (ht-merge result-map grouped-file-changes)))

(defun jujutsu-core--split-string-on-empty-lines (input-string)
  "Split INPUT-STRING into multiple strings based on empty lines."
  (let ((rx-split (rx bol (zero-or-more space) eol
                      (one-or-more (any space ?\n))
                      bol (zero-or-more space) eol)))
    (s-split rx-split input-string t)))

(defun jujutsu-core--get-status-data (rev)
  "Get status data for the given REV."
  (let ((template (ht (:change-id-short "change_id.short(8)")
                      (:change-id-shortest "change_id.shortest()")
                      (:commit-id-short "commit_id.short(8)")
                      (:commit-id-shortest "commit_id.shortest()")
                      (:empty$bool "empty")
                      (:bookmarks$list (jujutsu-core--template-list "bookmarks"))
                      (:git-head$bool "git_head")
                      (:description "description.first_line()"))))
    (-> (jujutsu-core--map-to-escaped-string template)
        (jujutsu-core--show-w/template rev)
        jujutsu-core--parse-string-to-map)))

(-comment
 (jujutsu-core--get-status-data "@-")

 foo-template
 )


(provide 'jujutsu-core)
;;; jujutsu-core.el ends here
