;;; jujutsu-log.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 19, 2024
;; Modified: August 19, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/jujutsu-log
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'ht)
(require 'dash)
(require 's)

(require 'jujutsu-core)
(require 'jujutsu-formatting)

(defcustom jujutsu-log-revset-fallback "@ | ancestors(immutable_heads().., 2) | trunk()"
  "Default value when no revset is specified."
  :group 'jujutsu
  :type 'string)

(defun jujutsu-log--get-log-entries (&optional revset)
  "Get status data for the given REVSET."
  (let ((revset (or revset jujutsu-log-revset-fallback))
        (template (ht (:change-id-short "change_id.short(8)")
                      (:change-id-shortest "change_id.shortest()")
                      (:commit-id-short "commit_id.short(8)")
                      (:commit-id-shortest "commit_id.shortest()")
                      (:empty "empty")
                      (:branches "branches")
                      (:hidden "hidden")
                      (:author-email "author.email()")
                      (:timestamp "author.timestamp().format(\\\"%Y-%m-%d %H:%M:%S\\\")")
                      (:current-working-copy "current_working_copy")
                      (:remote-branches "remote_branches")
                      (:git-head "git_head")
                      (:root "root")
                      (:immutable "immutable")
                      (:description "description"))))
    (--> (jujutsu-core--map-to-escaped-string template)
        (jujutsu-core--log-w/template it revset)
        (jujutsu-core--split-string-on-empty-lines it)
        (-map #'jujutsu-core--parse-string-to-map it))))

(defun jujutsu-log--format-log-entry (data)
  "Format a status entry using DATA with fontification."
  (-let* [((&hash :change-id-short chids :change-id-shortest chidss
                  :commit-id-short coids :commit-id-shortest coidss
                  :branches branches :empty empty :description desc
                  :root root :author-email author-email
                  :timestamp timestamp :immutable immutable
                  :current-working-copy cwc)
           data)
          (node (cond ((s-equals? cwc "true")
                       (propertize "@" 'face 'magit-keyword))
                      ((s-equals? immutable "true")
                       (propertize "◆" 'face 'magit-log-date))
                      (t
                       "○")))
          (author-email (if author-email
                            (propertize author-email 'face 'warning)
                          ""))
          (root (if (s-equals? root "true") t nil))
          (empty (if (s-equals? empty "true")
                     (propertize "(empty) " 'face 'warning)
                   ""))
          (timestamp (if timestamp (propertize timestamp 'face 'magit-log-date)
                       ""))
          (branches (if branches
                        (s-concat (propertize branches 'face 'magit-branch-local) " ")
                      ""))
          (change-id (jujutsu-formatting--format-id chids chidss))
          (commit-id (jujutsu-formatting--format-id coids coidss))
          (desc (if desc
                    (propertize desc 'face 'jujutsu-description-face)
                  (propertize "(no description set)" 'face 'warning)))]
    (if root
        (format "%s  %s %s %s\n"
                node
                change-id
                (propertize "root()" 'face 'magit-keyword)
                commit-id)
      (s-join ""
              (list
               (format "%s  %s %s %s %s%s" node change-id author-email timestamp branches commit-id)
               "\n"
               (format "│  %s%s\n" empty desc))))))

(provide 'jujutsu-log)
;;; jujutsu-log.el ends here
