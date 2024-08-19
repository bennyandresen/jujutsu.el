;;; jujutsu-diff.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 12, 2024
;; Modified: August 12, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/bennyandresen/jujutsu-diff
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'jujutsu-dash)
(require 's)

(defun jujutsu-diff--split-git-diff (diff-output)
  "Split DIFF-OUTPUT into separate diffs for each file."
  (let ((file-diffs '())
        (current-diff "")
        (diff-header-regex "^diff --git a/\\(.+\\) b/\\1$"))
    (dolist (line (split-string diff-output "\n"))
      (if (string-match diff-header-regex line)
          (progn
            (unless (s-blank? current-diff)
              (push current-diff file-diffs))
            (setq current-diff line))
        (setq current-diff (concat current-diff "\n" line))))
    (unless (string-empty-p current-diff)
      (push current-diff file-diffs))
    (nreverse file-diffs)))

(-tests
 (->> "resources/test-diff-git.output"
      -slurp
      jujutsu-diff--split-git-diff
      (nth 1))
 1)

(defun jujutsu-diff--split-git-diff-into-hunks (diff-output)
  "Split DIFF-OUTPUT into hunks based on '@@' markers."
  (with-temp-buffer
    (insert diff-output)
    (goto-char (point-min))
    (let ((hunks '())
          (hunk-start (point-min))
          (hunk-end (point-min)))
      (while (re-search-forward "^@@" nil t)
        (setq hunk-end (match-beginning 0))
        (when (> hunk-end hunk-start)
          (push (buffer-substring-no-properties hunk-start hunk-end) hunks))
        (setq hunk-start hunk-end))
      (push (buffer-substring-no-properties hunk-start (point-max)) hunks)
      (nreverse hunks))))

(-comment
 (->> "resources/test-diff-git.output"
      -slurp
      jujutsu-diff--split-git-diff
      (nth 1)
      jujutsu-diff--split-git-diff-into-hunks
      (nth 2))
 1)

(defun jujutsu-diff--parse-diff-hunk (hunk-str)
  (let* ((lines (split-string hunk-str "\n"))
         (header (car lines))
         (content (cdr lines))
         (result (list (ht-create))))
    (ht-set! (car result) :type :header)
    (ht-set! (car result) :content header)
    (dolist (line content)
      (let ((entry (ht-create)))
        (cond
         ((string-prefix-p "-" line)
          (ht-set! entry :type :removed)
          (ht-set! entry :content (substring line 1)))
         ((string-prefix-p "+" line)
          (ht-set! entry :type :added)
          (ht-set! entry :content (substring line 1)))
         (t
          (ht-set! entry :type :context)
          (ht-set! entry :content line)))
        (push entry result)))
    (nreverse result)))

(-tests
 (->> "resources/test-diff-git.output"
      -slurp
      jujutsu-diff--split-git-diff
      (nth 1)
      jujutsu-diff--split-git-diff-into-hunks
      (nth 3)
      jujutsu-diff--parse-diff-hunk
      jujutsu-dev--ht-to-edn-pp)
 :=
 "[{:type :header, :content \"@@ -75,7 +82,7 @@\"}
 {:type :context, :content \"        (s-split \\\"\\\\n\\\" it t)))\"}
 {:type :context, :content \"\"}
 {:type :context, :content \" (defun jj--map-to-escaped-string (map)\"}
 {:type :removed,
  :content \"  \\\"Convert MAP (hash-table) to an escaped string.\\\"\"}
 {:type :added,
  :content \"  \\\"Convert MAP (hash-table) to an escaped string for use as a jj template.\\\"\"}
 {:type :context, :content \"   (->> map\"}
 {:type :context, :content \"        (ht-map (lambda (key value)\"}
 {:type :context,
  :content \"                  (format \\\"\\\\\\\\\\\\\\\"%s \\\\\\\\\\\\\\\" ++ %s ++ \\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\n\\\\\\\\\\\\\\\"\\\"\"}
 {:type :context, :content \"\"}]
")

(defun jujutsu-diff--max-content-width (chunks type)
  "Calculate the maximum content width for CHUNKS of given TYPE."
  (->> chunks
       (--filter (or (eq (ht-get it :type) :context)
                     (eq (ht-get it :type) type)))
       (--map (length (ht-get it :content)))
       (-max)))

(defun jujutsu-diff--pad-string (s width)
  "Pad string S to WIDTH."
  (format (format "%%-%ds" width) (or s "")))

(defun jujutsu-diff--format-line (left right left-width right-width separator)
  "Format LEFT and RIGHT content with given widths (LEFT-WIDTH, RIGHT-WIDTH) and SEPARATOR."
  (let* ((left-content (ht-get left :content))
         (right-content (ht-get right :content))
         (left-type (ht-get left :type))
         (right-type (ht-get right :type))
         (left-formatted (jujutsu-diff--pad-string left-content left-width))
         (right-formatted (jujutsu-diff--pad-string right-content right-width))
         (is-context (and (eq left-type :context) (eq right-type :context))))
    (concat
     (if is-context
         (propertize left-formatted 'face 'magit-diff-context)
       (propertize left-formatted 'face 'magit-diff-removed-highlight))
     separator
     (if is-context
         (propertize right-formatted 'face 'magit-diff-context)
       (propertize right-formatted 'face 'magit-diff-added-highlight)))))

(defun jujutsu-diff--format-header (content total-width)
  "Format header CONTENT to TOTAL-WIDTH."
  (propertize (jujutsu-diff--pad-string content total-width) 'face 'magit-diff-hunk-heading))

(defun jujutsu-diff--process-chunk (acc chunk left-width right-width separator)
  "Process a single CHUNK, updating the accumulator ACC."
  (let ((formatted (car acc))
        (removed (cdr acc))
        (type (ht-get chunk :type))
        (content (ht-get chunk :content)))
    (pcase type
      (:header
       (cons (cons (jujutsu-diff--format-header content (+ left-width (length separator) right-width))
                   formatted)
             removed))
      (:context
       (cons (cons (jujutsu-diff--format-line chunk chunk left-width right-width separator)
                   formatted)
             removed))
      (:removed
       (cons formatted (cons chunk removed)))
      (:added
       (if (null removed)
           (cons (cons (jujutsu-diff--format-line (ht-create) chunk left-width right-width separator)
                       formatted)
                 removed)
         (cons (cons (jujutsu-diff--format-line (car removed) chunk left-width right-width separator)
                     formatted)
               (cdr removed)))))))

(defun jujutsu-diff--create-side-by-side-diff (diff-git-chunk)
  "Create a side-by-side diff representation from DIFF-GIT-CHUNK."
  (let* ((left-width (jujutsu-diff--max-content-width diff-git-chunk :removed))
         (right-width (jujutsu-diff--max-content-width diff-git-chunk :added))
         (separator " ")
         (initial-acc (cons nil nil))  ; (formatted-lines . removed-lines)
         (result-and-removed
          (-reduce-from
           (lambda (acc chunk)
             (jujutsu-diff--process-chunk acc chunk left-width right-width separator))
           initial-acc
           diff-git-chunk))
         (formatted-lines (car result-and-removed))
         (remaining-removed (cdr result-and-removed)))
    (-concat
     (nreverse formatted-lines)
     (--map (jujutsu-diff--format-line it (ht-create) left-width right-width separator)
            remaining-removed))))

(-comment
 (with-current-buffer "*jj debug*"
   (erase-buffer)
   (->> "resources/test-diff-git.output"
        -slurp
        jujutsu-diff--split-git-diff
        (nth 1)
        jujutsu-diff--split-git-diff-into-hunks
        (nth 2)
        jujutsu-diff--parse-diff-hunk
        jujutsu-diff--create-side-by-side-diff
        (-map (lambda (s) (insert s) (insert "\n"))))
   (display-buffer "*jj debug*"))
 1)

(provide 'jujutsu-diff)
;;; jujutsu-diff.el ends here
