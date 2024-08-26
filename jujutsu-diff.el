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
(require 'dash)
(require 'dash-x)
(require 'ht)
(require 's)

(require 'jujutsu-core)

(defun jujutsu-diff--run (filename &optional revision)
  (let ((revision (or revision "@")))
    (jujutsu-core--run-command
     (s-join " " (list "diff" "-r" revision "--git" filename)))))

(-comment
 (jujutsu-diff--run "dash-x.el")
 1)

(defun jujutsu-diff--split-git-diff-by-file (diff-output)
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
      jujutsu-diff--split-git-diff-by-file
      (nth 0))
 1)

(defun jujutsu-diff--parse-diffs (diff-lst)
  "Parse a list of git diff outputs DIFF-LST into a hash table.
Each entry in the hash table will contain the filename, metadata,
and diff content."
  (let ((result (ht-create)))
    (dolist (diff diff-lst)
      (let* ((lines (s-split "\n" diff))
             (filename-line (car lines))
             (filename (when (string-match "^diff --git a/\\(.+\\) b/\\1$" filename-line)
                         (match-string 1 filename-line)))
             (metadata-end (--find-index (s-starts-with? "@@" it) lines))
             (metadata (when metadata-end
                         (-slice lines 1 metadata-end)))
             (diff-content (when metadata-end
                             (-slice lines metadata-end))))
        (when filename
          (ht-set! result filename
                   (ht (:metadata metadata)
                       (:diff-content (s-join "\n" diff-content)))))))
    result))

(-tests
 (--> "resources/test-diff-git.output"
      -slurp
      jujutsu-diff--split-git-diff-by-file
      jujutsu-diff--parse-diffs
      (ht-get it "jujutsu.el")
      (ht-get it :metadata))
 :=
 '("index 5b32d77748...1aa38bdebf 100644"
   "--- a/jujutsu.el"
   "+++ b/jujutsu.el"))


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
 (--> "resources/test-diff-git.output"
      -slurp
      jujutsu-diff--split-git-diff
      jujutsu-diff--parse-diffs
      (ht-get* it "jujutsu.el" :diff-content)
      jujutsu-diff--split-git-diff-into-hunks)
 1)

(defun jujutsu-diff--parse-diff-hunk (hunk-content)
  "Parse the HUNK-CONTENT into a list of hash-tables, excluding the header."
  (let ((result '()))
    (dolist (line (s-lines hunk-content))
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
          (ht-set! entry :content (if (> (length line) 0)
                                      (substring line 1)
                                    ""))))
        (push entry result)))
    (nreverse result)))

(-tests
 (-->
  "        (s-split \"\\n\" it t)))

 (defun jj--map-to-escaped-string (map)
-  \"Convert MAP (hash-table) to an escaped string.\"
+  \"Convert MAP (hash-table) to an escaped string for use as a jj template.\"
   (->> map
        (ht-map (lambda (key value)
                  (format \"\\\"%s \\\" ++ %s ++ \\\"\\\\n\\\"\"
"
  jujutsu-diff--parse-diff-hunk
  (nth 0 it)
  ;; because of ht-equality specialness just a single map is checked.
  (ht-equal? it
             (ht (:type :context)
                 (:content "        (s-split \"\\n\" it t)))"))))
 := t)

(defun jujutsu-diff--parse-hunks (hunks)
  "Parse a list of HUNKS into a hash-table.
Each entry in the hash-table will contain the hunk header and the
parsed diff content."
  (let ((result (ht-create)))
    (dolist (hunk hunks)
      (let* ((lines (s-split "\n" hunk))
             (header (car lines))
             (content (s-join "\n" (cdr lines))))
        (when (s-starts-with? "@@ " header)
          (ht-set! result
                   header
                   (jujutsu-diff--parse-diff-hunk content)))))
    result))

(-tests
 (--> "resources/test-diff-git.output"
      -slurp
      jujutsu-diff--split-git-diff-by-file
      jujutsu-diff--parse-diffs
      (ht-get* it "jujutsu.el" :diff-content)
      jujutsu-diff--split-git-diff-into-hunks
      jujutsu-diff--parse-hunks
      (ht-get it "@@ -75,7 +82,7 @@")
      (nth 3 it)
      (ht-equal?
       it
       (ht (:type :removed)
           (:content "  \"Convert MAP (hash-table) to an escaped string.\""))))
 := t)

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

(-comment
 (jujutsu-diff--pad-string "  hello world" 30)

 1)

(defun jujutsu-diff--format-line (left right left-width right-width separator)
  "Format LEFT and RIGHT content with given widths (LEFT-WIDTH,RIGHT-WIDTH) and SEPARATOR."
  (-let* [((&hash :content left-content :type left-type) left)
          ((&hash :content right-content :type right-type) right)
          (left-formatted (jujutsu-diff--pad-string left-content left-width))
          (right-formatted (jujutsu-diff--pad-string right-content right-width))
          (is-context (and (eq left-type :context) (eq right-type :context)))]
    (concat
     (if is-context
         (propertize left-formatted 'face 'magit-diff-context)
       (propertize left-formatted 'face 'magit-diff-removed))
     separator
     (if is-context
         (propertize right-formatted 'face 'magit-diff-context)
       (propertize right-formatted 'face 'magit-diff-added)))))

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
 ;; for demo purposes only
 (let* ((hunks (--> "resources/test-diff-git.output"
                    -slurp
                    jujutsu-diff--split-git-diff-by-file
                    jujutsu-diff--parse-diffs
                    (ht-get* it "jujutsu.el" :diff-content)
                    jujutsu-diff--split-git-diff-into-hunks
                    jujutsu-diff--parse-hunks))
        (hunk-keys (ht-keys hunks))
        (len (length hunk-keys))
        (rand (random len)))
   (with-current-buffer (get-buffer-create "*jj debug*")
     (fundamental-mode)
     (erase-buffer)
     (--> hunks
          (ht-get it (nth rand hunk-keys))
          jujutsu-diff--create-side-by-side-diff
          (-map (lambda (s) (insert s) (insert "\n")) it))
     (display-buffer "*jj debug*")))
 1)

(provide 'jujutsu-diff)
;;; jujutsu-diff.el ends here
