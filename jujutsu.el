;;; jujutsu.el --- Experimental porcelain for experimental CVS  -*- lexical-binding: t -*-
;;; Commentary:
;;; Experimental Emacs Major-Mode for experimental VCS
;;; Code:
;;; TBD

(require 'ht)
(require 's)
(require 'dash)
(require 'transient)
(require 'magit-section)
(require 'ert)

(require 'jujutsu-utils)

(defface jujutsu-id-face
  '((t :inherit font-lock-comment-face))
  "Face used for Jujutsu commit IDs and change IDs."
  :group 'jujutsu)

(defface jujutsu-id-shortest-face
  '((t :inherit font-lock-constant-face))
  "Face used for the shortest part of Jujutsu commit IDs and change IDs."
  :group 'jujutsu)

(defface jujutsu-description-face
  '((t :inherit text))
  "Face used for Jujutsu commit descriptions."
  :group 'jujutsu)

(defcustom jujutsu-log-revset-fallback "@ | ancestors(immutable_heads().., 2) | trunk()"
  "Default value when no revset is specified."
  :group 'jujutsu
  :type 'string)

(defun jj--find-project-root ()
  "Find the root directory of the Jujutsu project."
  (locate-dominating-file default-directory ".jj"))

(defun jj--run-command (command &optional use-personal-config)
  "Run a jj COMMAND from the project root and return its output as a string.
If USE-PERSONAL-CONFIG is t, run jj without suppressing the user config."
  (let* ((default-directory (or (jj--find-project-root)
                                (error "Not in a Jujutsu project")))
         (jj-cmds (list "jj" "--no-pager" "--color" "never" command))
         (cmd-list (if use-personal-config
                       jj-cmds
                     (-concat '("env" "JJ_CONFIG=/dev/null") jj-cmds)))
         (cmd-string (s-join " " cmd-list)))
    (shell-command-to-string cmd-string)))

(defun jj--show-w/template (template &optional rev)
  "Run `jj show' command with a custom TEMPLATE and optional REV.

TEMPLATE is a string containing the custom template for the `jj show' command.
REV is an optional revision specifier. If not provided, it defaults to '@'

This function constructs and executes a `jj show' command with the given
template and revision, returning the command's output as a string."
  (let* ((rev (or rev "@"))
         (formatted (format "show --summary --template \"%s\" %s"
                            template
                            rev)))
    (jj--run-command formatted)))

(defun jj--log-w/template (template &optional revset)
  "Run `jj log' command with a custom TEMPLATE and optional REVSET.

TEMPLATE is a string containing the custom template for the `jj log' command.

This function constructs and executes a `jj log' command with the given
template, disabling graph output and adding newlines between entries. It returns
the command's output as a string, with each log entry separated by newlines."
  (let* ((revset (or revset jujutsu-log-revset-fallback))
         (formatted (format "log --revisions \"%s\" --no-graph --template \"%s ++ \\\"\\n\\n\\\"\""
                            revset
                            template)))
    (jj--run-command formatted)))

(defun jj--file-list ()
  "Get the list of files from `jj file list' command."
  (--> "file list"
       jj--run-command
       (s-split "\n" it t)))

(defun jj--map-to-escaped-string (map)
  "Convert MAP (hash-table) to an escaped string for use as a jj template."
  (->> map
       (ht-map (lambda (key value)
                 (format "\\\"%s \\\" ++ %s ++ \\\"\\\\n\\\""
                         key value)))
       (s-join " ++ ")))

(-tests
 (let ((m (ht ('foo nil)
            ('bar 1))))
   (jj--map-to-escaped-string m))
 :=
 "\\\"bar \\\" ++ 1 ++ \\\"\\\\n\\\" ++ \\\"foo \\\" ++ nil ++ \\\"\\\\n\\\""
 )

(defun jj--parse-file-change (line)
  "Parse a file change LINE into a hash-table."
  (-let* [(regex (rx bos (group (any "AMD")) " " (group (+ not-newline)) eos) line)
          ((res m1 m2) (s-match regex line))]
    (when res
      (ht (m1 m2)))))

(defun jj--parse-key-value (line)
  "Parse a KEY-VALUE LINE into a hash-table."
  (unless (s-matches? (rx bos (any "AMD") " ") line)
    (-when-let* [(regex (rx bos
                            (group (+ (not (any " ")))) ; key
                            " "
                            (optional (group (+ not-newline))) ; optional value
                            eos))
                 ((res m1 m2) (s-match regex line))]
      (ht ((intern m1) m2)))))

(defun jj--parse-and-group-file-changes (file-changes)
  "Parse and group FILE-CHANGES by their change type into a hash-table."
  (let ((grouped-changes (ht ('files-added nil)
                             ('files-modified nil)
                             ('files-deleted nil))))
    (when-let ((parsed-changes (-map #'jj--parse-file-change file-changes)))
      (dolist (change parsed-changes)
        (-let* [((&hash "A" a-file "M" m-file "D" d-file) change)
                ((&hash 'files-added a-coll 'files-modified m-coll 'files-deleted d-coll) grouped-changes)]
          (when a-file (ht-set! grouped-changes 'files-added (-concat a-coll (list a-file))))
          (when m-file (ht-set! grouped-changes 'files-modified (-concat m-coll (list m-file))))
          (when d-file (ht-set! grouped-changes 'files-deleted (-concat d-coll (list d-file)))))))
    grouped-changes))

(defun jj--parse-string-to-map (input-string)
  "Parse INPUT-STRING into a hash-table and an organized list of file change."
  (let* ((lines (s-split "\n" input-string t))
         (file-change-lines (-filter #'jj--parse-file-change lines))
         (grouped-file-changes (jj--parse-and-group-file-changes file-change-lines))
         (key-values (-keep #'jj--parse-key-value lines))
         (result-map (apply #'ht-merge key-values)))
    (ht-merge result-map grouped-file-changes)))

(defun jj--split-string-on-empty-lines (input-string)
  "Split INPUT-STRING into multiple strings based on empty lines."
  (let ((rx-split (rx bol (zero-or-more space) eol
                      (one-or-more (any space ?\n))
                      bol (zero-or-more space) eol)))
    (s-split rx-split input-string t)))

(defun jj--get-status-data (rev)
  "Get status data for the given REV."
  (let ((template (ht ('change-id-short "change_id.short(8)")
                      ('change-id-shortest "change_id.shortest()")
                      ('commit-id-short "commit_id.short(8)")
                      ('commit-id-shortest "commit_id.shortest()")
                      ('empty "empty")
                      ('branches "branches")
                      ('git-head "git_head")
                      ('description "description"))))
    (-> (jj--map-to-escaped-string template)
        (jj--show-w/template rev)
        jj--parse-string-to-map)))

(defun jj--get-log-data (&optional revset)
  "Get status data for the given REVSET."
  (let ((revset (or revset jujutsu-log-revset-fallback))
        (template (ht ('change-id-short "change_id.short(8)")
                      ('change-id-shortest "change_id.shortest()")
                      ('commit-id-short "commit_id.short(8)")
                      ('commit-id-shortest "commit_id.shortest()")
                      ('empty "empty")
                      ('branches "branches")
                      ('hidden "hidden")
                      ('author-email "author.email()")
                      ('timestamp "author.timestamp().format(\\\"%Y-%m-%d %H:%M:%S\\\")")
                      ('current-working-copy "current_working_copy")
                      ('remote-branches "remote_branches")
                      ('git-head "git_head")
                      ('root "root")
                      ('description "description"))))
    (--> (jj--map-to-escaped-string template)
        (jj--log-w/template it revset)
        (jj--split-string-on-empty-lines it)
        (-map #'jj--parse-string-to-map it))))

(defun jj--format-id (id-short id-shortest)
  "Format ID-SHORT with ID-SHORTEST distinguished."
  (let* ((shortest-length (length id-shortest))
         (shortest-part (substring id-short 0 shortest-length))
         (rest-part (substring id-short shortest-length)))
    (s-concat
     (propertize shortest-part 'face 'jujutsu-id-shortest-face)
     (propertize rest-part 'face 'jujutsu-id-face))))

(defun jj--format-status-line (data)
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
          (change-id (jj--format-id chids chidss))
          (commit-id (jj--format-id coids coidss))
          (desc (if desc
                    (propertize desc 'face 'jujutsu-description-face)
                  (propertize "(no description set)" 'face 'warning)))]
    (format "%s %s %s%s%s" change-id commit-id branches empty desc)))

(defun jj--format-log-line (data)
  "Format a status line using DATA with fontification."
  (-let* [((&hash 'change-id-short chids 'change-id-shortest chidss
                  'commit-id-short coids 'commit-id-shortest coidss
                  'branches branches 'empty empty 'description desc
                  'root root 'author-email author-email
                  'timestamp timestamp
                  'current-working-copy cwc)
           data)
          (cwc (if (s-equals? cwc "true") "@" "◉"))
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
          (change-id (jj--format-id chids chidss))
          (commit-id (jj--format-id coids coidss))
          (desc (if desc
                    (propertize desc 'face 'jujutsu-description-face)
                  (propertize "(no description set)" 'face 'warning)))]
    (if root
        (list (format "%s  %s %s %s\n"
                      cwc
                      change-id
                      (propertize "root()" 'face 'magit-keyword)
                      commit-id) )
      (list
       (format "%s  %s %s %s %s%s\n" cwc change-id author-email timestamp branches commit-id)
       (format "│  %s%s\n" empty desc)))))

(defun jj--format-log-entries (revset)
  "Format log entries for REVSET as a list of strings.

This function retrieves log data for the given REVSET, formats each entry,
and returns a list of formatted strings. Each log entry is formatted using
`jj--format-log-line`.

If the log data includes the root commit, it will be included in the output.
Otherwise, a tilde '~' is appended to indicate there are earlier commits
not shown.

REVSET is a string specifying the revision set to display in the log."
  (let* ((log-data (jj--get-log-data revset))
         (includes-root? (-some (lambda (m) (s-equals? (ht-get m 'root) "true")) log-data)))
    (-concat (-map #'jj--format-log-line log-data)
             (when (not includes-root?) (list "~")))))

(define-fringe-bitmap 'jujutsu-fringe-triangle-right
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'jujutsu-fringe-triangle-down
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(defun jujutsu-format-status-header (wc-status p-status)
  "Format the status header with working copy and parent commit information."
  (list (format "%s %s\n"
                (propertize "Working copy :" 'face 'font-lock-type-face)
                (jj--format-status-line wc-status))
        (format "%s %s\n"
                (propertize "Parent commit:" 'face 'font-lock-type-face)
                (jj--format-status-line p-status))
        "\n"))

(defun jujutsu-format-working-copy-changes (all-files files-added files-modified files-deleted)
  "Format the working copy changes section."
  (cons
   (if (> (length all-files) 0)
       (propertize "Working copy changes:\n" 'face 'font-lock-keyword-face)
     (propertize "The working copy is clean\n" 'face 'font-lock-keyword-face))
   (-concat
    (jujutsu-format-file-changes "A" 'magit-diffstat-added files-added)
    (jujutsu-format-file-changes "M" 'diff-changed files-modified)
    (jujutsu-format-file-changes "D" 'magit-diffstat-removed files-deleted)
    '("\n"))))

(defun jujutsu-format-file-changes (change-type face files)
  "Format file changes of CHANGE-TYPE with FACE for FILES."
  (-map (lambda (file)
          (concat
           (propertize " " 'display '(left-fringe jujutsu-fringe-triangle-right))
           (propertize
            (format "%s %s\n%s"
                    change-type
                    file
                    (propertize (format "  Details for %s:\n    (Add actual file details here)\n" file)
                                'invisible t
                                'details t))
            'face face)))
        files))

(defun jujutsu-format-log-section (revset)
  "Format the log section using REVSET."
  (cons (propertize "Log:\n" 'face 'font-lock-keyword-face)
        (jj--format-log-entries revset)))

(defun jujutsu-status ()
  "Display a summary of the current Jujutsu working copy status."
  (interactive)
  (let* ((wc-status (jj--get-status-data "@"))
         (p-status (jj--get-status-data "@-"))
         (all-files (-concat (ht-get wc-status 'files-added)
                             (ht-get wc-status 'files-modified)
                             (ht-get wc-status 'files-deleted))))
    (with-current-buffer (get-buffer-create "*jujutsu-status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (->> (list (jujutsu-format-status-header wc-status p-status)
                   (jujutsu-format-working-copy-changes
                    all-files
                    (ht-get wc-status 'files-added)
                    (ht-get wc-status 'files-modified)
                    (ht-get wc-status 'files-deleted))
                   (jujutsu-format-log-section jujutsu-log-revset-fallback))
             -flatten
             (apply #'s-concat)
             insert))
      (goto-char (point-min))
      (jujutsu-status-mode)
      (switch-to-buffer "*jujutsu-status*"))))

(defun jujutsu-toggle-file-details ()
  "Toggle the visibility of file details for the file at point."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (when (looking-at (rx bos (zero-or-more space) (any "AM") " "))
        ;; Check if we're on a file line
        (let* ((start (point))
               (end (save-excursion
                      (forward-line)
                      (while (and (not (eobp)) (looking-at (rx bos "  ")))
                        (forward-line))
                      (point)))
               (details-start (1+ (line-end-position)))
               (current-invisible (get-text-property details-start 'invisible)))
          (when (> end start)
            (put-text-property details-start end 'invisible (not current-invisible))
            ;; Update the fringe indicator
            (put-text-property start (1+ start) 'display
                               (if current-invisible
                                   '(left-fringe jujutsu-fringe-triangle-down)
                                 '(left-fringe jujutsu-fringe-triangle-right)))))))))

(define-derived-mode jujutsu-status-mode special-mode "jujutsu status"
  "Major mode for displaying Jujutsu status."
  :group 'jujutsu
  (setq buffer-read-only t))

(define-key jujutsu-status-mode-map (kbd "g") #'jujutsu-status)

(defun jujutsu-status-squash (args)
  "Run jj squash with ARGS."
  (interactive (list (transient-args 'jujutsu-status-squash-popup)))
  (let ((cmd (concat "squash " (s-join " " args))))
    (jj--run-command cmd t)
    (jujutsu-status)))

(defun jujutsu-status-new (args)
  "Run jj new with ARGS."
  (interactive (list (transient-args 'jujutsu-status-new-popup)))
  (let ((cmd (concat "squash " (s-join " " args))))
    (jj--run-command cmd t)
    (jujutsu-status)))

(defun jujutsu-status-abandon ()
  "Run `jj abandon'."
  (interactive)
  (jj--run-command "abandon" t)
  (jujutsu-status))

(defun jujutsu-status-describe (args)
  "Run jj describe with ARGS."
  (interactive (list (transient-args 'jujutsu-status-describe-popup)))
  (let ((cmd (concat "describe " (s-join " " args))))
    (jj--run-command cmd t)
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
(define-key jujutsu-status-mode-map (kbd "TAB")  #'jujutsu-toggle-file-details)

(provide 'jujutsu)
;;; jujutsu.el ends here
