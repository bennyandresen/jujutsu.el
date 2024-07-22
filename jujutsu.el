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

(defun jj--log-w/template (template)
    "Run `jj log' command with a custom TEMPLATE.

TEMPLATE is a string containing the custom template for the `jj log' command.

This function constructs and executes a `jj log' command with the given
template, disabling graph output and adding newlines between entries. It returns
the command's output as a string, with each log entry separated by newlines."
  (let* ((formatted (format "log --no-graph --template \"%s ++ \\\"\\n\\n\\\"\""
                            template)))
    (jj--run-command formatted)))

(defun jj--file-list ()
  "Get the list of files from `jj file list' command."
  (--> "file list"
       jj--run-command
       (s-split "\n" it t)))

(defun jj--map-to-escaped-string (map)
  "Convert MAP (hash-table) to an escaped string."
  (->> map
       (ht-map (lambda (key value)
                 (format "\\\"%s \\\" ++ %s ++ \\\"\\\\n\\\""
                         key value)))
       (s-join " ++ ")))

(defun jj--parse-file-change (line)
  "Parse a file change LINE into a cons of (type . filename)."
  (-let* [(regex (rx bos (group (any "AMD")) " " (group (+ not-newline)) eos) line)
          ((res m1 m2) (s-match regex line))]
    (when res
      (ht (m1 m2)))))

(defun jj--parse-key-value (line)
  "Parse a KEY-VALUE LINE into a cons of (key . value)."
  (unless (s-matches? (rx bos (any "AMD") " ") line)
    (-when-let* [(regex (rx bos
                            (group (+ (not (any " ")))) ; key
                            " "
                            (optional (group (+ not-newline))) ; optional value
                            eos))
                 ((res m1 m2) (s-match regex line))]
      (ht ((intern m1) m2)))))

(defun jj--parse-and-group-file-changes (file-changes)
  "Parse and group FILE-CHANGES by their change type."
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
  "Parse INPUT-STRING into a map and an organized list of file change."
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
    ;; (split-string input-string rx-split t "[ \t\n]+")
    (s-split rx-split input-string t)))

(defun jj--get-status-data (rev)
  "Get status data for the given REV."
  (let ((template (ht ('change-id-short "change_id.short(8)")
                      ('change-id-shortest "change_id.shortest()")
                      ('commit-id-short "commit_id.short(8)")
                      ('commit-id-shortest "commit_id.shortest()")
                      ('empty "empty")
                      ('branches "branches")
                      ('description "description"))))
    (-> (jj--map-to-escaped-string template)
        (jj--show-w/template rev)
        jj--parse-string-to-map)))

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

(defun jujutsu-status ()
  "Display a summary of the current Jujutsu working copy status."
  (interactive)
  (-let* ((wc-status (jj--get-status-data "@"))
          (p-status (jj--get-status-data "@-"))
          ((&hash 'files-added files-added
                  'files-modified files-modified
                  'files-deleted files-deleted)
           wc-status)
          (all-files (-concat files-added files-modified files-deleted)))
    (with-current-buffer (get-buffer-create "*jujutsu-status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (->>
         (list (format "%s %s\n"
                       (propertize "Working copy :" 'face 'font-lock-type-face)
                       (jj--format-status-line wc-status))
               (format "%s %s\n"
                       (propertize "Parent commit:" 'face 'font-lock-type-face)
                       (jj--format-status-line p-status))
               "\n"
               (if (> (length all-files) 0)
                   (propertize "Working copy changes:\n" 'face 'font-lock-keyword-face)
                 (propertize "The working copy is clean" 'face 'font-lock-keyword-face))
               (-map (lambda (added-file) (propertize (format "A %s\n" added-file)
                                                 'face
                                                 'magit-diffstat-added))
                     files-added)
               (-map (lambda (modified-file) (propertize (format "M %s\n" modified-file)
                                                    'face
                                                    'diff-changed))
                     files-modified)
               (-map (lambda (deleted-file) (propertize (format "D %s\n" deleted-file)
                                                   'face
                                                   'magit-diffstat-removed))
                     files-deleted))
         -flatten
         (apply #'s-concat)
         insert))
      (goto-char (point-min))
      (jujutsu-status-mode)
      (display-buffer (current-buffer)))))

(define-derived-mode jujutsu-status-mode special-mode "Jujutsu Status"
  "Major mode for displaying Jujutsu status."
  :group 'jujutsu
  (setq buffer-read-only t))

(define-key jujutsu-status-mode-map (kbd "g") #'jujutsu-status)

(provide 'jujutsu)
;;; jujutsu.el ends here
