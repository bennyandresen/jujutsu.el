;;; jujutsu-mode.el -- Summary: Blah
;;; Commentary:
;;; Experimental Emacs Major-Mode for experimental VCS
;;; Code:
;;; TBD

(require 'map)
(require 'transient)
(require 'magit-section)
(require 'ert)

(defun jj-show-w/template (template &optional rev)
  (let* ((rev (or rev "@"))
         (formatted (format "show --summary --template \"%s\" %s"
                            template
                            rev)))
    (jj-run-command formatted)))

(defun jj-log-w/template (template)
  (let* ((formatted (format "log --no-graph --template \"%s ++ \\\"\\n\\n\\\"\""
                            template)))
    (jj-run-command formatted)))

(defun jj-find-project-root ()
  "Find the root directory of the Jujutsu project."
  (locate-dominating-file default-directory ".jj"))

(defun jj-run-command (command &optional use-config)
  "Run a jj COMMAND from the project root and return its output as a string.
If USE-CONFIG is t, run jj without suppressing the user config."
  (let* ((default-directory (or (jj-find-project-root)
                                (error "Not in a Jujutsu project")))
         (jj-cmds (list "jj" "--no-pager" "--color" "never" command))
         (cmd-list (if use-config
                       jj-cmds
                     (append (list "env" "JJ_CONFIG=/dev/null") jj-cmds)))
         (cmd-string (string-join cmd-list " ")))
    (shell-command-to-string cmd-string)))

(defun jj-file-list ()
  "Get the list of files from 'jj file list' command."
  (split-string (jj-run-command "file list") "\n" t))

(defun jj-st-insert-file-list (files)
  "Insert the file list as a section."
  (magit-insert-section (jj-files)
    (magit-insert-heading "Files:")
    (dolist (file files)
      (insert (format "  %s\n" file)))))

(defun map-to-escaped-string (map)
  "Convert MAP (plist, alist, or hash-table) to an escaped string."
  (mapconcat
   (lambda (pair)
     (format "\\\"%s \\\" ++ %s ++ \\\"\\\\n\\\""
             (car pair)
             (cdr pair)))
   (map-pairs map)
   " ++ "))

(defun parse-file-change (line)
  "Parse a file change LINE into a cons of (type . filename)."
  (when (string-match (rx bos (group (any "AMD")) " " (group (+ not-newline)) eos) line)
    (cons (match-string 1 line) (match-string 2 line))))

(defun parse-key-value (line)
  "Parse a KEY-VALUE LINE into a cons of (key . value)."
  (when (string-match (rx bos
                          (group (+ (not (any " "))))  ; key
                          " "
                          (optional (group (+ not-newline)))  ; optional value
                          eos) line)
    (cons (intern (match-string 1 line)) (match-string 2 line))))

(defun categorize-file-changes (file-changes)
  "Organize FILE-CHANGES into categories."
  (let ((categories (make-hash-table :test 'equal)))
    (dolist (change file-changes)
      (let ((type (car change))
            (file (cdr change)))
        (push file (gethash type categories '()))))
    (maphash (lambda (k v) (puthash k (nreverse v) categories)) categories)
    categories))

(defun parse-string-to-map (input-string)
  "Parse INPUT-STRING into a map and an organized list of file changes."
  (let* ((lines (split-string input-string "\n" t))
         (file-changes (delq nil (mapcar #'parse-file-change lines)))
         (key-values (delq nil (mapcar #'parse-key-value lines)))
         (result-map (make-hash-table :test 'equal)))
    (dolist (kv key-values)
      (puthash (car kv) (cdr kv) result-map))
    (list result-map (categorize-file-changes file-changes))))


;; (-> '(commit-id "commit_id"
;;       commit-id-short "commit_id.short()"
;;       commit-id-shortest "commit_id.shortest()"
;;       change-id "change_id"
;;       change-id-short "change_id.short()"
;;       change-id-shortest "change_id.shortest()"
;;       author "author"
;;       conflict "conflict"
;;       empty "empty"
;;       description "description")
;;     map-to-escaped-string
;;     (jj-show-w/template "@")
;;     parse-string-to-map
;;     car
;;     (map-elt 'commit-id-short))

(defun split-string-on-empty-lines (input-string)
  "Split INPUT-STRING into multiple strings based on empty lines."
  (let ((rx-split (rx bol (zero-or-more space) eol
                      (one-or-more (any space ?\n))
                      bol (zero-or-more space) eol)))
    (split-string input-string rx-split t "[ \t\n]+")))

;; (seq-map #'parse-string-to-map
;;         (-> '(commit-id "commit_id"
;;               commit-id-short "commit_id.short()"
;;               commit-id-shortest "commit_id.shortest()"
;;               change-id "change_id"
;;               change-id-short "change_id.short()"
;;               change-id-shortest "change_id.shortest()"
;;               description "description")
;;             map-to-escaped-string
;;             jj-log-w/template
;;             split-string-on-empty-lines))

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

(defun jj-get-status-data (rev)
  "Get status data for the given REV."
  (let ((template '(change-id-short "change_id.short(8)"
                    change-id-shortest "change_id.shortest()"
                    commit-id-short "commit_id.short(8)"
                    commit-id-shortest "commit_id.shortest()"
                    description "description")))
    (-> (map-to-escaped-string template)
        (jj-show-w/template rev)
        parse-string-to-map
        car)))

(defun jj-format-id (id-short id-shortest)
  "Format ID-SHORT with ID-SHORTEST distinguished."
  (let* ((shortest-length (length id-shortest))
         (shortest-part (substring id-short 0 shortest-length))
         (rest-part (substring id-short shortest-length)))
    (concat
     (propertize shortest-part 'face 'jujutsu-id-shortest-face)
     (propertize rest-part 'face 'jujutsu-id-face))))

(defun jj-format-status-line (data)
  "Format a status line using DATA with fontification."
  (let* ((change-id-short (map-elt data 'change-id-short))
         (change-id-shortest (map-elt data 'change-id-shortest))
         (commit-id-short (map-elt data 'commit-id-short))
         (commit-id-shortest (map-elt data 'commit-id-shortest))
         (description (map-elt data 'description))
         (change-id (jj-format-id change-id-short change-id-shortest))
         (commit-id (jj-format-id commit-id-short commit-id-shortest)))
    (format "%s %s %s"
            change-id
            commit-id
            (propertize description 'face 'jujutsu-description-face))))

(defun jujutsu-status ()
  "Display a summary of the current Jujutsu working copy status."
  (interactive)
  (let ((wc-status-data (jj-get-status-data "@"))
        (p-status-data (jj-get-status-data "@-")))
    (with-current-buffer (get-buffer-create "*jujutsu-status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s %s\n"
                        (propertize "Parent commit:" 'face 'font-lock-type-face)
                        (jj-format-status-line p-status-data)))
        (insert (format "%s %s\n\n"
                        (propertize "Working copy :" 'face 'font-lock-type-face)
                        (jj-format-status-line wc-status-data)))
        (insert (propertize "Files:\n" 'face 'font-lock-keyword-face))
        (dolist (file (jj-file-list))
          (insert (format "  %s\n" (propertize file 'face 'font-lock-string-face)))))
      (goto-char (point-min))
      (jujutsu-status-mode)
      (display-buffer (current-buffer)))))

(define-derived-mode jujutsu-status-mode special-mode "Jujutsu Status"
  "Major mode for displaying Jujutsu status."
  :group 'jujutsu
  (setq buffer-read-only t))

(define-key jujutsu-status-mode-map (kbd "g") #'jujutsu-status)
