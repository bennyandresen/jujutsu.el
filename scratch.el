;;; jujutsu-mode.el -- Summary: Blah
;;; Commentary:
;;; Experimental Emacs Major-Mode for experimental VCS
;;; Code:
;;; TBD

(defun jj-st-parse ()
  "Run `jj st' command and parse the output into a data structure."
  (let* ((output (shell-command-to-string "jj st"))
         (lines (split-string output "\n" t))
         (changes nil)
         (working-copy nil)
         (parent nil)
         (clean nil))
    (dolist (line lines)
      (cond
       ((string-match "^The working copy is clean" line)
        (setq clean t))
       ((string-match "^Working copy changes:" line)
        (setq changes nil))
       ((string-match "^\\([MA]\\) \\(.+\\)" line)
        (push (list :type (match-string 1 line)
                    :file (match-string 2 line))
              changes))
       ((string-match "^Working copy : \\([^ ]+\\) \\([^ ]+\\) \\(?:(empty) \\)?\\(.+\\)" line)
        (setq working-copy
              (list :change-id (match-string 1 line)
                    :commit-id (match-string 2 line)
                    :description (let ((desc (match-string 3 line)))
                                   (if (string= desc "(no description set)")
                                       nil
                                     desc)))))
       ((string-match "^Parent commit: \\([^ ]+\\) \\([^ ]+\\) \\(?:\\([^|]+\\) | \\)?\\(.+\\)" line)
        (setq parent (list :change-id (match-string 1 line)
                           :commit-id (match-string 2 line)
                           :branch (or (match-string 3 line) "")
                           :description (match-string 4 line))))))
    (list :clean clean
          :changes (nreverse changes)
          :working-copy working-copy
          :parent parent)))


(defface jj-st-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for headers in jj-st output.")

(defface jj-st-change-face
  '((t :inherit font-lock-variable-name-face))
  "Face for change types in jj-st output.")

(defface jj-st-file-face
  '((t :inherit default))
  "Face for file names in jj-st output.")

(defface jj-st-id-face
  '((t :inherit font-lock-constant-face))
  "Face for change and commit IDs in jj-st output.")

(defface jj-st-description-face
  '((t :inherit font-lock-string-face))
  "Face for descriptions in jj-st output.")

(defface jj-st-branch-face
  '((t :inherit font-lock-type-face))
  "Face for branch names in jj-st output.")

(defun jj-st ()
  "Run `jj st' command and display the parsed results with fontification."
  (interactive)
  (let ((result (jj-st-parse)))
    (with-current-buffer (get-buffer-create "*jj-st*")
      (erase-buffer)
      (insert (propertize "Jujutsu Status:\n\n" 'face 'jj-st-header-face))
      (if (plist-get result :clean)
          (insert (propertize "The working copy is clean\n\n" 'face 'font-lock-comment-face))
        (progn
          (insert (propertize "Working copy changes:\n" 'face 'jj-st-header-face))
          (dolist (change (plist-get result :changes))
            (insert (format "  %s %s\n"
                            (propertize (plist-get change :type) 'face 'jj-st-change-face)
                            (propertize (plist-get change :file) 'face 'jj-st-file-face))))
          (insert "\n")))
      (let ((wc (plist-get result :working-copy)))
        (insert (format "%s %s %s %s\n"
                        (propertize "Working copy:" 'face 'jj-st-header-face)
                        (propertize (plist-get wc :change-id) 'face 'jj-st-id-face)
                        (propertize (plist-get wc :commit-id) 'face 'jj-st-id-face)
                        (propertize (or (plist-get wc :description) "(no description set)")
                                    'face 'jj-st-description-face))))
      (let ((parent (plist-get result :parent)))
        (insert (format "%s %s %s %s%s\n"
                        (propertize "Parent commit:" 'face 'jj-st-header-face)
                        (propertize (plist-get parent :change-id) 'face 'jj-st-id-face)
                        (propertize (plist-get parent :commit-id) 'face 'jj-st-id-face)
                        (if (string-empty-p (plist-get parent :branch))
                            ""
                          (concat (propertize (plist-get parent :branch) 'face 'jj-st-branch-face) " | "))
                        (propertize (plist-get parent :description) 'face 'jj-st-description-face))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))


(require 'ert)

(defvar jj-st-mock-output nil
  "Mock output for jj st command.")

(defun mock-shell-command-to-string (command)
  (if (string= command "jj st")
      jj-st-mock-output
    (error "Unexpected command: %s" command)))

(defmacro with-mock-jj-st (output &rest body)
  `(let ((jj-st-mock-output ,output))
     (cl-letf (((symbol-function 'shell-command-to-string)
                #'mock-shell-command-to-string))
       ,@body)))

(with-mock-jj-st
 "Working copy changes:
A scratch.el
Working copy : vrmnqqtx 8e909380 (no description set)
Parent commit: zzzzzzzz 00000000 (empty) (no description set)"
 (jj-st-parse))

(ert-deftest test-jj-st-parse-modified-file ()
  (with-mock-jj-st
   "Working copy changes:
M flake.lock
Working copy : mmqxuuoo 971c5781 flake update, for unstable.yt-dlp
Parent commit: nlvkxokx d02c0525 trunk | home.nix: fix OnCalendar syntax"
   (let ((result (jj-st-parse)))
     (should (equal (plist-get result :clean) nil))
     (should (equal (plist-get result :changes)
                    '((:type "M" :file "flake.lock"))))
     (should (equal (plist-get result :working-copy)
                    '(:change-id "mmqxuuoo"
                      :commit-id "971c5781"
                      :description "flake update, for unstable.yt-dlp")))
     (should (equal (plist-get result :parent)
                    '(:change-id "nlvkxokx"
                      :commit-id "d02c0525"
                      :branch "trunk"
                      :description "home.nix: fix OnCalendar syntax"))))))

(ert-deftest test-jj-st-parse-clean-no-description ()
  (with-mock-jj-st
   "The working copy is clean
Working copy : qtkrkmvk fa3ae636 (empty) (no description set)
Parent commit: mmqxuuoo 971c5781 flake update, for unstable.yt-dlp"
   (let ((result (jj-st-parse)))
     (should (equal (plist-get result :clean) t))
     (should (equal (plist-get result :changes) nil))
     (should (equal (plist-get result :working-copy)
                    '(:change-id "qtkrkmvk"
                      :commit-id "fa3ae636"
                      :description nil)))
     (should (equal (plist-get result :parent)
                    '(:change-id "mmqxuuoo"
                      :commit-id "971c5781"
                      :branch ""
                      :description "flake update, for unstable.yt-dlp"))))))

(ert-deftest test-jj-st-parse-clean-with-description ()
  (with-mock-jj-st
   "The working copy is clean
Working copy : qtkrkmvk 645f7132 (empty) this is my change
Parent commit: mmqxuuoo 971c5781 flake update, for unstable.yt-dlp"
   (let ((result (jj-st-parse)))
     (should (equal (plist-get result :clean) t))
     (should (equal (plist-get result :changes) nil))
     (should (equal (plist-get result :working-copy)
                    '(:change-id "qtkrkmvk"
                      :commit-id "645f7132"
                      :description "this is my change")))
     (should (equal (plist-get result :parent)
                    '(:change-id "mmqxuuoo"
                      :commit-id "971c5781"
                      :branch ""
                      :description "flake update, for unstable.yt-dlp"))))))


(ert-deftest test-jj-st-parse-clean-with-description ()
  (with-mock-jj-st
   "The working copy is clean
Working copy : qtkrkmvk 645f7132 (empty) this is my change
Parent commit: mmqxuuoo 971c5781 flake update, for unstable.yt-dlp"
   (let ((result (jj-st-parse)))
     (should (equal (plist-get result :clean) t))
     (should (equal (plist-get result :changes) nil))
     (should (equal (plist-get result :working-copy)
                    '(:change-id "qtkrkmvk"
                      :commit-id "645f7132"
                      :description "this is my change")))
     (should (equal (plist-get result :parent)
                    '(:change-id "mmqxuuoo"
                      :commit-id "971c5781"
                      :branch ""
                      :description "flake update, for unstable.yt-dlp"))))))



;; Run all tests
;; (ert-run-tests-interactively "test-jj-st-parse-")
;;

(require 'transient)

(defun jj-describe ()
  "Run `jj describe' command."
  (interactive)
  (jj-run-command "describe"))

(defun jj-new ()
  "Run `jj new' command."
  (interactive)
  (jj-run-command "new"))

(defun jj-abandon ()
  "Run `jj abandon' command."
  (interactive)
  (jj-run-command "abandon"))

(defun jj-squash ()
  "Run `jj squash' command."
  (interactive)
  (jj-run-command "squash"))

(defun jj-show-w/template (template &optional rev)
  (let* ((rev (or rev "@"))
         (formatted (format "show --summary --template \"%s\" %s" template rev)))
    (jj-run-command formatted)))

(defun jj-log-w/template (template)
  (let* ((formatted (format "log --no-graph --template \"%s ++ \\\"\\n\\n\\\"\"" template)))
    (jj-run-command formatted)))


(transient-define-prefix jj-dispatch ()
  "Dispatch for jj commands."
  ["Jujutsu Commands"
   ("d" "Describe" jj-describe)
   ("n" "New" jj-new)
   ("a" "Abandon" jj-abandon)
   ("s" "Squash" jj-squash)])

(require 'magit-section)
(require 'map)

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

(defun jj-st-parse ()
  "Run 'jj st' command and parse the output into a data structure."
  (let* ((output (jj-run-command "jj st"))
         (lines (split-string output "\n" t))
         (changes nil)
         (working-copy nil)
         (parent nil)
         (clean nil))
    (dolist (line lines)
      (cond
       ((string-match "^The working copy is clean" line)
        (setq clean t))
       ((string-match "^Working copy changes:" line)
        (setq changes nil))
       ((string-match "^\\([MA]\\) \\(.+\\)" line)
        (push (map-into `(:type ,(match-string 1 line)
                          :file ,(match-string 2 line))
                        'hash-table)
              changes))
       ((string-match "^Working copy : \\([^ ]+\\) \\([^ ]+\\) \\(?:(empty) \\)?\\(.+\\)" line)
        (setq working-copy
              (map-into `(:change-id ,(match-string 1 line)
                          :commit-id ,(match-string 2 line)
                          :description ,(let ((desc (match-string 3 line)))
                                            (if (string= desc "(no description set)")
                                                nil
                                              desc)))
                        'hash-table)))
       ((string-match "^Parent commit: \\([^ ]+\\) \\([^ ]+\\) \\(?:\\([^|]+\\) | \\)?\\(.+\\)" line)
        (setq parent (map-into `(:change-id ,(match-string 1 line)
                                 :commit-id ,(match-string 2 line)
                                 :branch ,(or (match-string 3 line) "")
                                 :description ,(match-string 4 line))
                               'hash-table)))))
    (map-into `(:clean ,clean
                :changes ,(nreverse changes)
                :working-copy ,working-copy
                :parent ,parent)
              'hash-table)))

(defun jj-st-insert-file-list (files)
  "Insert the file list as a section."
  (magit-insert-section (jj-files)
    (magit-insert-heading "Files:")
    (dolist (file files)
      (insert (format "  %s\n" file)))))

(defun jj-st ()
  "Run 'jj st' command and display the parsed results with collapsible sections."
  (interactive)
  (let* ((result (jj-st-parse))
         (buffer (get-buffer-create "*jj-status*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-section-mode)

        (magit-insert-section (jj-status)
          (magit-insert-heading "Jujutsu Status")

          (if (map-elt result 'clean)
              (magit-insert-section (jj-clean)
                (insert "The working copy is clean\n"))
            (magit-insert-section (jj-changes)
              (magit-insert-heading "Working copy changes:")
              (dolist (change (map-elt result :changes))
                (insert (format "  %s %s\n"
                                (map-elt change :type)
                                (map-elt change :file))))))

          (magit-insert-section (jj-working-copy)
            (magit-insert-heading "Working copy:")
            (let ((wc (map-elt result :working-copy)))
              (insert (format "  %s %s %s\n"
                              (map-elt wc :change-id)
                              (map-elt wc :commit-id)
                              (or (map-elt wc :description) "(no description set)")))))

          (magit-insert-section (jj-parent)
            (magit-insert-heading "Parent commit:")
            (let ((parent (map-elt result :parent)))
              (insert (format "  %s %s %s%s\n"
                              (map-elt parent :change-id)
                              (map-elt parent :commit-id)
                              (if (string-empty-p (map-elt parent :branch))
                                  ""
                                (concat (map-elt parent :branch) " | "))
                              (map-elt parent :description)))))

          (jj-st-insert-file-list (jj-file-list))))

      (setq buffer-read-only t)
      (goto-char (point-min)))

    (display-buffer buffer)))

(defun map-to-escaped-string (map)
  "Convert MAP (plist, alist, or hash-table) to an escaped string."
  (mapconcat
   (lambda (pair)
     (format "\\\"%s \\\" ++ %s ++ \\\"\\\\n\\\""
             (car pair)
             (cdr pair)))
   (map-pairs map)
   " ++ "))

(defun parse-string-to-map (input-string)
  "Parse INPUT-STRING into a map and an organized list of file changes."
  (let ((lines (split-string input-string "\n" t))
        (result-map (make-hash-table :test 'equal))
        (file-changes (make-hash-table :test 'equal))
        (file-change-rx (rx bos (group (any "AMD")) " " (group (+ not-newline)) eos))
        (key-value-rx (rx bos
                          (group (+ (not (any " "))))  ; key
                          " "
                          (optional (group (+ not-newline)))  ; optional value
                          eos)))

    ;; Initialize file change categories
    (puthash "A" '() file-changes)
    (puthash "M" '() file-changes)
    (puthash "D" '() file-changes)

    (dolist (line lines)
      (cond
       ;; File change lines (A, M, or D)
       ((string-match file-change-rx line)
        (let ((change-type (match-string 1 line))
              (file-name (match-string 2 line)))
          (puthash change-type
                   (cons file-name (gethash change-type file-changes))
                   file-changes)))
       ;; Key-value pairs (including empty values)
       ((string-match key-value-rx line)
        (let ((key (match-string 1 line))
              (value (match-string 2 line)))
          (puthash (intern key) value result-map)))))

    ;; Reverse the file lists to maintain original order
    (maphash (lambda (k v) (puthash k (nreverse v) file-changes)) file-changes)

    (list result-map file-changes)))

(-> '(commit_id "commit_id"
      commit_id_short "commit_id.short()"
      commit_id_shortest "commit_id.shortest()"
      change_id "change_id"
      change_id_short "change_id.short()"
      change_id_shortest "change_id.shortest()"
      author "author"
      conflict "conflict"
      empty "empty"
      description "description")
    map-to-escaped-string
    (jj-show-w/template "@")
    parse-string-to-map
    car
    (map-elt 'commit_id_shortest))

(defun split-string-on-empty-lines (input-string)
  "Split INPUT-STRING into multiple strings based on empty lines."
  (let ((rx-split (rx bol (zero-or-more space) eol
                      (one-or-more (any space ?\n))
                      bol (zero-or-more space) eol)))
    (split-string input-string rx-split t "[ \t\n]+")))

(seq-map #'parse-string-to-map
        (-> '(commit-id "commit_id"
              commit-id-short "commit_id.short()"
              commit-id-shortest "commit_id.shortest()"
              change-id "change_id"
              change-id-short "change_id.short()"
              change-id-shortest "change_id.shortest()"
              description "description")
            map-to-escaped-string
            jj-log-w/template
            split-string-on-empty-lines))
