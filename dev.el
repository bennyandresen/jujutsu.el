(add-to-list 'load-path default-directory)

(require 'dash-x)
(require 'jujutsu)
(require 'parseedn)
(require 'dash)
(require 's)

(-tests-enable!)

(defun jujutsu-dev--generic-jet (x)
  (let ((temp-file (make-temp-file "jujutsu-")))
    (with-temp-buffer
      (insert (parseedn-print-str x))
      (write-region (point-min) (point-max) temp-file)
      (s-trim (shell-command-to-string (s-join " " (list "cat" temp-file "|" "jet")))))))

(defvar jujutsu-dev-dump-user-actions nil)

(-comment
 (setq jujutsu-dev-dump-user-actions (not jujutsu-dev-dump-user-actions))
 1)

(defun jujutsu-dev-dump (&optional x name)
  (interactive)
  (let ((debug-val (or x jujutsu-status-app-state)))
    (with-current-buffer (get-buffer-create (or name "*jj debug*"))
      (fundamental-mode)
      (erase-buffer)
      (insert (jujutsu-dev--generic-jet x))
      (when (featurep 'clojure-mode)
        (clojure-mode)))))

(defun jujutsu-dev-dump-display (&optional x name)
  (interactive)
  (jujutsu-dev-dump x name)
  (display-buffer (get-buffer (or name "*jj debug*"))))

(-comment
 (jujutsu-dev-dump-display (ht (:foo t)))
 (jujutsu-dev-dump-display (ht (:bar nil)))

 1)
