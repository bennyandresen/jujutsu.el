(add-to-list 'load-path default-directory)

(require 'jujutsu)
(require 'parseedn)
(require 'dash)
(require 's)

(-tests-enable!)

(defun jujutsu-dev--ht-to-edn-pp (x)
  (let ((temp-file (make-temp-file "jujutsu-")))
    (with-temp-buffer
      (insert "[")
      (parseedn-print-hash-or-alist x)
      (insert "]")
      (write-region (point-min) (point-max) temp-file)
      (shell-command-to-string (s-join " " (list "cat" temp-file "|" "jet"))))))

(defun jujutsu-dev--display-in-buffer (x)
  "Display the pretty-printed EDN representation of X in the *jj debug* buffer.
If the buffer doesn't exist, it will be created. The buffer's content is
replaced with the new output each time this function is called."
  (with-current-buffer (get-buffer-create "*jj debug*")
    (erase-buffer)
    (insert (jujutsu-dev--ht-to-edn-pp x))
    (display-buffer (current-buffer))))

(defun jujutsu-dev-dump-tree ()
  (interactive)
  (let ((current-tree jujutsu-status-tree))
    (with-current-buffer (get-buffer-create "*jj debug*")
      (fundamental-mode)
      (erase-buffer)
      (-> current-tree
          jujutsu-dev--ht-to-edn-pp
          insert)
      (clojure-mode))))
