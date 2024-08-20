(add-to-list 'load-path default-directory)

(require 'jujutsu-dash)
(require 'jujutsu)
(require 'parseedn)
(require 'dash)
(require 's)

(-tests-enable!)

(defun jujutsu-dev--ht-to-edn-pp (x)
  (let ((temp-file (make-temp-file "jujutsu-")))
    (with-temp-buffer
      (insert "{")
      (parseedn-print-hash-or-alist x)
      (insert "}")
      (write-region (point-min) (point-max) temp-file)
      (s-trim (shell-command-to-string (s-join " " (list "cat" temp-file "|" "jet")))))))

(-comment
 (jujutsu-dev--ht-to-edn-pp (ht (:foo t)
                                (:bar 3)))

 1)

(defun jujutsu-dev--seq-to-edn-pp (xs)
  (let ((temp-file (make-temp-file "jujutsu-")))
    (with-temp-buffer
      (insert "[")
      (parseedn-print-seq xs)
      (insert "]")
      (write-region (point-min) (point-max) temp-file)
      (s-trim (shell-command-to-string (s-join " " (list "cat" temp-file "|" "jet")))))))

(-comment
 (jujutsu-dev--seq-to-edn-pp '(:foo :bar :baz))
 (type-of (ht (:foo 1)))

 1)

(defvar jujutsu-dev-dump-user-actions nil)

(defun jujutsu-dev--display-in-buffer (x)
  "Display the pretty-printed EDN representation of X in the *jj debug* buffer.
If the buffer doesn't exist, it will be created. The buffer's content is
replaced with the new output each time this function is called."
  (with-current-buffer (get-buffer-create "*jj debug*")
    (fundamental-mode)
    (erase-buffer)
    (insert
     (if (sequencep x)
         (jujutsu-dev--seq-to-edn-pp x)
       (jujutsu-dev--ht-to-edn-pp x)))
    (clojure-mode)
    (display-buffer (current-buffer))))

(-comment
 (jujutsu-dev--display-in-buffer (ht (:foo t)))
 (jujutsu-dev--display-in-buffer [:foo :baz])
 (jujutsu-dev--display-in-buffer '(:plist true))

 1)

(defun jujutsu-dev-dump-tree ()
  (interactive)
  (let ((current-tree jujutsu-status-app-state))
    (with-current-buffer (get-buffer-create "*jj debug*")
      (fundamental-mode)
      (erase-buffer)
      (-> current-tree
          jujutsu-dev--ht-to-edn-pp
          insert)
      (clojure-mode))))
