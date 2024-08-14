;;; jujutsu-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 13, 2024
;; Modified: August 13, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/jujutsu-utils
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
(require 'parseedn)
(require 'dash)
(require 's)

(defun jj--slurp (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun jj--ht-to-edn-pp (x)
  (let ((temp-file (make-temp-file "jujutsu-")))
    (with-temp-buffer
      (insert "[")
      (parseedn-print-hash-or-alist x)
      (insert "]")
      (write-region (point-min) (point-max) temp-file)
      (shell-command-to-string (s-join " " (list "cat" temp-file "|" "jet"))))))

(defun jj--display-in-buffer (x)
  "Display the pretty-printed EDN representation of X in the *jj debug* buffer.
If the buffer doesn't exist, it will be created. The buffer's content is
replaced with the new output each time this function is called."
  (with-current-buffer (get-buffer-create "*jj debug*")
    (erase-buffer)
    (insert (jj--ht-to-edn-pp x))
    (display-buffer (current-buffer))))

(provide 'jujutsu-utils)
;;; jujutsu-utils.el ends here
