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

(require 'parseedn)

(defmacro jj--comment (&rest _body)
  nil)

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
      (message "temp-file: %s" temp-file)
      (shell-command-to-string (s-join " " (list "cat" temp-file "|" "jet"))))))

(provide 'jujutsu-utils)
;;; jujutsu-utils.el ends here
