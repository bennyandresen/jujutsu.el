;;; nx-util.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: December 14, 2024
;; Modified: December 14, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/nx-util
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'ht)
(require 'dash)
(require 'nx)
(require 'dash-x)

(defun nx-util-id-buffer-map (&optional buffer)
  "Build hash table mapping nx/id values to their buffer positions in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((id-map (make-hash-table :test 'equal))
          (pos (point-min)))
      (while (< pos (point-max))
        (when-let* ((id (get-text-property pos 'nx/id))
                    (end (next-property-change pos)))
          (puthash id (vector pos end) id-map)
          (setq pos end))
        (setq pos (1+ pos)))
      id-map)))

(defun nx-util-validate-id-coverage (&optional buffer)
  "Return list of positions where nx/id property is missing."
  (with-current-buffer (or buffer (current-buffer))
    (let ((missing-regions nil)
          (pos (point-min)))
      (while (< pos (point-max))
        (unless (get-text-property pos 'nx/id)
          (push (vector pos
                        (or (next-property-change pos)
                            (point-max)))
                missing-regions))
        (setq pos (or (next-property-change pos)
                      (point-max))))
      (nreverse missing-regions))))

(defun nx-util-replace-id-contents (buffer nx-id contents)
  (when (nx? contents)
    (with-current-buffer buffer
      (-let* [(buffer-map (nx-util-id-buffer-map buffer))
              ([beg end] (ht-get buffer-map nx-id))]
        (jujutsu-dev--display-in-buffer buffer-map)
        (when (and beg end)
          (let ((inhibit-read-only t))
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (jujutsu-status--render-node contents nil)))))))))

(-comment

 (jujutsu-status--render-node (nx :text (ht (:text "Foo") (:face 'default))) nil)

 (nx-util-replace-id-contents (current-buffer) 'nx-1263 (nx :text (ht (:text "Foo:") (:face 'default))))

 (nx :div (ht) nil)


 )

(provide 'nx-util)
;;; nx-util.el ends here
