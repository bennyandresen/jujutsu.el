;;; jujutsu-formatting.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 19, 2024
;; Modified: August 19, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/jujutsu-formatting
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 's)
(require 'dash)

(require 'jujutsu-faces)

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

(defun jujutsu-formatting--format-id (id-short id-shortest)
  "Format ID-SHORT with ID-SHORTEST distinguished."
  (let* ((shortest-length (length id-shortest))
         (shortest-part (substring id-short 0 shortest-length))
         (rest-part (substring id-short shortest-length)))
    (s-concat
     (propertize shortest-part 'face 'jujutsu-id-shortest-face)
     (propertize rest-part 'face 'jujutsu-id-face))))

(provide 'jujutsu-formatting)
;;; jujutsu-formatting.el ends here
