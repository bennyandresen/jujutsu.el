;;; nx.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Benjamin Andresen
;;
;; Author: Benjamin Andresen <b@lambda.icu>
;; Maintainer: Benjamin Andresen <b@lambda.icu>
;; Created: August 20, 2024
;; Modified: August 20, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bennyandresen/nx
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

(defun nx-node (type &optional props children)
  "Create a Jujutsu node with TYPE, optional PROPS and CHILDREN.
TYPE is a keyword symbol representing the node type.
PROPS is an optional hash table of properties.
CHILDREN is an optional list of child nodes."
  (ht (:type type)
      (:props (or props (ht)))
      (:children children)))

(defalias 'nx 'nx-node)

(provide 'nx)
;;; nx.el ends here
