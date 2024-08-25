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
;; Package-Requires: ((emacs "24.5"))
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
(require 'cl-extra)

(defun nx-node (type &optional props children)
  "Create a node with TYPE, optional PROPS and CHILDREN.
If :_nx-id is present in PROPS, it will be used as the node's ID."
  (let* ((contains-nxid? (lambda (props) (and (ht? props) (ht-contains? props :nx/id))))
         (id (if (funcall contains-nxid? props)
                (ht-get props :nx/id)
              (gensym "nx-"))))
    (when (funcall contains-nxid? props)
      (ht-remove props :nx/id))
    (ht (:id id)
        (:type type)
        (:props (or props (ht)))
        (:children children))))

(defalias 'nx 'nx-node)

(-comment
 (nx-node :foo)
 (nx-node :foo (ht (:nx/id 1)))

 )

(defun nx-type (node)
  "Get the type of NODE."
  (ht-get node :type))

(defun nx-props (node)
  "Get the properties of NODE."
  (ht-copy (ht-get node :props)))

(defun nx-children (node)
  "Get the children of NODE."
  (ht-get node :children))

(-tests
 (nx :foo)
 )

(defun nx--node-equal (node1 node2)
  "Deeply compare NODE1 and NODE2 for equality."
  (and (eq (nx-type node1) (nx-type node2))
       (ht-equal? (nx-props node1) (nx-props node2))
       (let ((children1 (nx-children node1))
             (children2 (nx-children node2)))
         (and (eq (length children1) (length children2))
              (cl-every #'nx--node-equal children1 children2)))))

(-comment
 (nx--node-equal (nx :root (ht) (list
                                 (nx :bar (ht))
                                 (nx :foo (ht (:bar t)))))
                 (nx :root (ht) (list
                                 (nx :bar (ht))
                                 (nx :foo (ht (:bar t))))))
 )


(defun nx-diff-trees (old-tree new-tree)
  "Compare OLD-TREE and NEW-TREE, returning a list of change operations."
  (nx--diff-nodes old-tree new-tree nil))

(defun nx--diff-nodes (old-node new-node parent-id)
  "Compare OLD-NODE and NEW-NODE, returning a list of change operations."
  (cond
   ;; Nodes are identical
   ((nx--node-equal old-node new-node)
    nil)
   ;; Node is new
   ((null old-node)
    `((:insert ,parent-id ,new-node)))
   ;; Node is removed
   ((null new-node)
    `((:remove ,(ht-get old-node :id))))
   ;; Node type changed
   ((not (eq (ht-get old-node :type) (ht-get new-node :type)))
    `((:replace ,(ht-get old-node :id) ,new-node)))
   ;; Properties changed
   ((not (ht-equal? (ht-get old-node :props) (ht-get new-node :props)))
    (append
     `((:update ,(ht-get old-node :id) ,(ht-get new-node :props)))
     (nx--diff-children
      (ht-get old-node :children)
      (ht-get new-node :children)
      (ht-get old-node :id))))
   ;; Compare children
   (t
    (nx--diff-children
     (ht-get old-node :children)
     (ht-get new-node :children)
     (ht-get old-node :id)))))

(defun nx-copy (node)
  "Create a deep copy of NODE, preserving IDs."
  (let* ((old-props (ht-get node :props))
         (new-props (ht-copy old-props))
         (new-children (mapcar #'nx-copy (ht-get node :children))))
    (when (ht-get node :id)
      (ht-set! new-props :nx/id (ht-get node :id)))
    (nx (ht-get node :type) new-props new-children)))

(defun nx--diff-children (old-children new-children parent-id)
  "Compare OLD-CHILDREN and NEW-CHILDREN, returning a list of change operations."
  (let ((changes '())
        (old-index 0)
        (new-index 0))

    ;; Compare children in order
    (while (and (< old-index (length old-children))
                (< new-index (length new-children)))
      (let ((old-child (nth old-index old-children))
            (new-child (nth new-index new-children)))
        (if (equal (ht-get old-child :id) (ht-get new-child :id))
            (progn
              (setq changes (append changes (nx--diff-nodes old-child new-child parent-id)))
              (setq old-index (1+ old-index))
              (setq new-index (1+ new-index)))
          (progn
            (push `(:remove ,(ht-get old-child :id)) changes)
            (setq old-index (1+ old-index))))))

    ;; Remove any remaining old children
    (while (< old-index (length old-children))
      (let ((old-child (nth old-index old-children)))
        (push `(:remove ,(ht-get old-child :id)) changes)
        (setq old-index (1+ old-index))))

    ;; Insert any remaining new children
    (while (< new-index (length new-children))
      (let ((new-child (nth new-index new-children)))
        (push `(:insert ,parent-id ,new-child) changes)
        (setq new-index (1+ new-index))))

    (nreverse changes)))

(defun nx-type (node)
  "Get the type of NODE."
  (ht-get node :type))

(defun nx-props (node)
  "Get the properties of NODE."
  (ht-copy (ht-get node :props)))

(defun nx-children (node)
  "Get the children of NODE."
  (ht-get node :children))

(defun nx-id (node)
  "Get the internal id of NODE."
  (ht-get node :id))

(defun nx? (obj)
  "Check if OBJ is a valid nx node.
Returns t if OBJ is a valid nx node, nil otherwise."
  (and (hash-table-p obj)
       (keywordp (ht-get obj :type))
       (hash-table-p (ht-get obj :props))
       (or (null (ht-get obj :children))
           (and (listp (ht-get obj :children))
                (cl-every #'nx? (ht-get obj :children))))))

(defun nx?-strict (obj)
  "Strictly check if OBJ is a valid nx node.
Throws an error with a descriptive message if OBJ is not a valid nx node.
Returns t if OBJ is a valid nx node."
  (cond
   ((not (hash-table-p obj))
    (error "Not a hash table: %S" obj))
   ((not (keywordp (ht-get obj :type)))
    (error "Invalid or missing :type: %S" (ht-get obj :type)))
   ((not (hash-table-p (ht-get obj :props)))
    (error "Invalid or missing :props: %S" (ht-get obj :props)))
   ((not (or (null (ht-get obj :children))
             (and (listp (ht-get obj :children))
                  (cl-every #'nx?-strict (ht-get obj :children)))))
    (error "Invalid :children: %S" (ht-get obj :children)))
   (t t)))

(defun nx--build-node-map (tree)
  "Build a hash table mapping node IDs to nodes in TREE."
  (let ((node-map (ht)))
    (nx--traverse-tree tree (lambda (node)
                              (ht-set! node-map (ht-get node :id) node)))
    node-map))

(defun nx--traverse-tree (node fn)
  "Traverse the tree starting at NODE, calling FN on each node."
  (funcall fn node)
  (dolist (child (ht-get node :children))
    (nx--traverse-tree child fn)))

(defun nx--find-parent (tree node)
  "Find the parent of NODE in TREE."
  (nx--find-parent-helper tree node nil))

(defun nx--find-parent-helper (current-node target-node parent)
  "Helper function for nx--find-parent."
  (if (eq current-node target-node)
      parent
    (catch 'found
      (dolist (child (ht-get current-node :children))
        (let ((result (nx--find-parent-helper child target-node current-node)))
          (when result
            (throw 'found result))))
      nil)))

(defun nx-apply-diff (tree diff-ops)
  "Apply DIFF-OPS to a copy of TREE and return the resulting new tree."
  (let* ((new-tree (nx-copy tree))
         (node-map (nx--build-node-map new-tree)))
    (dolist (op diff-ops)
      (pcase op
        (`(:insert ,parent-id ,new-node)
         (let ((parent (ht-get node-map parent-id)))
           (when parent
             (let ((new-children (append (ht-get parent :children) (list (nx-copy new-node)))))
               (ht-set! parent :children new-children)
               (nx--add-to-node-map node-map (nx-copy new-node))))))
        (`(:remove ,node-id)
         (let* ((node (ht-get node-map node-id))
                (parent (nx--find-parent new-tree node)))
           (when parent
             (let ((new-children (remove node (ht-get parent :children))))
               (ht-set! parent :children new-children)))))
        (`(:update ,node-id ,new-props)
         (let ((node (ht-get node-map node-id)))
           (when node
             (ht-set! node :props (ht-copy new-props)))))
        (`(:replace ,node-id ,new-node)
         (let* ((old-node (ht-get node-map node-id))
                (parent (nx--find-parent new-tree old-node)))
           (when parent
             (let ((new-children (mapcar (lambda (child)
                                           (if (equal (ht-get child :id) node-id)
                                               (nx-copy new-node)
                                             child))
                                         (ht-get parent :children))))
               (ht-set! parent :children new-children)
               (nx--add-to-node-map node-map (nx-copy new-node))))))))
    new-tree))

(defun nx--add-to-node-map (node-map node)
  "Add NODE and its children to NODE-MAP."
  (ht-set! node-map (ht-get node :id) node)
  (dolist (child (ht-get node :children))
    (nx--add-to-node-map node-map child)))

(-comment
;; Create an initial tree
(setq initial-tree
      (nx :root (ht (:nx/id 'root))
          (list (nx :child1 (ht (:nx/id 'child1) (:prop "old-value"))
                    (list (nx :grandchild (ht (:nx/id 'grandchild))))))))

;; Create a modified version of the tree
(setq modified-tree
      (nx :root (ht (:nx/id 'root))
          (list (nx :child1 (ht (:nx/id 'child1) (:prop "new-value"))
                    (list (nx :grandchild (ht (:nx/id 'grandchild)))))
                (nx :child2 (ht (:nx/id 'child2))))))
;;
;; Generate diff operations
(setq diff-ops (nx-diff-trees initial-tree modified-tree))

;; Apply the diff to the initial tree
(setq result-tree (nx-apply-diff initial-tree diff-ops))

(should (nx--node-equal modified-tree result-tree))
(should (not (nx--node-equal initial-tree result-tree)))
(should (equal (ht-get (nx-props (car (nx-children initial-tree))) :prop) "old-value"))

 )

(provide 'nx)
;;; nx.el ends here
