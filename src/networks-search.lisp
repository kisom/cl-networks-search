#|
  This file is a part of networks-search project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage networks-search
  (:use :cl :networks-graph)
  (:export :clean-path
           :bfs
           :dfs
           :network-connected-p))
(in-package :networks-search)

;; "Lisp isn't a language, it's a building material. -Alan Kay"

(defun shorten-path (nodes path)
  (if (endp nodes)
      path
      (if (endp (rest nodes))
          (nreverse (cons (first nodes) path))
          (let ((neighbours (remove-if-not
                             (lambda (o)
                               (neighbour-p (first nodes) o))
                             (rest nodes))))
            (if (endp neighbours)
                nil
                (let ((farthest (first (last neighbours))))
                  (shorten-path (member-if (lambda (o)
                                             (eql
                                              (node-label o)
                                              (node-label farthest)))
                                           (rest nodes))
                                (cons (first nodes) path))))))))

(defun equivalent-path-p (nodes path)
  (and (not (endp nodes))
       (not (endp path))
       (path-p path)
       (equal (node-label (first nodes))
              (node-label (first path)))
       (equal (node-label (first (last nodes)))
              (node-label (first (last path))))))

(defun minimise-path (nodes)
  (let ((path (shorten-path nodes '())))
    (if (equivalent-path-p nodes path)
        path
        nodes)))

(defun clean-path (nodes)
  (labels ((path-search (nodes path)
             (cond
               ((endp nodes) nil)
               ((endp (rest nodes))
                (nreverse (cons (first nodes) path)))
               (:else
                (if (neighbour-p (first nodes) (second nodes))
                    (path-search (rest nodes)
                                 (cons (first nodes) path))
                    (path-search (rest nodes) path))))))
    (if (endp nodes)
        nil
        (let ((path (path-search (rest nodes) (list (first nodes)))))
          (if path
              (minimise-path path)
              path)))))

(defun bfs (net start end)
  (unless (get-node net start)
    (error (format nil "~A not found in network.~%" start)))
  (unless (get-node net end)
    (error (format nil "~A not found in network.~%" end)))
  (labels ((bfs-search (q seen path)
             (if (endp q)
                 '()
                 (let ((v (first q))
                       (q (rest q)))
                   (if (eql v end)
                       (append (reverse path) (list v))
                       (let ((edges (remove-if (lambda (nd)
                                                 (member nd seen))
                                               (node-edges
                                                (get-node net v)))))
                         (bfs-search (append q edges)
                                     (cons v seen)
                                     (cons v path))))))))
    (let ((path (bfs-search (list start) '() '())))
      (if path
          (clean-path (mapcar (get-node-map net) path))
          nil))))

(defun dfs (net start end)
  (unless (get-node net start)
    (error (format nil "~A not found in network.~%" start)))
  (unless (get-node net end)
    (error (format nil "~A not found in network.~%" end)))
  (labels ((dfs-search (s seen path)
              (if (endp s)
                 '()
                 (let ((i (first s))
                       (s (rest s)))
                   (if (eql i end)
                       (nreverse (cons i path))
                       (dfs-search (append
                                    (remove-if (lambda (edge)
                                                 (or
                                                  (member edge seen)
                                                  (member edge s)))
                                               (node-edges
                                                (get-node net i)))
                                    s)
                                   (cons i seen)
                                   (cons i path)))))))
    (let ((path (dfs-search (list start) '() '())))
      (if path
          (clean-path (mapcar (get-node-map net) path))
          nil))))

(defmacro every-not (pred lst)
  `(every (lambda (x)
            (not (funcall ,pred x)))
          ,lst))

(defun node-connected-p (net node)
  (every-not (lambda (o)
               (null (bfs net (node-label node)
                              (node-label o))))
             (remove (node-label  node) net :key #'node-label)))

(defun network-connected-p (net)
  (every (lambda (nd)
           (node-connected-p net nd))
         net))

