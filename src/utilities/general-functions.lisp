(in-package :careerpacific)

(export '())

;;this is taken from http://common-lisp.net/pipermail/clhp-cvs/2003-October/000072.html

(defmacro make-keyword (name)
  "Translates a string into a keyword: (MAKE-KEYWORD \"foo\") -->
:FOO"
  `(read-from-string (format nil ":~a" ,name)))

;;checking cyclic reference in list

(defun cyclic-p (l)
  (cyclic-p-aux l (make-hash-table)))

(defun cyclic-p-aux (l seen)
  (if (consp l)
      (or (gethash l seen)
	  (progn
	    (setf (gethash l seen) t)
	    (or (cyclic-p-aux (car l) seen)
		(cyclic-p-aux (cdr l) seen))))))
;;from lol

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
	  (cons
	   (tree-leaves%% (car tree) test result)
	   (tree-leaves%% (cdr tree) test result))
	  (if (funcall test tree)
	      (funcall result tree)
	      tree))))
	   
(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (element)
      (declare (ignorable element))
      ,test)
    (lambda (element)
      (declare (ignorable element))
      ,result)))

;;substitute the old item with new in the list
;;returns the list unchanged if it is not found.

(defun substitute-item-in-list (list old new)
  (tree-leaves list (eql element old) new))


;;;;;;;;;
;;; Function: interleave
;;;;;;;;;
;;; This performs the same function, but the code is written slightly differently.
;;; Instead of taking the first of both lists and appending them to the recursive
;;; result in order, it only constructs the result one element at a time. The
;;; interleaved order is achieved by âswitchingâ the arguments list order in the
;;; recursive call.
;;;;;;;;;

(defun interleave (first-list second-list)
  (if (null first-list)
      second-list
      (cons (car first-list) (interleave second-list (cdr first-list)))))

;;utility function to filter out by the test function

(defun filter (list test)
  (let ((accumulator nil))
    (dolist (element list)
      (if (funcall test element)
	  (push element accumulator)))
    (nreverse accumulator)))

;;return the element of the tree if it passes the test function

(let ((accumulator nil))
  (defun tree-filter%% (tree test)
    (if tree
	(if (listp tree)
	    (cons
	     (tree-filter%% (car tree) test)
	     (tree-filter%% (cdr tree) test))
	    (if (funcall test tree)
		(push tree accumulator))))
    accumulator))

(defmacro tree-filter (tree test)
  `(tree-filter%%
    ,tree
    (lambda (element)
      (declare (ignorable element))
      ,test)))


(defun return-list (object)
  (cond
    ((null object) nil)
    ((and (clos-object-p object) (has-clos-sub-object-p object))
     (cons object (return-list (retrieve-object-slot-values object))))
    ((and (clos-object-p object) (not (has-clos-sub-object-p object)))
     object)
    ((listp object)
     (cons (return-list (car object)) (return-list (cdr object))))))

;;from on lisp

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
	  :test test))