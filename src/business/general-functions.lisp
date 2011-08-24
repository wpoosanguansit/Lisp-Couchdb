(in-package :careerpacific)

(export '())

;;general functions for the business layer

(defun belong-to-p (object sub-object-or-list)
  (ignore-errors (cond ((clos-object-p sub-object-or-list)
			(member sub-object-or-list 
				(funcall (symbol-function (pluralize-symbol (type-of sub-object-or-list)) object)))
			((listp sub-object-or-list)
			 (and (belong-to-p object (car sub-object-or-list)) 
			      (belong-to-p object (cdr sub-object-or-list))))
			(t nil)))))

;;map the plural symbols in the hash to retrieve

(defun pluralize-symbol (symbol)
  (cdr (assoc symbol *plural-symbol-associative-list*)))