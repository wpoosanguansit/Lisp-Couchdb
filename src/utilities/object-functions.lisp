(in-package :careerpacific)

(export '())

;;this function accept only clos objects with slots that have list of clos 
;;objects or list of string/integer or string/integer
;;
;;this is to extract the keyword and value slots out of the objects
;;it is for update and request to the couchdb.
;;list-type indicate the type of list that we will define.
;;it can be complete which would mean complete object or for-update
;;which would return the values without id
;;the format ((:id . "12345567890") (:content . "content"))
;;object - must be of clos object
;;list-type - 'alist or 'plist
;;with-or-without - 'with or without
;;remove-list - the list if the form of '(:ID :REV) to be included or excluded
;;sub-object-as-id - 'sub-object-as-id - this is the flag to indicate if the value retrieved is for saving the object
;;           couchdb.  this will retrieve only the object ids in the slots of the main object
;;           i.e. in resume, only the id of the addresses will be retrieved instead of the whole addresse
;;           object values.

(defun value-list-of (object list-type &optional with-or-without remove-list sub-object-as-id)
  (if (not (non-cyclic-clos-object-p object)) 
		 (error 'careerpacific-integration-error
			:error-number 2001 error-type "value-list-of argument error"
			:reason (format nil "value-list-of ~a : 
                        the object references itself in its slot value" object)))
  (cond 
     ((and (atom object) (clos-object-p object))
      (let* ((keywords (mapcar #'(lambda (keyword) (make-keyword keyword))
			       (mapcar #'closer-mop:slot-definition-name 
				       (closer-mop:class-slots (closer-mop::class-of object)))))
	     (pre-processed-list (mapcar #'(lambda (keyword) (funcall (symbol-function keyword) object))
					 (mapcar #'closer-mop:slot-definition-name 
						 (closer-mop:class-slots (closer-mop::class-of object)))))
	     (slot-value-list (mapcar #'(lambda (object) 
					  (transform-value-list object list-type with-or-without 
								remove-list sub-object-as-id))
				      pre-processed-list)))
	(cond 
	  ((and (eql list-type 'as-plist) (eql with-or-without 'with))
	   (remove-if-not #'(lambda(element) (member (car element) remove-list))
			  (mapcar #'list keywords slot-value-list)))
	  ((and (eql list-type 'as-plist) (eql with-or-without 'without))
	   (remove-if #'(lambda(element) (member (car element) remove-list))
		      (mapcar #'list keywords slot-value-list)))
	  ((and (eql list-type 'as-alist) (eql with-or-without 'with))
	   (remove-if-not #'(lambda(element) (member (car element) remove-list))
			  (mapcar #'cons keywords slot-value-list)))
	  ((and (eql list-type 'as-alist) (eql with-or-without 'without))
	   (remove-if #'(lambda(element) (member (car element) remove-list))
		      (mapcar #'cons keywords slot-value-list)))
	  ((and (eql list-type 'as-plist) (eql with-or-without nil))
	   (mapcar #'list keywords slot-value-list))
	  ((and (eql list-type 'as-alist) (eql with-or-without nil))
	   (mapcar #'cons keywords slot-value-list))
	  (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
		    :error-type "value-list-of : wrong agument type" 
		    :reason "only 'alist, 'plist, 'with, 'without or sub-object-as-id are allowed")))))
     (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "value-list-of : wrong argument type" 
	       :reason "object is not a clos object"))))

;;iterate version

(defun value-list-of (object list-type &optional with-or-without remove-list sub-object-as-id)
  (if (not (non-cyclic-clos-object-p object)) 
		 (error 'careerpacific-integration-error
			:error-number 2001 error-type "value-list-of argument error"
			:reason (format nil "value-list-of ~a : 
                        the object references itself in its slot value" object)))
  (cond 
    ((and (atom object) (clos-object-p object))
     (let* ((keywords (retrieve-object-keywords object)) 
	      (pre-processed-list (retrieve-object-slot-values object))	 
	    (slot-value-list (mapcar #'(lambda (object) 
					 (transform-value-list object list-type with-or-without 
							       remove-list sub-object-as-id))
				     pre-processed-list)))
       (cond 
	 ((and (eql list-type 'as-plist) (eql with-or-without 'with))
	  (remove-if-not #'(lambda(element) (member (car element) remove-list))
			 (mapcar #'list keywords slot-value-list)))
	 ((and (eql list-type 'as-plist) (eql with-or-without 'without))
	  (remove-if #'(lambda(element) (member (car element) remove-list))
		     (mapcar #'list keywords slot-value-list)))
	 ((and (eql list-type 'as-alist) (eql with-or-without 'with))
	  (remove-if-not #'(lambda(element) (member (car element) remove-list))
			 (mapcar #'cons keywords slot-value-list)))
	 ((and (eql list-type 'as-alist) (eql with-or-without 'without))
	  (remove-if #'(lambda(element) (member (car element) remove-list))
			    (mapcar #'cons keywords slot-value-list)))
	 ((and (eql list-type 'as-plist) (eql with-or-without nil))
	  (mapcar #'list keywords slot-value-list))
	 ((and (eql list-type 'as-alist) (eql with-or-without nil))
	  (mapcar #'cons keywords slot-value-list))
	 (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
		   :error-type "value-list-of : wrong agument type" 
		   :reason "only 'alist, 'plist, 'with, 'without and 'sub-object-as-id are allowed")))))
    (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
	      :error-type "value-list-of : wrong argument type" 
	      :reason "object is not a clos object"))))

(defun value-list-of (object list-type &optional with-or-without remove-list sub-object-as-id)
  (if (not (non-cyclic-clos-object-p object)) 
		 (error 'careerpacific-integration-error
			:error-number 2001 error-type "value-list-of argument error"
			:reason (format nil "value-list-of ~a : 
                        the object references itself in its slot value" object)))
  (cond 
     ((and (atom object) (clos-object-p object))
      (let* ((keywords (retrieve-object-keywords object)) 
	     (pre-processed-list (retrieve-object-slot-values object))
	     (slot-value-list (retrieve-transform-values-list 
			       pre-processed-list list-type 
			       with-or-without remove-list sub-object-as-id)))
	(cond 
	  ((and (eql list-type 'as-plist) (eql with-or-without 'with))
	   (remove-if-not #'(lambda(element) (member (car element) remove-list))
			  (mapcar #'list keywords slot-value-list)))
	  ((and (eql list-type 'as-plist) (eql with-or-without 'without))
	   (remove-if #'(lambda(element) (member (car element) remove-list))
		      (mapcar #'list keywords slot-value-list)))
	  ((and (eql list-type 'as-alist) (eql with-or-without 'with))
	   (remove-if-not #'(lambda(element) (member (car element) remove-list))
			  (mapcar #'cons keywords slot-value-list)))
	  ((and (eql list-type 'as-alist) (eql with-or-without 'without))
	   (remove-if #'(lambda(element) (member (car element) remove-list))
		      (mapcar #'cons keywords slot-value-list)))
	  ((and (eql list-type 'as-plist) (eql with-or-without nil))
	   (mapcar #'list keywords slot-value-list))
	  ((and (eql list-type 'as-alist) (eql with-or-without nil))
	   (mapcar #'cons keywords slot-value-list))
	  (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
		    :error-type "value-list-of : wrong agument type" 
		    :reason "only 'alist, 'plist, 'with, 'without or sub-object-as id are allowed")))))
     (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "value-list-of : wrong argument type" 
	       :reason "object is not a clos object"))))

;;the value of the slots, which is represented by object in the arg list, can be of
;;value - t or string or a integer or list of string or clos object or clos object list
;;else error is raised.

(defun transform-value-list (object list-type &optional with-or-without remove-list sub-object-as-id) 
  (cond 
    ((null object) nil)
    ((equal object t) "true")
    ((or (stringp object) (integerp object)
	 (all-elements-are-string-p object)) object)
    ((and (atom object) (clos-object-p object) (eql sub-object-as-id 'sub-object-as-id))
      (retrieve-id-list object))
    ((and (atom object) (clos-object-p object))
      (list (value-list-of object list-type with-or-without remove-list sub-object-as-id)))
    ((and (all-elements-are-clos-object-p object) (eql sub-object-as-id 'sub-object-as-id)) 
     (retrieve-id-list object))
    ((all-elements-are-clos-object-p object)
     (cons (transform-value-list (car object) list-type with-or-without remove-list sub-object-as-id)
	   (transform-value-list (cdr object) list-type with-or-without remove-list sub-object-as-id)))
    (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
	      :error-type "transform-value-list : wrong argument type" 
	      :reason (format nil "~a not allowed" (type-of object))))))

(defun clos-object-p (object)
  (member 'standard-object 
	  (loop for i in (closer-mop:class-direct-superclasses (class-of object)) 
		collect (class-name i))))

;;this is to check if every element in the list is string or clos object
;;return t if true nil if not.

(defun all-elements-are-string-p (list)
  (ignore-errors (every #'stringp list)))

(defun all-elements-are-clos-object-p (list)
  (ignore-errors (every #'clos-object-p list)))

(defun all-elements-are-of-type (type list)
  (ignore-errors (every #'(lambda (object) (eql type (type-of object))) list)))

(defun all-elements-are-of-user-type (type list)
  (ignore-errors (every #'(lambda (object) (string-equal type (user-type object))) list)))
 
(defun all-elements-are-of-string-value (value function list)
  (ignore-errors (every #'(lambda (object) (string-equal value (function object))) list)))

;;this function is to take a list of objects and returns a list of unique ids for those objects

(defun retrieve-id-list (object-list)
  (cond 
    ((clos-object-p object-list) 
     (id object-list))
    ((all-elements-are-clos-object-p object-list)  
     (mapcar #'id (remove-duplicates object-list :test #'same-persisted-object-p)))
    (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-id-list : wrong argument type" 
	       :reason "argument passed is not a list of clos objects"))))

;;this method is to take a list of id string and returns the list of unique objects pointed to by the ids

(defun retrieve-object-list (class-by-id id-list)
  (cond 
    ((stringp id-list) 
     (retrieve class-by-id id-list))
    ((all-elements-are-string-p id-list)  
     (mapcar #'(lambda (id) (retrieve class-by-id id)) 
	     (remove-duplicates id-list :test #'string-equal)))
    (t (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-object-list : wrong argument type" 
	       :reason "argument passed is not a list of id string pointing to clos objects"))))

;;this uses the make-{class name}-complete to take the full sub object info and intantiates
;;those sub objects with the main object

(defun retrieve-object-complete-list (make-function document-list)
  (if (or (not (functionp make-function)) (not (listp document-list)))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-object-complete-list : wrong argument type" 
	       :reason "arguments passed are not a function and document list - #'function list"))
  (iterate:iterate (iterate:for element iterate:in document-list) 
		    (iterate:collect (funcall make-function (car element)) into result-list)
		    (iterate:finally 
		     (return 
		       (values (remove-duplicates result-list :test #'same-persisted-object-p)))))) 
		      

;;these methods are to be used in the value-list-of function
;;this method retrieve the list of  keywords out of the object being passed

(defun retrieve-object-keywords (object)
  (if (not (clos-object-p object))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-object-keywords : wrong argument type" 
	       :reason "argument passed is not a clos object"))
  (iterate:iterate (iterate:for element iterate:in (closer-mop:class-slots (closer-mop::class-of object))) 
		    (iterate:collect (make-keyword (closer-mop:slot-definition-name element)) 
		      into keywords)
		    (iterate:finally (return (values keywords)))))

;;this method retrieve a list of slot values of the object being passed

(defun retrieve-object-slot-values (object)
  (if (not (clos-object-p object))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-slot-values : wrong argument type" 
	       :reason "argument passed is not a clos object"))
  (iterate:iterate (iterate:for element iterate:in (closer-mop:class-slots (class-of object)))
		   (iterate:collect (slot-value object 
						(closer-mop:slot-definition-name element)) 
		     into pre-processed-list)
		   (iterate:finally (return (values pre-processed-list)))))

;;this method retrieve a list of transformed slot values of the object being passed
;;this will apply recursively to the sub object in the slot values as well, i.e.
;;if resume has user as the sub object in the slot the user object will be retreived
;;as either the plist or alist within the resume plist or alist etc.

(defun retrieve-transform-values-list (pre-processed-list list-type with-or-without remove-list sub-object-as-id)
  (if (not (listp pre-processed-list))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-transform-values-list : wrong argument type" 
	       :reason "argument passed is not a list"))
  (mapcar #'(lambda (object) 
	      (transform-value-list object list-type with-or-without remove-list sub-object-as-id))
	    pre-processed-list))


;;this is a function to check the status of clos object if it is deleted

(defun deleted-persisted-object-p (object)
  (ignore-errors 
    (if (and (persisted-object-p object)
	     (string-equal (status object) "deleted"))
      (return t))))

;;this function check if the object was persisted

(defun persisted-object-p (object)
  (ignore-errors (if (rev object) t)))

(defun all-elements-are-persisted-object-p (object-list)
  (ignore-errors (every #'persisted-object-p object-list)))

;;to clone object, the code is from http://www.archivum.info/comp.lang.lisp/2008-02/msg02471.html

(defun copy-instance (instance)
  (if (not (clos-object-p instance))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "copy-instance : wrong argument type" 
	       :reason "arguments passed are not clos object"))
  (loop with instance-class = (class-of instance)
	with object = (allocate-instance instance-class)
	for slot-definition in (closer-mop:class-slots instance-class)
	for slot-name = (closer-mop:slot-definition-name slot-definition)
	when (slot-boundp instance slot-name)
	do (setf (slot-value object slot-name)
		 (slot-value instance slot-name))
	finally (return object)))

(defun same-persisted-object-p (first second)
  (ignore-errors 
    (string-equal (id first) (id second))))


;;this is the local function to exclude id and rev  value from object  value list
;;for the objects like job application object but retain the ones in sub objects like resume etc.

(defun remove-id-and-rev (document)
  (remove (assoc :rev document) (remove (assoc :id document) document)))

;;this is to check if the object still contain clos sub objects in its slots

(defun has-clos-sub-object-p (object)
  (ignore-errors 
    (some #'clos-object-p (retrieve-object-slot-values object))))

;;this function is to return all sub objects of the argument passed
;;it includes the object itself in the list as well

;;accumulator style

(defun retrieve-all-sub-objects (object)
  (if (not (clos-object-p object))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "retrieve-all-sub-objects-of : wrong argument type" 
	       :reason "arguments passed are not clos object"))
  (labels ((return-list (object accumulator)
	     (cond
	       ((or (stringp object) (integerp object) (null object)) 
		accumulator)
	       ((and (clos-object-p object) (not (has-clos-sub-object-p object)))
		(cons object accumulator))
	       ((and (clos-object-p object) (has-clos-sub-object-p object))
		(cons object (return-list (retrieve-object-slot-values object) accumulator)))
	       ((listp object)
		(return-list (car object) (return-list (cdr object) accumulator))))))
    (return-list object nil)))

;;checking if the object has itself referenced in its sub objects
;;it retrieves all the sub object + the object itself and check if
;;the occurence of the object has a duplicate.
;;if it is the case, it self referenced itself in its sub objects
;;else it is not.

(defun non-cyclic-clos-object-p (object)
  (not   
    (duplicate object (retrieve-all-sub-objects object) :test #'same-persisted-object-p)))


;;this method just a simple method to delete all objects in the list passed as argument

(defun erase-all (object-list)
  (if (not (all-elements-are-persisted-object-p object-list))
      (error 'careerpacific-invalid-arguments-error :error-number 2001 
	       :error-type "delete-all : wrong argument type" 
	       :reason "arguments passed are not clos object list"))
  (if object-list
      (iterate:iterate (iterate:for object iterate:in object-list)
		       (erase object))))
		   