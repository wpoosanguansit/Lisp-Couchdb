(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((education education))
  (if (not (dirty-marker education))
      (values education t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'education (list (id education))) ()  
			     (value-list-of education 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev education) rev)
	       (setf (dirty-marker education) nil)
	       (values education ok)))))

(defmethod save-or-update ((education education))
  (if (persisted-object-p education)
      (update education)
      (save education)))

;;check the condition before saving

(defmethod save :before ((education education))
  (if (deleted-persisted-object-p education)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "education illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user education)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save education initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'education-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'education id) () () ()))
  (make-education document))

(defmethod retrieve ((object (eql 'education-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(education _view education education-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-education documents))

(defmethod erase ((education education))
  (let* ((ok) (object)
	 (prior-education (copy-instance education))) 
    (unwind-protect
	 (progn
	   (setf (status prior-education) "deleted")
	   (setf (updated-date prior-education) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-education)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev education) (rev object))
	(setf (status education) "deleted")
	(setf (updated-date education) (get-today-date))
	(setf (dirty-marker education) nil)))
    (values education ok)))

(defmethod erase :before ((education education))
  (if (deleted-persisted-object-p education)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "education illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p education))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase education initial argument"
				     :reason "education has to be persisted before it can be erased")))

(defmethod update ((education education))
  (if (not (dirty-marker education))
      (values education t)
      (progn
	(setf (updated-date education) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'education (id education)) () 
			(substitute-item-in-list 
			 (value-list-of education 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev education) rev)
	  (setf (dirty-marker education) nil)
	  (values education ok)))))

(defmethod udpate :before ((education education))
  (if (deleted-persisted-object-p education)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "education illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p education))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update education initial argument"
				     :reason "education has to be persisted before it can be updated")))

(defun make-education (document)
  (make-instance 'education
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :education-level (cdr (assoc :education-level document))
		 :institution-name (cdr (assoc :institution-name document))
		 :location (cdr (assoc :location document))
		 :comment (cdr (assoc :comment document))
		 :graduation-date (cdr (assoc :graduation-date document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-education-complete (document)
  (make-instance 'education
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :education-level (cdr (assoc :education-level document))
		 :institution-name (cdr (assoc :institution-name document))
		 :location (cdr (assoc :location document))
		 :comment (cdr (assoc :comment document))
		 :graduation-date (cdr (assoc :graduation-date document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))