(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((reference reference))
  (if (not (dirty-marker reference))
      (values reference t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'reference (list (id reference))) ()  
			     (value-list-of reference 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev reference) rev)
	       (setf (dirty-marker reference) nil)
	       (values reference ok)))))

(defmethod save-or-update ((reference reference))
  (if (persisted-object-p reference)
      (update reference)
      (save reference)))

;;check the condition before saving

(defmethod save :before ((reference reference))
  (if (deleted-persisted-object-p reference)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user reference)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save reference initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'reference-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'reference id) () () ()))
  (make-reference document))

(defmethod retrieve ((object (eql 'reference-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(reference _view reference reference-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-reference documents))

(defmethod erase ((reference reference))
  (let* ((ok) (object)
	 (prior-reference (copy-instance reference))) 
    (unwind-protect
	 (progn
	   (setf (status prior-reference) "deleted")
	   (setf (updated-date prior-reference) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-reference)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev reference) (rev object))
	(setf (status reference) "deleted")
	(setf (updated-date reference) (get-today-date))
	(setf (dirty-marker reference) nil)))
    (values reference ok)))

(defmethod erase :before ((reference reference))
  (if (deleted-persisted-object-p reference)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p reference))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase reference initial argument"
				     :reason "reference has to be persisted before it can be erased")))

(defmethod update ((reference reference))
  (if (not (dirty-marker reference))
      (values reference t)
      (progn
	(setf (updated-date reference) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'reference (id reference)) () 
			(substitute-item-in-list 
			 (value-list-of reference 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev reference) rev)
	  (setf (dirty-marker reference) nil)
	  (values reference ok)))))

(defmethod udpate :before ((reference reference))
  (if (deleted-persisted-object-p reference)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p reference))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update reference initial argument"
				     :reason "reference has to be persisted before it can be updated")))

(defun make-reference (document)
  (make-instance 'reference 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :name (cdr (assoc :name document))
		 :company (cdr (assoc :company document))
		 :designation (cdr (assoc :designation document))
		 :relationship (cdr (assoc :relationship document))
		 :contact-email (cdr (assoc :contact-email document))
		 :contact-phone (cdr (assoc :contact-phone document))
		 :number-of-years (cdr (assoc :number-of-years document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-reference-complete (document)
  (make-instance 'reference 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :name (cdr (assoc :name document))
		 :company (cdr (assoc :company document))
		 :designation (cdr (assoc :designation document))
		 :relationship (cdr (assoc :relationship document))
		 :contact-email (cdr (assoc :contact-email document))
		 :contact-phone (cdr (assoc :contact-phone document))
		 :number-of-years (cdr (assoc :number-of-years document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))