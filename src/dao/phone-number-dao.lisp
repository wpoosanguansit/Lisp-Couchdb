(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((phone-number phone-number))
  (if (not (dirty-marker phone-number))
      (values phone-number t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'phone-number (list (id phone-number))) ()  
			     (value-list-of phone-number 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev phone-number) rev)
	       (setf (dirty-marker phone-number) nil)
	       (values phone-number ok)))))

(defmethod save-or-update ((phone-number phone-number))
  (if (persisted-object-p phone-number)
      (update phone-number)
      (save phone-number)))

;;check the condition before saving

(defmethod save :before ((phone-number phone-number))
  (if (deleted-persisted-object-p phone-number)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user phone-number)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save phone-number initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'phone-number-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'phone-number id) () () ()))
  (make-phone-number document))

(defmethod retrieve ((object (eql 'phone-number-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(phone-number _view phone-number phone-number-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-phone-number documents))

(defmethod erase ((phone-number phone-number))
  (let* ((ok) (object)
	 (prior-phone-number (copy-instance phone-number))) 
    (unwind-protect
	 (progn
	   (setf (status prior-phone-number) "deleted")
	   (setf (updated-date prior-phone-number) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-phone-number)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev phone-number) (rev object))
	(setf (status phone-number) "deleted")
	(setf (updated-date phone-number) (get-today-date))
	(setf (dirty-marker phone-number) nil)))
    (values phone-number ok)))

(defmethod erase :before ((phone-number phone-number))
  (if (deleted-persisted-object-p phone-number)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p phone-number))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase phone-number initial argument"
				     :reason "phone-number has to be persisted before it can be erased")))

(defmethod update ((phone-number phone-number))
  (if (not (dirty-marker phone-number))
      (values phone-number t)
      (progn
	(setf (updated-date phone-number) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'phone-number (id phone-number)) () 
			(substitute-item-in-list 
			 (value-list-of phone-number 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev phone-number) rev)
	  (setf (dirty-marker phone-number) nil)
	  (values phone-number ok)))))

(defmethod udpate :before ((phone-number phone-number))
  (if (deleted-persisted-object-p phone-number)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p phone-number))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update phone-number initial argument"
				     :reason "phone-number has to be persisted before it can be updated")))		  

(defun make-phone-number (document)
  (make-instance 'phone-number 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :phone-number-type (cdr (assoc :phone-number-type document))
		 :phone-number (cdr (assoc :phone-number document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-phone-number-complete (document)
  (make-instance 'phone-number 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :phone-number-type (cdr (assoc :phone-number-type document))
		 :phone-number (cdr (assoc :phone-number document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))
