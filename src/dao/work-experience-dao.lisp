(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((work-experience work-experience))
  (if (not (dirty-marker work-experience))
      (values work-experience t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'work-experience (list (id work-experience))) ()  
			     (value-list-of work-experience 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev work-experience) rev)
	       (setf (dirty-marker work-experience) nil)
	       (values work-experience ok)))))

(defmethod save-or-update ((work-experience work-experience))
  (if (persisted-object-p work-experience)
      (update work-experience)
      (save work-experience)))

;;check the condition before saving

(defmethod save :before ((work-experience work-experience))
  (if (deleted-persisted-object-p work-experience)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user work-experience)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save work-experience initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'work-experience-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'work-experience id) () () ()))
  (make-work-experience document))

(defmethod retrieve ((object (eql 'work-experience-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(work-experience _view work-experience work-experience-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-work-experience documents))

(defmethod erase ((work-experience work-experience))
  (let* ((ok) (object)
	 (prior-work-experience (copy-instance work-experience))) 
    (unwind-protect
	 (progn
	   (setf (status prior-work-experience) "deleted")
	   (setf (updated-date prior-work-experience) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-work-experience)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev work-experience) (rev object))
	(setf (status work-experience) "deleted")
	(setf (updated-date work-experience) (get-today-date))
	(setf (dirty-marker work-experience) nil)))
    (values work-experience ok)))

(defmethod erase :before ((work-experience work-experience))
  (if (deleted-persisted-object-p work-experience)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p work-experience))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase work-experience initial argument"
				     :reason "work-experience has to be persisted before it can be erased")))

(defmethod update ((work-experience work-experience))
  (if (not (dirty-marker work-experience))
      (values work-experience t)
      (progn
	(setf (updated-date work-experience) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'work-experience (id work-experience)) () 
			(substitute-item-in-list 
			 (value-list-of work-experience 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev work-experience) rev)
	  (setf (dirty-marker work-experience) nil)
	  (values work-experience ok)))))

(defmethod udpate :before ((work-experience work-experience))
  (if (deleted-persisted-object-p work-experience)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p work-experience))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update work-experience initial argument"
				     :reason "work-experience has to be persisted before it can be updated")))

(defun make-work-experience (document)
  (make-instance 'work-experience
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :company-name (cdr (assoc :company-name document))
		 :company-industry (cdr (assoc :company-industry document))
		 :compensation (cdr (assoc :compensation document))
		 :other-compensation (cdr (assoc :other-compensation document))
		 :location (cdr (assoc :location document))
		 :position-level (cdr (assoc :location document))
		 :responsibility (cdr (assoc :responsibility document))
		 :reason-for-leaving (cdr (assoc :reason-for-leaving document))
		 :start-date (cdr (assoc :start-date document))
		 :end-date (cdr (assoc :end-date document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-work-experience-complete (document)
  (make-instance 'work-experience
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :company-name (cdr (assoc :company-name document))
		 :company-industry (cdr (assoc :company-industry document))
		 :compensation (cdr (assoc :compensation document))
		 :other-compensation (cdr (assoc :other-compensation document))
		 :location (cdr (assoc :location document))
		 :position-level (cdr (assoc :location document))
		 :responsibility (cdr (assoc :responsibility document))
		 :reason-for-leaving (cdr (assoc :reason-for-leaving document))
		 :start-date (cdr (assoc :start-date document))
		 :end-date (cdr (assoc :end-date document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))
