(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((picture picture))
  (if (not (dirty-marker picture))
      (values picture t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'picture (list (id picture))) ()  
			     (value-list-of picture 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev picture) rev)
	       (setf (dirty-marker picture) nil)
	       (values picture ok)))))

(defmethod save-or-update ((picture picture))
  (if (persisted-object-p picture)
      (update picture)
      (save picture)))

;;check the condition before saving

(defmethod save :before ((picture picture))
  (if (deleted-persisted-object-p picture)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "picture illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user picture)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save picture initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'picture-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'picture id) () () ()))
  (make-picture document))

(defmethod retrieve ((object (eql 'picture-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(picture _view picture picture-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-picture documents))

(defmethod erase ((picture picture))
  (let* ((ok) (object)
	 (prior-picture (copy-instance picture))) 
    (unwind-protect
	 (progn
	   (setf (status prior-picture) "deleted")
	   (setf (updated-date prior-picture) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-picture)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev picture) (rev object))
	(setf (status picture) "deleted")
	(setf (updated-date picture) (get-today-date))
	(setf (dirty-marker picture) nil)))
    (values picture ok)))

(defmethod erase :before ((picture picture))
  (if (deleted-persisted-object-p picture)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "picture illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p picture))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase picture initial argument"
				     :reason "picture has to be persisted before it can be erased")))

(defmethod update ((picture picture))
  (if (not (dirty-marker picture))
      (values picture t)
      (progn
	(setf (updated-date picture) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'picture (id picture)) () 
			(substitute-item-in-list 
			 (value-list-of picture 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev picture) rev)
	  (setf (dirty-marker picture) nil)
	  (values picture ok)))))

(defmethod udpate :before ((picture picture))
  (if (deleted-persisted-object-p picture)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "picture illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p picture))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update picture initial argument"
				     :reason "picture has to be persisted before it can be updated")))

(defun make-picture (document)
  (make-instance 'picture 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :picture-type (cdr (assoc :picture-type document))
		 :picture (cdr (assoc :picture document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-picture-complete (document)
  (make-instance 'picture 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :picture-type (cdr (assoc :picture-type document))
		 :picture (cdr (assoc :picture document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))
