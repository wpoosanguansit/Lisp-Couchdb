(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((select-block select-block))
  (if (not (dirty-marker select-block))
      (values select-block t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'select-block (list (id select-block))) ()  
			     (value-list-of select-block 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev select-block) rev)
	       (setf (dirty-marker select-block) nil)
	       (values select-block ok)))))

(defmethod save-or-update ((select-block select-block))
  (if (persisted-object-p select-block)
      (update select-block)
      (save select-block)))

;;check the condition before saving

(defmethod save :before ((select-block select-block))
  (if (deleted-persisted-object-p select-block)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "select-block illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user select-block)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save select-block initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'select-block-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'select-block id) () () ()))
  (make-select-block document))

(defmethod retrieve ((object (eql 'select-block-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(select-block _view select-block select-block-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-select-block documents))

(defmethod erase ((select-block select-block))
  (let* ((ok) (object)
	 (prior-select-block (copy-instance select-block))) 
    (unwind-protect
	 (progn
	   (setf (status prior-select-block) "deleted")
	   (setf (updated-date prior-select-block) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-select-block)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev select-block) (rev object))
	(setf (status select-block) "deleted")
	(setf (updated-date select-block) (get-today-date))
	(setf (dirty-marker select-block) nil)))
    (values select-block ok)))

(defmethod erase :before ((select-block select-block))
  (if (deleted-persisted-object-p select-block)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "select-block illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p select-block))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase select-block initial argument"
				     :reason "select-block has to be persisted before it can be erased")))

(defmethod update ((select-block select-block))
  (if (not (dirty-marker select-block))
      (values select-block t)
      (progn
	(setf (updated-date select-block) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'select-block (id select-block)) () 
			(substitute-item-in-list 
			 (value-list-of select-block 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev select-block) rev)
	  (setf (dirty-marker select-block) nil)
	  (values select-block ok)))))

(defmethod udpate :before ((select-block select-block))
  (if (deleted-persisted-object-p select-block)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "select-block illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p select-block))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update select-block initial argument"
				     :reason "select-block has to be persisted before it can be updated")))

(defun make-select-block (document)
  (make-instance 'select-block 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :select-block-value (cdr (assoc :select-block-value document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-select-block-complete (document)
  (make-instance 'select-block 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :select-block-value (cdr (assoc :select-block-value document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))