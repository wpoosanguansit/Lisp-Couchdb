 (in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((skill skill))
  (if (not (dirty-marker skill))
      (values skill t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'skill (list (id skill))) ()  
			     (value-list-of skill 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev skill) rev)
	       (setf (dirty-marker skill) nil)
	       (values skill ok)))))

(defmethod save-or-update ((skill skill))
  (if (persisted-object-p skill)
      (update skill)
      (save skill)))

;;check the condition before saving

(defmethod save :before ((skill skill))
  (if (deleted-persisted-object-p skill)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user skill)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save skill initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'skill-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'skill id) () () ()))
  (make-skill document))

(defmethod retrieve ((object (eql 'skill-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(skill _view skill skill-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-skill documents))

(defmethod erase ((skill skill))
  (let* ((ok) (object)
	 (prior-skill (copy-instance skill))) 
    (unwind-protect
	 (progn
	   (setf (status prior-skill) "deleted")
	   (setf (updated-date prior-skill) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-skill)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev skill) (rev object))
	(setf (status skill) "deleted")
	(setf (updated-date skill) (get-today-date))
	(setf (dirty-marker skill) nil)))
    (values skill ok)))

(defmethod erase :before ((skill skill))
  (if (deleted-persisted-object-p skill)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p skill))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase skill initial argument"
				     :reason "skill has to be persisted before it can be erased")))

(defmethod update ((skill skill))
  (if (not (dirty-marker skill))
      (values skill t)
      (progn
	(setf (updated-date skill) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'skill (id skill)) () 
			(substitute-item-in-list 
			 (value-list-of skill 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev skill) rev)
	  (setf (dirty-marker skill) nil)
	  (values skill ok)))))

(defmethod udpate :before ((skill skill))
  (if (deleted-persisted-object-p skill)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p skill))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update skill initial argument"
				     :reason "skill has to be persisted before it can be updated")))

(defun make-skill (document)
  (make-instance 'skill 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :skill-name (cdr (assoc :skill-name document))
		 :description (cdr (assoc :description document))
		 :skill-level (cdr (assoc :skill-level document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-skill-complete (document)
  (make-instance 'skill 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :skill-name (cdr (assoc :skill-name document))
		 :description (cdr (assoc :description document))
		 :skill-level (cdr (assoc :skill-level document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))
