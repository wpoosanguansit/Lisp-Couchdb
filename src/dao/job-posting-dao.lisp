(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((job-posting job-posting))
  (if (not (dirty-marker job-posting))
      (values job-posting t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'job-posting (list (id job-posting))) ()  
			     (value-list-of job-posting 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev job-posting) rev)
	       (setf (dirty-marker job-posting) nil)
	       (values job-posting ok)))))

(defmethod save-or-update ((job-posting job-posting))
  (if (persisted-object-p job-posting)
      (update job-posting)
      (save job-posting)))

;;check the condition before saving

(defmethod save :before ((job-posting job-posting))
  (if (deleted-persisted-object-p job-posting)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user job-posting)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save job-posting initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'job-posting-by-id)) id)
  (setf doc 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'job-posting id) () () ()))
  (make-job-posting doc))

(defmethod retrieve ((object (eql 'job-posting-by-user-id)) id)
  (setf docs (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(job-posting _view job-posting job-posting-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-job-posting docs))

(defmethod erase ((job-posting job-posting))
  (let* ((ok) (object)
	 (prior-job-posting (copy-instance job-posting))) 
    (unwind-protect
	 (progn
	   (setf (status prior-job-posting) "deleted")
	   (setf (updated-date prior-job-posting) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-job-posting)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev job-posting) (rev object))
	(setf (status job-posting) "deleted")
	(setf (updated-date job-posting) (get-today-date))
	(setf (dirty-marker job-posting) nil)))
    (values job-posting ok)))

(defmethod erase :before ((job-posting job-posting))
  (if (deleted-persisted-object-p job-posting)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p job-posting))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase job-posting initial argument"
				     :reason "job-posting has to be persisted before it can be erased")))

(defmethod update ((job-posting job-posting))
  (if (not (dirty-marker job-posting))
      (values job-posting t)
      (progn
	(setf (updated-date job-posting) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'job-posting (id job-posting)) () 
			(substitute-item-in-list 
			 (value-list-of job-posting 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev job-posting) rev)
	  (setf (dirty-marker job-posting) nil)
	  (values job-posting ok)))))

(defmethod udpate :before ((job-posting job-posting))
  (if (deleted-persisted-object-p job-posting)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p job-posting))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update job-posting initial argument"
				     :reason "job-positng has to be persisted before it can be updated")))

(defun make-job-posting (document)
  (make-instance 'job-posting 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :job-title (cdr (assoc :job-title document))
		 :job-type (cdr (assoc :job-type document))
		 :job-category (cdr (assoc :job-category document))
		 :work-location (cdr (assoc :work-location document))
		 :responsibility (cdr (assoc :responsibility document))
		 :education-level (cdr (assoc :education-level document))
		 :requirements (cdr (assoc :requirements document))
		 :salary-budget (cdr (assoc :salary-budget document))
		 :traveling-requirement (cdr (assoc :traveling-requirement document))
		 :posting-start-date (cdr (assoc :posting-start-date document))
		 :posting-end-date (cdr (assoc :posting-end-date document))
		 :keywords-for-search (cdr (assoc :keywords-for-search document))
		 :access-list (cdr (assoc :access-list document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-job-posting-complete (document)
  (make-instance 'job-posting 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :job-title (cdr (assoc :job-title document))
		 :job-type (cdr (assoc :job-type document))
		 :job-category (cdr (assoc :job-category document))
		 :work-location (cdr (assoc :work-location document))
		 :responsibility (cdr (assoc :responsibility document))
		 :education-level (cdr (assoc :education-level document))
		 :requirements (cdr (assoc :requirements document))
		 :salary-budget (cdr (assoc :salary-budget document))
		 :traveling-requirement (cdr (assoc :traveling-requirement document))
		 :posting-start-date (cdr (assoc :posting-start-date document))
		 :posting-end-date (cdr (assoc :posting-end-date document))
		 :keywords-for-search (cdr (assoc :keywords-for-search document))
		 :access-list (cdr (assoc :access-list document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))