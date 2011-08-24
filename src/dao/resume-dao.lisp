(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((resume resume))
  (if (not (dirty-marker resume))
      (values resume t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			     (list* 'resume (list (id resume))) ()  
			     (value-list-of resume 'as-alist 'without '(:ID :REV :DIRTY-MARKER) 'sub-object-as-id)
			     cl-couchdb-client:+json-content-type+)
	       (declare (ignore ok_ id_ rev_))
	       (setf (rev resume) rev)
	       (setf (dirty-marker resume) nil)
	       (values resume ok)))))

(defmethod save-or-update ((resume resume))
  (if (persisted-object-p resume)
      (update resume)
      (save resume)))

;;check the condition before saving

(defmethod save :before ((resume resume))
  (if (deleted-persisted-object-p resume)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal operation"
	     :reason "object is deleted, it can not be saved"))
  (if (not (persisted-object-p (user resume)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "save resume initial argument"
				     :reason "user has to be persisted before saving")))
  

(defmethod retrieve ((object (eql 'resume-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'resume id) () () ()))
  (make-resume document))

(defmethod retrieve ((object (eql 'resume-by-user-id)) id)
  (setf documents (unwrap-query-wrapper 
	      (db-request :get cl-couchdb-client:*couchdb-server* 
						'(resume _view resume resume-by-user-id) 
						(prepare-keys (list :key id)))))
  (mapcar #'make-resume documents))

(defmethod erase ((resume resume))
  (let* ((ok) (object)
	 (prior-resume (copy-instance resume))) 
    (unwind-protect
	 (progn
	   (setf (status prior-resume) "deleted")
	   (setf (updated-date prior-resume) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-resume)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev resume) (rev object))
	(setf (status resume) "deleted")
	(setf (updated-date resume) (get-today-date))
	(setf (dirty-marker resume) nil)))
    (values resume ok)))

(defmethod erase :before ((resume resume))
  (if (deleted-persisted-object-p resume)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p resume))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase resume initial argument"
				     :reason "resume has to be persisted before it can be erased")))

(defmethod erase :after ((resume resume))
  (iterate:iterate (iterate:for resume-review iterate:in (resume-reviews resume)) 
		    (erase resume-review)))

(defmethod update ((resume resume))
  (if (not (dirty-marker resume))
      (values resume t)
      (progn
	(setf (updated-date resume) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'resume (id resume)) () 
			(substitute-item-in-list 
			 (value-list-of resume 'as-alist 'without '(:ID :DIRTY-MARKER) 'sub-object-as-id) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev resume) rev)
	  (setf (dirty-marker resume) nil)
	  (values resume ok)))))

(defmethod udpate :before ((resume resume))
  (if (deleted-persisted-object-p resume)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p resume))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update resume initial argument"
				     :reason "resume has to be persisted before it can be udpated")))

(defun make-resume (document)
  (make-instance 'resume 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (retrieve 'user-by-id (cdr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :addresses (retrieve-object-list 'address-by-id (cdr (assoc :addresses document)))
		 :phone-numbers (retrieve-object-list 'phone-number-by-id (cdr (assoc :phone-numbers document)))
		 :emails (retrieve-object-list 'email-by-id (cdr (assoc :emails document)))
		 :educations (retrieve-object-list 'education-by-id (cdr (assoc :educations document)))
		 :work-experiences (retrieve-object-list 'work-experience-by-id (cdr (assoc :work-experiences document)))
		 :skills (retrieve-object-list 'skill-by-id (cdr (assoc :skills document)))
		 :pictures (retrieve-object-list 'picture-by-id (cdr (assoc :pictures document)))
		 :references (retrieve-object-list 'reference-by-id (cdr (assoc :references document)))
		 :languages (retrieve-object-list 'language-by-id (cdr (assoc :languages document)))
		 :websites (retrieve-object-list 'website-by-id (cdr (assoc :websites document)))
		 :resume-reviews (retrieve-object-list 'resume-review-by-id (cdr (assoc :resume-reviews document)))
		 :availability (cdr (assoc :availability document))
		 :expected-salary (cdr (assoc :expected-salary document))
		 :current-salary (cdr (assoc :current-salary document))
		 :consultant-note (cdr (assoc :consultant-note document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))

(defun make-resume-complete (document)
  (make-instance 'resume 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :user (make-user (cadr (assoc :user document)))
		 :status (cdr (assoc :status document))
		 :addresses (retrieve-object-complete-list #'make-address-complete (cdr (assoc :addresses document)))
		 :phone-numbers (retrieve-object-complete-list #'make-phone-number-complete 
							       (cdr (assoc :phone-numbers document)))
		 :emails (retrieve-object-complete-list #'make-email-complete (cdr (assoc :emails document)))
		 :educations (retrieve-object-complete-list #'make-education-complete (cdr (assoc :educations document)))
		 :work-experiences (retrieve-object-complete-list #'make-work-experience-complete 
								  (cdr (assoc :work-experiences document)))
		 :skills (retrieve-object-complete-list #'make-skill-complete (cdr (assoc :skills document)))
		 :pictures (retrieve-object-complete-list #'make-picture-complete (cdr (assoc :pictures document)))
		 :references (retrieve-object-complete-list #'make-reference-complete (cdr (assoc :references document)))
		 :languages (retrieve-object-complete-list #'make-language-complete (cdr (assoc :languages document)))
		 :websites (retrieve-object-complete-list #'make-website-complete (cdr (assoc :websites document)))
		 :resume-reviews (retrieve-object-complete-list #'make-resume-review-complete 
								(cdr (assoc :resume-reviews document)))
		 :availability (cdr (assoc :availability document))
		 :expected-salary (cdr (assoc :expected-salary document))
		 :current-salary (cdr (assoc :current-salary document))
		 :consultant-note (cdr (assoc :consultant-note document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))