(in-package :careerpacific)

(export '(retrieve save delete update))

(defmethod save ((user user))
  (if (not (dirty-marker user))
      (values user t)
      (progn
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list* 'user (list (id user))) ()  
			(value-list-of user 'as-alist 'without '(:ID :REV :DIRTY-MARKER))
			cl-couchdb-client:+json-content-type+)
	  (declare (ignore ok_ id_ rev_))
	  (setf (rev user) rev)
	  (setf (dirty-marker user) nil)
	  (values user ok)))))

(defmethod save-or-update ((user user))
  (if (persisted-object-p user)
      (update user)
      (save user)))

;;check the condition before saving

(defmethod save :before ((user user))
  (if (deleted-persisted-object-p user)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal operation"
	     :reason "object is deleted, it can not be saved")))
  

(defmethod retrieve ((object (eql 'user-by-id)) id)
  (setf document 
	(db-request :get cl-couchdb-client:*couchdb-server* 
					  (list 'user id) () () ()))
  (make-user document))

(defmethod erase ((user user))
  (let* ((ok) (object)
	 (prior-user (copy-instance user)))
    (unwind-protect
	 (progn
	   (setf (status prior-user) "deleted")
	   (setf (updated-date prior-user) (get-today-date))
	   (multiple-value-bind (_object _ok) (update prior-user)
	     (setf object _object) 
	     (setf ok _ok)))
      (when ok 
	(setf (rev user) (rev object))
	(setf (status user) "deleted")
	(setf (updated-date user) (get-today-date))
	(setf (dirty-marker user) nil)))
    (values user ok)))

(defmethod erase :before ((user user))
  (if (deleted-persisted-object-p user)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal operation"
	     :reason "object is deleted, it can not be erased"))
  (if (not (persisted-object-p user))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "erase user initial argument"
				     :reason "user has to be persisted before updating")))

;;erase all of the related sub objects

(defmethod erase :after ((user user))
  (erase-all (retrieve 'company-profile-by-user-id (id user)))
  (erase-all (retrieve 'education-by-user-id (id user)))
  (erase-all (retrieve 'email-by-user-id (id user)))
  (erase-all (retrieve 'interview-question-by-user-id (id user)))
  (erase-all (retrieve 'job-application-by-user-id (id user)))
  (erase-all (retrieve 'language-by-user-id (id user)))
  (erase-all (retrieve 'payment-by-user-id (id user)))
  (erase-all (retrieve 'phone-number-by-user-id (id user)))
  (erase-all (retrieve 'picture-by-user-id (id user)))
  (erase-all (retrieve 'reference-by-user-id (id user)))
  (erase-all (retrieve 'resume-by-user-id (id user)))
  (erase-all (retrieve 'select-block-by-user-id (id user)))
  (erase-all (retrieve 'skill-by-user-id (id user)))
  (erase-all (retrieve 'website-by-user-id (id user)))
  (erase-all (retrieve 'work-experience-by-user-id (id user))))

(defmethod update ((user user))
  (if (not (dirty-marker user))
      (values user t)
      (progn
	(setf (updated-date user) (get-today-date))
	(destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
	    (db-request :put cl-couchdb-client:*couchdb-server* 
			(list 'user (id user)) () 
			(substitute-item-in-list 
			 (value-list-of user 'as-alist 'without '(:ID :DIRTY-MARKER)) 
			 ':REV ':_REV)
			cl-couchdb-client:+json-content-type+)		 
	  (declare (ignore id_ ok_ rev_))
	  (setf (rev user) rev)
	  (setf (dirty-marker user) nil)
	  (values user ok)))))

(defmethod update :before ((user user))
  (if (deleted-persisted-object-p user)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal operation"
	     :reason "object is deleted, it can not be updated"))
  (if (not (persisted-object-p user))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "update user initial argument"
				     :reason "user has to be persisted before it can be updated")))

(defun make-user (document)
  (make-instance 'user 
		 :id (or (cdr (assoc :_id document)) (cdr (assoc :id document)))
		 :rev (or (cdr (assoc :_rev document)) (cdr (assoc :rev document)))
		 :status (cdr (assoc :status document))
		 :first-name (cdr (assoc :first-name document))
		 :middle-name (cdr (assoc :middle-name document))
		 :last-name (cdr (assoc :last-name document))
		 :user-name (cdr (assoc :user-name document))
		 :password (cdr (assoc :password document))
		 :display-name (cdr (assoc :display-name document))
		 :user-type (cdr (assoc :user-type document))
		 :created-date (cdr (assoc :created-date document))
		 :updated-date (cdr (assoc :updated-date document))
		 :dirty-marker nil))