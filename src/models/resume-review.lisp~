(in-package :careerpacific)

(export '())

(defclass resume-review ()
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason ":user must be provided.")
	            :accessor user)
   (resume-id       :initarg :resume-id
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason ":resume must be provided.")
	            :accessor resume-id)
   (review-content  :initarg :review-content
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason ":review-content must be provided.")
	            :accessor review-content)
   (created-date    :initarg :created-date
	            :initform (funcall #'get-today-date)
	            :reader created-date)
   (updated-date    :initarg :updated-date
	            :initform (funcall #'get-today-date)
	            :accessor updated-date)
   (status          :initarg :status
	            :initform "active"
	            :accessor status)
   (dirty-marker    :initarg :dirty-marker
		    :initform t
		    :accessor dirty-marker)))


;;this is for the validation of the data
;;and setting the id for the instance, the id
;;will be the combination of the resume id + 
;;the unique random no of resume-review

(defmethod initialize-instance :after ((self resume-review) &rest args)
  (if (not (and (string-equal "consultant" (user-type (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason "user has to be a persisted consultant user object"))
  (if (not (stringp (resume-id self)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason "resume-id has to be string of a persisted resume object"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason (format nil "status passed : ~a is not valid string. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (stringp (review-content self)))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume-review initial argument"
				     :reason (format nil "review-content passed is not valid, 
                                      it has to be string : ~a." (review-content self))))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "resume_review"))))

;;immutable slots

;;(defmethod (setf created-date) :before (value (self resume-review))
;;  (error 'careerpacific-illegal-operation-error
;;				     :error-number 3001 :error-type "resume-review illegal setf"
;;				     :reason "created-date is immutable, it can not be setf"))

;;check arguement passed

(defmethod (setf id) :before (value (self resume-review))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume-review illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason "id has to be string. 
                                      it has to be in the format \"user@email.com_resume-review_1\"")))

(defmethod (setf rev) :before (value (self resume-review))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason "rev has to be string")))

(defmethod (setf user) :before (value (self resume-review))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume-review illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (string-equal "consultant" (user-type value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason "user has to be a persisted consultant user object")))

(defmethod (setf resume-id) :before (value (self resume-review))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume-review illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp (resume-id self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason "resume-id has to be a string of a persisted resume object")))

(defmethod (setf review-content) :before (value (self resume-review))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume-review illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason (format nil "review-content passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf status) :before (value (self resume-review))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume-review illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self resume-review))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self resume-review))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume-review setf argument"
	     :reason (format nil "dirty-marker passed is not a valid value : ~a. it has to be one 
                                      of nil or t" value))))

;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self resume-review))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self resume-review))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self resume-review))
  (setf (dirty-marker self) t))

(defmethod (setf resume-id) :after (value (self resume-review))
  (setf (dirty-marker self) t))


(defmethod (setf review-content) :after (value (self resume-review))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self resume-review))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self resume-review))
  (setf (dirty-marker self) t))

;;method accesing the slots

(defmethod user :before ((self resume-review))
  )

