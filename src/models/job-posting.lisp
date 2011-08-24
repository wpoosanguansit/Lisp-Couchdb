(in-package :careerpacific)

(export '())

(defclass job-posting ()  
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
 				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (job-title       :initarg :job-title
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":job-title must be provided.")
	            :accessor job-title)
   (job-type        :initarg :job-type
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":job-type must be provided.")
		    :accessor job-type)
   (job-category    :initarg :job-category
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":job-category must be provided.")
		    :accessor job-category)
   (work-location   :initarg :work-location
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":work-location must be provided.")
		    :accessor work-location)
   (responsibility  :initarg :responsibility
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":responsibility must be provided.")
		    :accessor responsibility)
   (education-level :initarg :education-level
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":edication-level must be provided.")
		    :accessor education-level)
   (requirements    :initarg :requirements
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason ":requirements must be provided.")
		    :accessor requirements)
   (salary-budget   :initarg :salary-budget
		    :initform nil
		    :accessor salary-budget)
   (traveling-requirement    :initarg :traveling-requirement
			     :initform nil
			     :accessor traveling-requirement)
   (posting-start-date       :initarg :posting-start-date
			     :initform (error 'careerpacific-invalid-arguments-error
					      :error-number 2001 :error-type "job-posting initial argument"
					      :reason ":posting-start-date must be provided.")
			     :accessor posting-start-date)
   (posting-end-date         :initarg :posting-end-date
			     :initform (error 'careerpacific-invalid-arguments-error
					      :error-number 2001 :error-type "job-posting initial argument"
					      :reason ":posting-start-date must be provided.")
			     :accessor posting-end-date)
   (keywords-for-search      :initarg :keywords-for-search
			     :initform nil
			     :accessor keywords-for-search)
   (access-list     :initarg :access-list
		    :initform nil
		    :accessor access-list)
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

(defmethod initialize-instance :after ((self job-posting) &rest args)
  (if (not (and (string-equal "employer" (user-type (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason "user passed has to be a persisted employer user object"))
  (if (not (stringp (job-title self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "job-title has to be a string"))
  (if (not (stringp (job-category self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "job-category has to be a string"))
  (if (not (stringp (work-location self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "work-location has to be a string"))
  (if (not (stringp (responsibility self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "responsibility has to be a string"))
  (if (not (member (education-level self) *valid-education-level* :test #'string-equal))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason (format nil "education-level passed is not valid string 
                                              : ~a. it has to be one of these values : ~{~a~^, ~}" 
						     (education-level self) *valid-education-level*)))
  (if (not (stringp (requirements self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "requirements has to be a string"))
  (if (not (stringp (traveling-requirement self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "traveling-requirement has to be a string"))
  (if (not (stringp (posting-start-date self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting initial argument"
	     :reason "posting-start-date has to be a string"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "job-posting initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "job_posting"))))

;;immutable slot

;;(defmethod (setf created-date) :before (value (self job-posting))
;;  (error 'careerpacific-illegal-operation-error
;;	 :error-number 3001 :error-type "job-posting illegal setf"
;;	 :reason "created-date is immutable, it can not be setf"))

;;check the arguments

(defmethod (setf id) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason "id has to be string. 
                                      it has to be in the format \"user@job-posting.com_job-posting_1\"")))

(defmethod (setf rev) :before (value (self job-posting))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason "rev has to be string")))

(defmethod (setf user) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (string-equal "employer" (user-type value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason "user has to be an id string or persisted employer user object")))


(defmethod (setf job-title) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "job-title passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf job-type) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "job-type passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf job-category) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "job-category passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf work-location) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "work-location passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf responsiblity) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "responsibility passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf education-level) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "education-level passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf requirements) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "requirements passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf salary-budget) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "salary-budget passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf traveling-requriement) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "traveling-requirement passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf posting-start-date) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "posting-start-date passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf posting-end-date) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "posting-end-date passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf keywords-for-search) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "keywords-for-search passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf access-list) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (all-elements-are-string-p value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil ":access-list passed : ~{~a~^, ~} is not valid, 
                                      it has to be a list of string" value))))

(defmethod (setf status) :before (value (self job-posting))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "job-posting illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self job-posting))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self job-posting))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "job-posting setf argument"
	     :reason (format nil "dirty-marker passed : ~a  is not a valid value. it has to be one                                       of nil or t" value))))

;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self job-posting))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self job-posting))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf job-title) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf job-type) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf job-category) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf work-location) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf responsibility) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf education-level) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf requirements) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf salary-budget) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf traveling-requirement) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf posting-start-date) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf posting-end-start) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf keywords-for-search) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf access-list) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self job-posting))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self job-posting))
  (setf (dirty-marker self) t))

;;methods accessing the slots

(defmethod user :before ((self job-posting))
  )