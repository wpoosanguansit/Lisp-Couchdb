(in-package :careerpacific)

(export '())

(defclass work-experience ()
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (company-name    :initarg :company-name
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":company-name must be provided.")
		    :accessor company-name)
   (company-industry   :initarg :company-industry
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":company-industry must be provided.")
		       :accessor company-industry)
   (compensation    :initarg :compensation
	            :initform nil
	            :accessor compensation)
   (other-compensation          :initarg :other-compensation
				:initform nil
				:accessor other-compensation)
   (location        :initarg :location
		    :initform nil
		    :accessor location)
   (designation     :initarg :designation
		    :initform nil
		    :accessor designation)
   (position-level  :initarg :position-level
		    :initform nil
		    :accessor position-level)
   (responsibility  :initarg :responsibility
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":responsibility must be provided.")
		    :accessor responsibility)
   
   (reason-for-leaving     :initarg :reason-for-leaving
			   :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":reson-for-leaving must be provided.")
			   :accessor reason-for-leaving)
   (start-date      :initarg :start-date
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":start-date must be provided.")
		    :accessor start-date)
   (end-date        :initarg :end-date
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason ":end-date must be provided.")
		    :accessor end-date)
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
;;the unique random no of work-experience
;;(if (not (stringp (resume self)))
;;      (error 'careerpacific-invalid-arguments-error
;;				     :error-number 2001 :error-type "work-experience initial argument"
;;				     :reason "resume has to be string. 
;;                                      it has to be in the format \"user@email.com_resume_1\""))

(defmethod initialize-instance :after ((self work-experience) &rest args)
  (if (not (and (eql 'user (type-of (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason "user has to be a persisted user object"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "work_experience"))))

;;immutable slots

;;(defmethod (setf created-date) :before (value (self work-experience))
;;  (if (deleted-persisted-object-p self)
;;      (error 'careerpacific-illegal-operation-error
;;				     :error-number 3002 :error-type "work-experience illegal setf"
;;				     :reason "object is deleted, it can not be setf"))
;;  (error 'careerpacific-illegal-operation-error
;;				     :error-number 3001 :error-type "work-experience illegal setf"
;;				     :reason "created-date is immutable, it can not be setf"))

;;check arguement passed

(defmethod (setf id) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
				     :error-number 3002 :error-type "work-experience illegal setf"
				     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience setf argument"
				     :reason "id has to be string. 
                                      it has to be in the format \"user@email.com_work_experience_1\"")))

(defmethod (setf rev) :before (value (self work-experience))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience setf argument"
				     :reason "rev has to be string")))


(defmethod (setf user) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
				     :error-number 3002 :error-type "work-experience illegal setf"
				     :reason "object is deleted, it can not be setf"))
  (if (not (and (eql 'user (type-of value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason "user has to be a persisted user object")))

(defmethod (setf company-name) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "company-name passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf company-industry) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "company-industry passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf compensation) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience setf argument"
				     :reason (format nil "compensation passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf other-compensation) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "other-compensation passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf location) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "location passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf designation) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "designation passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf position-level) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "position-level passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf responsibility) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "responsibility passed : ~a is not valid, 
                                      it has to be string" value))))

(defmethod (setf reason-for-leaving) :before (value (self work-experience))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience setf argument"
				     :reason "reason-for-leaving passed is not valid, 
                                      it has to be string : ~a." value)))

(defmethod (setf status) :before (value (self work-experience))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "work-experience illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self work-experience))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "work-experience setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self work-experience))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "work-experience setf argument"
				     :reason (format nil "dirty-marker passed is not a valid value : ~a. it has to be one 
                                      of nil or t" value))))

;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self work-experience))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self work-experience))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self work-experience))
  (setf (dirty-marker self) t))


(defmethod (setf company-name) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf company-industry) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf compensation) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf other-compensation) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf location) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf designation) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf position-level) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf responsibility) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf start-date) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf end-date) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf reason-for-leaving) :after (value (self work-experience))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self work-experience))
  (setf (dirty-marker self) t))


(defmethod (setf updated-date) :after (value (self work-experience))
  (setf (dirty-marker self) t))

;;methods for accessing the slot

(defmethod user :before ((self work-experience))
  )

