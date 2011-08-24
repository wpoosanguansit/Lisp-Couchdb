(in-package :careerpacific)

(export '())

(defclass reference ()
  ((id              :initarg :id
		    :initform nil
		    :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (name            :initarg :name
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":name must be provided.")
		    :accessor name)
   (relationship    :initarg :relationship
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":relationship must be provided.")
		    :accessor relationship)
   (contact-email   :initarg :contact-email
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":contact-email must be provided.")
	            :accessor contact-email)
   (contact-phone   :initarg :contact-phone
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":contact-phone must be provided.")
		    :accessor contact-phone)
   (company         :initarg :company
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":company must be provided.")
		    :accessor company)
   (designation     :initarg :designation
		    :initform nil
		    :accessor designation)
   (number-of-years :initarg :number-of-years
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason ":number-of-years must be provided.")
		    :accessor number-of-years)
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
;;the unique random no of reference
;;(if (not (stringp (resume self)))
;;      (error 'careerpacific-invalid-arguments-error
;;				     :error-number 201 :error-type "reference initial argument"
;;				     :reason "resume has to be a string. 
;;                                      it has to be in the format \"user@email.com_resume_1\""))

(defmethod initialize-instance :after ((self reference) &rest args)
  (if (not (and (eql 'user (type-of (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason "user has to be an id string or persisted user object"))
  (if (not (stringp (name self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference initial argument"
	     :reason "name has to be a string"))
  (if (not (stringp (relationship self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference initial argument"
	     :reason "relationship has to be a string"))
  (if (not (stringp (contact-email self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference initial argument"
	     :reason "contact-email has to be a string"))
  (if (not (stringp (contact-phone self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference initial argument"
	     :reason "contact-phone has to be a string"))
  (if (not (stringp (company self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference initial argument"
	     :reason "company has to be a string"))
  (if (not (stringp (number-of-years self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference initial argument"
	     :reason "number-of-years has to be a integer string 0-9"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 201 :error-type "reference initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "reference"))))

;;immutable slot

;;(defmethod (setf created-date) :before (value (self reference))
;;  (error 'careerpacific-illegal-operation-error
;;	 :error-number 3001 :error-type "reference illegal setf"
;;	 :reason "created-date is immutable, it can not be setf"))

;;check the arguments

(defmethod (setf id) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason "id has to be a string. 
              it has to be in the format \"user@email.com_reference_1\"")))

(defmethod (setf rev) :before (value (self reference))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason "rev has to be a string")))

(defmethod (setf user) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (eql 'user (type-of value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason "user has to be an id string or persisted user object. 
             in case of string, it has to be in the format \"user@email.com\"")))


(defmethod (setf name) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf contact-email) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "contact-email passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf contact-phone) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "contact-phone passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf company) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "company passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf number-of-year) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "number-of-year passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf designation) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "designation passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf status) :before (value (self reference))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "reference illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self reference))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "reference setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self reference))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 201 :error-type "reference setf argument"
	     :reason (format nil "dirty-marker passed : ~a  is not a valid value. it has to be one                                       of nil or t" value))))


;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self reference))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self reference))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf name) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf contact-email) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf contact-phone) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf company) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf number-of-year) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf designation) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self reference))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self reference))
  (setf (dirty-marker self) t))

;;methods for accessing the slots

(defmethod user :before ((self reference))
  )

