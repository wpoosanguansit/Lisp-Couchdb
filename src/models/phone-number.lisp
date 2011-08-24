(in-package :careerpacific)

(export '())

(defclass phone-number ()  
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
 				     :error-number 2001 :error-type "phone-number initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (phone-number-type    :initarg :phone-number-type
	            :initform (error 'careerpacific-invalid-arguments-error
 				     :error-number 2001 :error-type "phone-number-type initial argument"
				     :reason ":phone-number-type must be provided.")
	            :accessor phone-number-type)
   (phone-number    :initarg :phone-number
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "phone-number initial argument"
				     :reason ":phone-number must be provided.")
		    :accessor phone-number)
   (status          :initarg :status
	            :initform "active"
	            :accessor status)
   (created-date    :initarg :created-date
	            :initform (funcall #'get-today-date)
	            :reader created-date)
   (updated-date    :initarg :updated-date
	            :initform (funcall #'get-today-date)
	            :accessor updated-date)
   (dirty-marker    :initarg :dirty-marker
		    :initform t
		    :accessor dirty-marker)))

;;this is for the validation of the data

(defmethod initialize-instance :after ((self phone-number) &rest args)
  (if (not (and (eql 'user (type-of (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "phone-number initial argument"
				     :reason "user has to be an id string or persisted user object"))
  (if (not (member (phone-number-type self) *valid-phone-number-type* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "phone-number initial argument"
				     :reason "phone-number-type passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (phone-number-type self) *valid-phone-number-type*))
  (if (not (stringp (phone-number self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason "phone-number has to be a string"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "phone-number initial argument"
				     :reason "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "phone_number"))))


;;immutable slot

;;(defmethod (setf created-date) :before (value (self phone-number))
;;  (error 'careerpacific-illegal-operation-error
;;	 :error-number 3001 :error-type "phone-number illegal setf"
;;	 :reason "created-date is immutable, it can not be setf"))

;;check the arguments

(defmethod (setf id) :before (value (self phone-number))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason "id has to be string. 
                                      it has to be in the format \"user@email.com_phone-number_1\"")))

(defmethod (setf rev) :before (value (self phone-number))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason "rev has to be string"))) 

(defmethod (setf user) :before (value (self phone-number))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (eql 'user (type-of value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason "user has to be an id string or a persisted user object. 
                      in case of string, it has to be in the format \"user@email.com\"")))


(defmethod (setf phone-number-type) :before (value (self phone-number))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-phone-number-type* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number initial argument"
	     :reason "phone-number-type passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-phone-number-type*)))

(defmethod (setf phone-number) :before (value (self phone-number))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason (format nil "phone-number passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf status) :before (value (self phone-number))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "phone-number illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self phone-number))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self phone-number))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "phone-number setf argument"
	     :reason (format nil "dirty-marker passed : ~a  is not a valid value. it has to be one                                       of nil or t" value))))

;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self phone-number))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self phone-number))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self phone-number))
  (setf (dirty-marker self) t))

(defmethod (setf phone-number-type) :after (value (self phone-number))
  (setf (dirty-marker self) t))

(defmethod (setf phone-number) :after (value (self phone-number))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self phone-number))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self phone-number))
  (setf (dirty-marker self) t))

;;methods accessing the slots

(defmethod user :before ((self phone-number))
  )

