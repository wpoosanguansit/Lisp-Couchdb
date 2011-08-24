(in-package :careerpacific)

(export '())

;; User.
;;
;; Context: This represent the base class for all of the user in the system
;;
;; Rationale: All of the user types: Administrator, Staff, Employer, Candidate
;; Consultant have common attributes like first name, middle name, last name, etc.
;; Those attributes will be declared here while the specific classes, Employer, Consultant
;; etc. will declare those specific attributes to those user types.
;;
;; --Watt P. <>

(defclass user ()
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (first-name      :accessor first-name
		    :initarg :first-name
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "user initial argument"
				     :reason ":first-name must be provided.")
		    :documentation "The user's first-name.")
   (middle-name     :accessor middle-name
		    :initarg :middle-name
		    :initform nil
		    :documentation "The user's middle-name.")
   (last-name        :accessor last-name
		    :initarg :last-name
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "user initial argument"
				     :reason ":last-name must be provided.")
		    :documentation "The user's last-name.")
   (user-name       :accessor user-name
		    :initarg :user-name
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "user initial argument"
				     :reason ":user-name must be provided.") 
		    :documentation "The user's user-name ")
   (password        :accessor password
		    :initarg :password
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "user initial argument"
				     :reason ":password must be provided.") 
		    :documentation "The user's password")
   (display-name    :accessor display-name
		    :initarg :display-name
		    :initform nil)
   (user-type       :accessor user-type
		    :initarg :user-type
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "user initial argument"
				     :reason ":user-type must be provided."))
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
  
;;this is for the data validation
;;and set the default value for addresses which is a proxy object
;;holding the addresses for this user

(defmethod initialize-instance :after ((self user) &rest args)
  (if (not (stringp (first-name self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user initial argument"
	     :reason "first-name has to be a string"))
  (if (not (stringp (last-name self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user initial argument"
	     :reason "last-name has to be a string"))
  (if (not (stringp (user-name self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user initial argument"
	     :reason "user-name has to be a string"))
  (if (not (stringp (password self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user initial argument"
	     :reason "password has to be a string"))
  (if (not (stringp (user-type self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user initial argument"
	     :reason "user-type has to be a string"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "user initial argument"
				     :reason (format nil "status passed : ~a is not valid string. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (display-name self))
      (setf (display-name self) (user-name self)))
  (if (not (id self))
      (setf (id self) (user-name self))))

;;immutable slots

;;(defmethod (setf created-date) :before (value (self user))
;;  (if (deleted-persisted-object-p self)
;;      (error 'careerpacific-illegal-operation-error
;;	     :error-number 3002 :error-type "user illegal setf"
;;	     :reason "object is deleted, it can not be setf"))
;;  (error 'careerpacific-illegal-operation-error
;;	 :error-number 3001 :error-type "user illegal setf"
;;	 :reason "created-date is immutable, it can not be setf"))

(defmethod (setf user-type) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (error 'careerpacific-illegal-operation-error
	 :error-number 3001 :error-type "user illegal setf"
	 :reason "user-type is immutable, it can not be setf"))

;;check the arguments

(defmethod (setf id) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason "id has to be a string. 
                                      it has to be in the format \"user@email.com_resume_1_resume_review_1\"")))

(defmethod (setf first-name) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason "first-name has to be a string")))


(defmethod (setf middle-name) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "middle-name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf last-name) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "last-name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf user-name) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "user-name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf password) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "password passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf display-name) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "display-name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf status) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self user))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "user illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "user setf argument"
	     :reason (format nil "dirty-marker passed : ~a  is not a valid value. it has to be one                                       of nil or t" value))))

;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf rev) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf first-name) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf middle-name) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf last-name) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf user-name) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf password) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf display-name) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self user))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self user))
  (setf (dirty-marker self) t))
