(in-package :careerpacific)

(defclass select-block ())
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
 				     :error-number 2001 :error-type "select-block initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (consultant      :initarg :consultant
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "select-block initial argument"
				     :reason ":consultant must be provided.")
	            :accessor consultant)
   (select-block-value    :initarg :select-block
			  :initform (error 'careerpacific-invalid-arguments-error
					   :error-number 2001 :error-type "select-block initial argument"
					   :reason ":select-block must be provided.")
			  :accessor select-block)
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

(defmethod initialize-instance :after ((self select-block) &rest args)
  (if (not (and (eql 'user (type-of (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "select-block initial argument"
				     :reason "user has to be a string or a peristed user object"))
  (if (not (member (select-block-value self) *valid-select-block-value* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "select-block initial argument"
				     :reason "select-block-value passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (select-block-value self) *valid-select-block-value*))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "select-block initial argument"
				     :reason "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "select_block"))))


;;immutable slot

;;(defmethod (setf created-date) :before (value (self select-block))
;;  (error 'careerpacific-illegal-operation-error
;;	 :error-number 3001 :error-type "select-block illegal setf"
;;	 :reason "created-date is immutable, it can not be setf"))

;;check the arguments

(defmethod (setf id) :before (value (self select-block))
  (if ( deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "select-block illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block setf argument"
	     :reason "id has to be string. 
                                      it has to be in the format \"user@select-block.com_select-block_1\"")))

(defmethod (setf rev) :before (value (self select-block))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block setf argument"
	     :reason "rev has to be string")))


(defmethod (setf user) :before (value (self select-block))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "select-block illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (eql 'user (type-of value))
		(persisted-object-p value)
		(or (string-equal "candidate" (user-type value))
		    (string-equal "empployer" (user-type value)))))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block setf argument"
	     :reason "user has to be a string or persisted employer or candidate user object")))

(defmethod (setf select-block-value) :before (value (self select-block))
  (if (not (member value *valid-select-block-value* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block initial argument"
	     :reason "select-block-value passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-select-block-value*)))

(defmethod (setf status) :before (value (self select-block))
  (if ( deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "select-block illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self select-block))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self select-block))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "select-block setf argument"
	     :reason (format nil "dirty-marker passed : ~a  is not a valid value. it has to be one                                       of nil or t" value))))

;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self select-block))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self select-block))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self select-block))
  (setf (dirty-marker self) t))

(defmethod (setf select-block-value) :after (value (self select-block))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self select-block))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self select-block))
  (setf (dirty-marker self) t))

;;methods accessing the slots

(defmethod user :before ((self select-block))
  )