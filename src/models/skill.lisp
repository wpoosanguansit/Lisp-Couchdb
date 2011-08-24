(in-package :careerpacific)

(export '())

(defclass skill ()       
  ((id              :initarg :id
	            :initform nil
	            :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skill initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (skill-name      :initarg :skill-name
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skill initial argument"
				     :reason ":skill-name must be provided.")
		    :accessor skill-name)
   (description     :initarg :description
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skil initial argument"
				     :reason ":description must be provided.")
		    :accessor description)
   (skill-level           :initarg :skill-level
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skil initial argument"
				     :reason "skill-level must be provided")
	            :accessor skill-level)
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
;;the unique random no of skill
;;(if (not (stringp (resume self)))
;;      (error 'careerpacific-invalid-arguments-error
;;				     :error-number 2001 :error-type "skill initial argument"
;;				     :reason "resume has to be a string. 
;;                                      it has to be in the format \"user@email.com\""))

(defmethod initialize-instance :after ((self skill) &rest args)
  (if (not (and (eql 'user (type-of (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skill initial argument"
				     :reason "user has to be a persisted user object"))
  (if (not (stringp (skill-name self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill initial argument"
	     :reason "skill-name passed is not valid, it has to be a string"))
  (if (not (stringp (description self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill initial argument"
	     :reason "description passed is not valid, it has to be a string"))
  (if (not (member (skill-level self) *valid-skill-level* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skill initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (skill-level self) *valid-skill-level*)))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "skill initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "skill"))))

;;immutable slots

;;(defmethod (setf created-date) :before (value (self skill))
;;  (error 'careerpacific-illegal-operation-error
;;				     :error-number 3001 :error-type "skill illegal setf"
;;				     :reason "created-date is immutable, it can not be setf"))

;;check arguement passed

(defmethod (setf id) :before (value (self skill))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason "id has to be a string. 
                                      it has to be in the format \"user@email.com_skill_1\"")))

(defmethod (setf rev) :before (value (self skill))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason "rev has to be a string")))


(defmethod (setf user) :before (value (self skill))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (eql 'user (type-of value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason "user has to be a persisted user object")))

(defmethod (setf skill-name) :before (value (self skill))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason (format nil "skill-name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf skill-level) :before (value (self skill))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason (format nil "skill-level passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf status) :before (value (self skill))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "skill illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self skill))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self skill))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "skill setf argument"
	     :reason (format nil "dirty-marker passed is not a valid value : ~a. it has to be one 
                                      of nil or t" value))))


;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self skill))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self skill))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self skill))
  (setf (dirty-marker self) t))


(defmethod (setf skill-name) :after (value (self skill))
  (setf (dirty-marker self) t))

(defmethod (setf skill-level) :after (value (self skill))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self skill))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self skill))
  (setf (dirty-marker self) t))

;;method accessing the slot

(defmethod user :before ((self skill))
  )

