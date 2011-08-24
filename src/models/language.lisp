(in-package :careerpacific)

(export '())

(defclass language ()
  ((id              :initarg :id
		    :initform nil
		    :accessor id)
   (rev             :initarg :rev
	            :initform nil
	            :accessor rev)
   (user            :initarg :user
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason ":user must be provided.")
		    :accessor user)
   (language-name   :initarg :language-name
	            :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason ":language-name must be provided.")
		    :accessor language-name)
   (writing-level   :initarg :writing-level
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason ":writing-level must be provided.")
		    :accessor writing-level)
   (spoken-level    :initarg :spoken-level
		    :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason ":spoken-level must be provided.")
		    :accessor spoken-level)
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
;;the unique random no of language
;;(if (not (stringp (resume self)))
;;      (error 'careerpacific-invalid-arguments-error
;;				     :error-number 2001 :error-type "language initial argument"
;;				     :reason "resume has to be a string. 
;;                                      it has to be in the format \"user@email.com_resume_1\""))

(defmethod initialize-instance :after ((self language) &rest args)
  (if (not (and (eql 'user (type-of (user self)))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason "user has to be an id string or a persisted user object"))
  (if (not (stringp (language-name self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language initial argument"
	     :reason "language-name has to be a string"))
  (if (not (member (status self) *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-object-status*)))
  (if (not (member (spoken-level self) *valid-language-skill-level* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason (format nil "spoken-level passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (spoken-level self) *valid-language-skill-level*)))
  (if (not (member (writing-level self) *valid-language-skill-level* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason (format nil "writing-level passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (writing-level self) *valid-language-skill-level*)))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "language"))))

;;immutable slots

;;(defmethod (setf created-date) :before (value (self language))
;;  (error 'careerpacific-illegal-operation-error
;;	 :error-number 3001 :error-type "language illegal setf"
;;	 :reason "created-date is immutable, it can not be setf"))

;;check arguement passed

(defmethod (setf id) :before (value (self language))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "language illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason "id has to be a string. 
                                      it has to be in the format \"user@email.com_language_1\"")))

(defmethod (setf rev) :before (value (self language))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason "rev has to be a string")))


(defmethod (setf user) :before (value (self language))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "language illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (eql 'user (type-of value))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason "user has to be an id string or a persisted user object. 
                      in case of string, it has to be in the format \"user@email.com\"")))


(defmethod (setf language-name) :before (value (self language))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "language illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason (format nil "language-name passed : ~a is not valid, 
                                      it has to be a string" value))))

(defmethod (setf writing-level) :before (value (self language))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "language illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-language-skill-level* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "language initial argument"
				     :reason (format nil "writing-level passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (writing-level self) *valid-language-skill-level*))))

(defmethod (setf spoken-level) :before (value (self language))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "language illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-language-skill-level* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "spoken-level initial argument"
				     :reason (format nil "spoken-level passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (spoken-level self) *valid-language-skill-level*))))


(defmethod (setf status) :before (value (self language))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "language illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self language))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self language))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "language setf argument"
	     :reason (format nil "dirty-marker passed is not a valid value : ~a. it has to be one 
                                      of nil or t" value))))


;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self language))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self language))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self language))
  (setf (dirty-marker self) t))


(defmethod (setf language-name) :after (value (self language))
  (setf (dirty-marker self) t))

(defmethod (setf writing-level) :after (value (self language))
  (setf (dirty-marker self) t))

(defmethod (setf spoken-level) :after (value (self language))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self language))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self language))
  (setf (dirty-marker self) t))

;;methods accessing the slots

(defmethod user :before ((self language))
  )
