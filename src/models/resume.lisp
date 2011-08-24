(in-package :careerpacific)

(export '())

(defclass resume ()
  ((id                 :initarg :id
		       :initform nil
		       :accessor id)
   (rev                :initarg :rev
		       :initform nil
		       :accessor rev)
   (user               :initarg :user
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":user must be provided.")
		       :accessor user)
   (addresses          :initarg :addresses
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":addresses must be provided.")
		       :accessor addresses)  
   (phone-numbers      :initarg :phone-numbers
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":phone-number must be provided.")
		       :accessor phone-numbers)
   (emails             :initarg :emails
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":emails must be provided.")
		       :accessor emails)
   (educations         :initarg :educations
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":educations must be provided.")
		       :accessor educations)  
   (work-experiences   :initarg :work-experiences
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":work-experiences must be provided.")
		       :accessor work-experiences)
   (skills             :initarg :skills
		       :initform nil
		       :accessor skills)
   (pictures           :initarg :pictures
		       :initform nil
		       :accessor pictures)
   (references         :initarg :references
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":references must be provided.")
		       :accessor references)
   (languages          :initarg :languages
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":language must be provided.")
		       :accessor languages)
   (websites           :initarg :websites
		       :initform nil
		       :accessor websites)
   (resume-reviews     :initarg :resume-reviews
		       :initform nil
		       :accessor resume-reviews)
   (availability       :initarg :availability
		       :initform (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason ":availability must be provided.")
		       :accessor availability)
   (expected-salary    :initarg :expected-salary
		       :initform nil
		       :accessor expected-salary)
   (current-salary     :initarg :current-salary
		       :initform nil
		       :accessor current-salary)
   (consultant-note    :initarg :consultant-note
		       :initform nil
		       :accessor consultant-note)
   (status             :initarg :status
		       :initform "complete"
		       :accessor status)
   (created-date       :initarg :created-date
		       :initform (funcall #'get-today-date)
		       :reader created-date)
   (updated-date       :initarg :updated-date
		       :initform (funcall #'get-today-date)
		       :accessor updated-date)
   (dirty-marker       :initarg :dirty-marker
		       :initform t
		       :accessor dirty-marker)))

;;this is to set the variables after initialization

(defmethod initialize-instance :after ((self resume) &rest args)
  (if (not (and (or (string-equal "candidate" (user-type (user self)))
		    (string-equal "consultant" (user-type (user self))))
		(persisted-object-p (user self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "user passed has to be 
                                     a peristed candidate or consultant user object"))
  (if (not (and (all-elements-are-of-type 'address (addresses self))
		(all-elements-are-persisted-object-p (addresses self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "addresses has to be a list of address objects"))
  (if (not (and (all-elements-are-of-type 'phone-number (phone-numbers self))
		(all-elements-are-persisted-object-p (phone-numbers self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "phone-numbers has to be a list of  phone-number objects"))
  (if (not (and (all-elements-are-of-type 'email (emails self))
		(all-elements-are-persisted-object-p (emails self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "emails has to be a list of string or email objects"))
  (if (not (and (all-elements-are-of-type 'education (educations self))
		(all-elements-are-persisted-object-p (educations self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "educations has to be a list of  education objects"))
  (if (not (and (all-elements-are-of-type 'work-experience (work-experiences self))
		(all-elements-are-persisted-object-p (work-experiences self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "work-experiences has to be a list of  work-experience objects"))
  (if (not (and (all-elements-are-of-type 'skill (skills self))
		(all-elements-are-persisted-object-p (skills self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "skills has to be a list of skill objects"))
  (if (not (and (all-elements-are-of-type 'reference (references self))
		(all-elements-are-persisted-object-p (references self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "references has to be a list of reference objects"))

  (if (not (and (all-elements-are-of-type 'language (languages self))
		(all-elements-are-persisted-object-p (languages self))))
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason "languages has to be a list of language objects"))
  (if (not (stringp (availability self)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume initial argument"
	     :reason "availability has to be string"))
  (if (not (member (status self) *valid-resume-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
				     :error-number 2001 :error-type "resume initial argument"
				     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" (status self) *valid-resume-status*)))
  (if (not (id self))
      (setf (id self) (generate-unique-id (id (user self)) "resume"))))

;;immutable slots

;;(defmethod (setf created-date) :before (value (self resume))
;;  (error 'careerpacific-illegal-operation-error
;;				     :error-number 3001 :error-type "resume illegal setf"
;;				     :reason "created-date is immutable, it can not be setf"))

;;check arguement passed

(defmethod (setf id) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason "id has to be string. 
                                      it has to be in the format \"user@email.com_resume_1\"")))

(defmethod (setf rev) :before (value (self resume))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason "rev has to be string")))


(defmethod (setf user) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (or (string-equal "candidate" (user-type value))
		    (string-equal "consultant" (user-type value)))
		(persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason "user has to be a persisted  candidate or consultant user object")))

(defmethod (setf addresses) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'address value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "addresses passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of address objects" value))))

(defmethod (setf phone-numbers) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'phone-number value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "phone-numbers passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of phone-number objects" value))))

(defmethod (setf emails) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'email value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "emails passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of email objects" value))))

(defmethod (setf educations) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'education value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "educations passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of education objects" value))))

(defmethod (setf work-experiences) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'work-experience value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "work-experiences passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of work-experience objects" value))))

(defmethod (setf skills) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'skill value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "skills passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of skill objects" value))))

(defmethod (setf pictures) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'picture value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "pictures passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of picture objects" value))))

(defmethod (setf references) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'reference value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "references passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of reference objects" value))))

(defmethod (setf languages) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'website value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "websites passed :  ~{~a~^, ~} are not valid, 
                                      it has to be a list of website objects" value))))

(defmethod (setf resume-reviews) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (and (all-elements-are-of-type 'resume-review value)
		(all-elements-are-persisted-object-p value)))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "resume-reviews passed :  ~{~a~^, ~} is not valid, 
                                      it has to be a list of resume-review objects" value))))

(defmethod (setf availability) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason "availability has to be string")))

(defmethod (setf expected-salary) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason "expected-salary has to be string")))

(defmethod (setf consultant-note) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (stringp value))
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason "consultant-note has to be string")))

(defmethod (setf status) :before (value (self resume))
  (if (deleted-persisted-object-p self)
      (error 'careerpacific-illegal-operation-error
	     :error-number 3002 :error-type "resume illegal setf"
	     :reason "object is deleted, it can not be setf"))
  (if (not (member value *valid-object-status* :test #'string-equal)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "status passed is not valid string : ~a. it has to be one 
                                      of these values : ~{~a~^, ~}" value *valid-object-status*))))

(defmethod (setf updated-date) :before (value (self resume))
  (if (not (stringp value)) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "updated-date passed : ~a  is not a valid value. 
              it has to be a date" value))))

(defmethod (setf dirty-marker) :before (value (self resume))
  (if (not (member value '(nil t))) 
      (error 'careerpacific-invalid-arguments-error
	     :error-number 2001 :error-type "resume setf argument"
	     :reason (format nil "dirty-marker passed is not a valid value : ~a. it has to be one 
                                      of nil or t" value))))


;;this method check the values being set and set the dirty-marker to t when setf is called

(defmethod (setf id) :after (value (self resume))
  (setf (dirty-marker self) t))


(defmethod (setf rev) :after (value (self resume))
  (setf (dirty-marker self) t))


(defmethod (setf user) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf addresses) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf phone-numbers) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf emails) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf educations) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf work-experiences) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf skills) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf pictures) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf references) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf languages) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf websites) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf resume-reviews) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf availability) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf expected-salary) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf current-salary) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf consultant-note) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf status) :after (value (self resume))
  (setf (dirty-marker self) t))

(defmethod (setf updated-date) :after (value (self resume))
  (setf (dirty-marker self) t))

;;method accessing the slot

(defmethod user :before ((self resume))
  )

;;convienient method to add sub objects that are in the list

(defmethod add ((self resume) (value address))
  (if (and (not (member value (addresses self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'address (type-of value)))
      (setf (addresses self) (cons value (addresses self)))))

(defmethod add ((self resume) (value phone-number))
  (if (and (not (member value (phone-numbers self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'phone-number (type-of value)))
      (setf (phone-numbers self) (cons value (phone-numbers self)))))

(defmethod add ((self resume) (value email))
  (if (and (not (member value (emails self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'email (type-of email)))
      (setf (emails self) (cons value (emails self)))))

(defmethod add ((value education) (self resume))
  (if (and (not (member value (educations self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'resume (type-of value)))
      (setf (educations self) (cons value (educations self)))))

(defmethod add ((self resume) (value work-experience))
  (if (and (not (member value (work-experiences self) :test #'same-persisted-object-p))
	   (persisted-object-p value))
      (setf (work-experiences self) (cons value (work-experiences self)))))

(defmethod add ((self resume) (value skill))
  (if (and (not (member value (skills self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'skill (type-of value)))
      (setf (skills self) (cons value (skills self)))))

(defmethod add ((self resume) (value picture))
  (if (and (not (member value (pictures self) :test #'same-persisted-object-p))
	   (persisted-object-p value))
      (setf (pictures self) (cons value (pictures self)))))

(defmethod add ((self resume) (value reference))
  (if (and (not (member value (referenes self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'reference (type-of value)))
      (setf (references self) (cons value (references self)))))

(defmethod add ((self resume) (value language))
  (if (and (not (member value (languages self) :test #'same-persisted-object-p))
	   (persisted-object-p value))
      (setf (languages self) (cons value (languages self)))))

(defmethod add ((self resume) (value website))
  (if (and (not (member value (websites self) :test #'same-persisted-object-p))
	   (persisted-object-p value)
	   (eql 'website (type-of value)))
      (setf (websites self) (cons value (websites self)))))

(defmethod add ((self resume) (value resume-review))
  (if (and (not (member value (resume-reviews self) :test #'same-persisted-object-p))
	   (persisted-object-p value))
      (setf (resume-reviews self) (cons value (resume-reviews self)))))

;;erase methods

(defmethod erase ((self resume) (value address))
  (if (and (persisted-object-p value)
	   (eql 'address (type-of value)))
      (setf (addresses self) (remove value (addresses self) :test #'same-persisted-object-p))))

(defmethod erase ((self resume) (value phone-number))
  (if (and (persisted-object-p value) 
	   (eql 'phone-number (type-of value)))
      (setf (phone-numbers self) (remove value (phone-numbers self) :test #'same-persisted-object-p))))

(defmethod erase ((self resume) (value email))
  (if (and (persisted-object-p value)
	   (eql 'email (type-of value)))
      (setf (educations self) (remove value (educations self) :test #'same-persisted-object-p))))
    
(defmethod erase ((self resume) (value work-experience))
  (if (and (persisted-object-p value)
	   (eql 'work-experience (type-of value)))
      (setf (work-experiences self) (remove value (work-experiences self) :test #'same-persisted-object-p))))

(defmethod erase ((self resume) (value skill))
  (if (and (persisted-object-p value) 
	   (eql 'skill (type-of value)))
      (setf (skills self) (remove value (skill self) :test #'same-persisted-object-p))))

(defmethod erase ((self resume) (value picture))
  (if (and (persisted-object-p value) 
	   (eql 'picture (type-of value)))
      (setf (pictures self) (remove value (pictures self) :test #'same-persisted-object-p))))
   
(defmethod erase ((self resume) (value reference))
  (if (and (persisted-object-p value) 
	   (eql 'reference (type-of value)))
      (setf (references self) (remove value (references self) :test #'same-persisted-object-p))))
    

(defmethod erase ((self resume) (value language))
  (if (and (persisted-object-p value) 
	   (eql 'language (type-of value)))
      (setf (languages self) (remove value (languages self) :test #'same-persisted-object-p))))

(defmethod erase ((self resume) (value website))
  (if (and (persisted-object-p value) 
	   (eql 'website (type-of value)))
      (setf (websites self) (remove value (websites self) :test #'same-persisted-object-p))))

(defmethod erase ((self resume) (value resume-review))
  (if (and (persisted-object-p value) 
	   (eql 'resume-review (type-of value)))
      (setf (resume-reviews self) (remove value (resume-reviews self) :test #'same-persisted-object-p))))