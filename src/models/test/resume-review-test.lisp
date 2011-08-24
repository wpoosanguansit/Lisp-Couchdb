(in-package :careerpacific)

(lift:deftestsuite resume-review () ())

(lift:addtest (resume-review)
  (:documentation "test the values of initialize-instance :after")
  (setf rr (make-instance 'resume-review
		 :resume 'watt@yahoo.com_resume_1
		 :status "active"
		 :created-date (get-today-date)
		 :updated-date (get-today-date)
		 :review-content "content"))
  (lift:ensure (id rr)))
