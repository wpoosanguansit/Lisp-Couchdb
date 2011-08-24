(in-package :careerpacific)

(export '())

(defun reviewable-p (resume resume-review) 
  (ignore-errors (if (and (<  (length (resume-reviews resume)) 
			      *maximum-number-of-resume-reviews-allowed*)
			  (not (has-been-reviewed-by-p *current-user* resume)))
		     (return t))))

(defun has-been-reviewed-by-p (consultant resume)
  (ignore-errors (some 
		  #'(lambda (resume-review) 
		      (same-persisted-object-p consultant (user resume-review)))
		  (resume-reviews resume))))     

;;(if (not (and (eql 'resume (type-of resume)) 
;;		  (eql 'resume-review (type-of resume-review))
;;		  (persisted-object-p resume)
;;		  (persisted-object-p resume-review)))
;;	(error 'careerpacific-invalid-arguments-error
;;	       :error-number 2001 :error-type "reviewable-p initial arguments"
;;	       :reason "resume and resume-review  have to be a persisted resume 
;;                                      and resume-review objects"))


;;checking if the job application has been applied

(defun has-been-applied-p (job-posting resume)
 (ignore-errors 
   (if (not (retrieve 'job-application-by-job-posting-and-resume-id 
		 (id job-posting) (id resume)))
       (return t))))