(in-package :careerpacific)

(export '())

;;this change the status of the resume from complete to active

(contextl:define-layered-method activate :in-layer candidate ((resume resume))
				(setf (status resume) "active")
				(update resume))

;;this change the status of the resume to inactive

(contextl:define-layered-method deactivate :in-layer candidate ((resume resume))
				(setf (status resume) "inactive")
				(update resume))

;;apply for the job-posting creates a job application, the status of the 
;;job application will be applied

(contextl:define-layered-method apply-for-job-posting :in-layer candidate
				((job-posting job-posting) (resume resume) &optional consultant-note)
				(if (and (not (has-been-applied-p job-posting resume))
					 (string-equal "active" (status resume))
					 (string-equal "reviewed" (status resume)))
				    (save (make-instance 'job-application :user *current-user*
							 :job-posting job-posting :resume resume
							 :status "applied"))))
				    

(contextl:define-layered-method post-interview-questions :in-layer candidate 
				((job-application job-application) (interview-question interview-question))
				(if (and (string-equal "information-requested" (status job-application))
					 (string-equal "answered" (status interview-question))
					 (belong-to-p job-application interview-question))
				    (progn
				      (add job-application interview-question)
				      (setf (status job-application) "replied")
				      (update job-application))))

(contextl:define-layered-method post-interview-questions :in-layer candidate 
				((job-application job-application) (interview-questions list))
				(if (and (string-equal "information-requested" (status job-application))
					 (all-elements-are-of-string-value "answered" #'status interview-questions)
					 (belong-to-p job-application interview-questions))
				    (progn
				      (iterate:iterate (iterate:for interview-question iterate:in (interview-questions))
						       (add job-application interview-question))
						       (setf (status job-application) "replied")
						       (update job-application))))