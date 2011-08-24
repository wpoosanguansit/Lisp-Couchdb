(in-package :careerpacific)

(export '())

;;this change the status of the resume from active to reviewed.  it is then ready for submission.

(contextl:define-layered-method add-resume-review :in-layer consultant ((resume resume) (resume-review resume-review))
				(if (reviewable-p resume resume-review)
				    (progn
				      (add resume resume-review)
				      (setf (status resume) "reviewed")
				      (update resume))))

;;this change the status of the resume to inactive

(contextl:define-layered-method deactivate :in-layer consultant ((resume resume))
				(setf (status resume) "inactive")
				(update resume))

;;this apply the resume to a job posting, it creates the job application and set the status
;;to submitted

(contextl:define-layered-method apply-for-job-posting :in-layer consultant 
				((job-posting job-posting) (resume resume) &optional consultant-note)
				(if (and (not (has-been-applied-p job-posting resume))
					 (string-equal "reviewed" (status resume))
					 (has-been-reviewed-by-p *current-user* resume))
				    (save (make-instance 'job-application :user *current-user*
							 :job-posting job-posting :resume resume
							 :consultant-note consultant-note :status "submitted"))))

(contextl:define-layered-method submit-resume-for-job-application :in-layer consultant 
				((job-application job-application) &optional consultant-note)
				(if (and (string-equal "applied" (status job-application))
					 (has-been-reviewed-by-p *current-user* (resume job-application)))
				    (progn
				      (setf (user job-application) *current-user*)
				      (setf (consultant-note job-application) consultant-note) 
				      (setf (status job-application) "submitted")
				      (update job-application))))

(contextl:define-layered-method forward-interview-questions :in-layer consultant ((job-application job-application))
				(if (and (string-equal "information-requested" (status job-application))
					 (has-been-reviewed-by-p *current-user* (resume job-application)))
				    (progn 
				      (setf (status job-application) "forwarded")
				      (update job-application))))

(contextl:define-layered-method review-interview-questions :in-layer consultant ((job-application job-application))
				(if (and (string-equal "replied" (status job-application))
					 (has-been-reviewed-by-p *current-user* (resume job-application)))
				    (progn 
				      (setf (status job-application) "interview-question-reviewed")
				      (update job-application))))

(contextl:define-layered-method schedule-personal-interview :in-layer consultant ((job-application job-application))
				(if (and (or (string-equal "personal-interview-requested" (status job-application))
					     (string-equal "selected" (status job-application)))
					 (has-been-reviewed-by-p *current-user* (resume job-application)))
				    (progn 
				      (setf (status job-application) "scheduled-for-personal-interview")
				      (setf (user job-application) *current-user*)
				      (update job-application))))

(contextl:define-layered-method hire :in-layer consultant ((job-application job-application))
				(if (and (or (string-equal "selected" (status job-application))
					     (string-equal "acknowledged" (status job-application)))
					 (has-been-reviewed-by-p *current-user* (resume job-application)))
				    (progn
				      (setf (status job-application) "hired")
				      (setf resume (retrieve 'resume-by-id (id (resume job-application))))
				      (setf (status resume) "hired")
				      (update job-application)
				      (update resume))))



