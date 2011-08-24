(in-package :careerpacific)

(export '())

(contextl:define-layered-function activate (object))

(contextl:define-layered-function deactivate (object))

(contextl:define-layered-function add-resume-review (resume resume-review))

(contextl:define-layered-function apply-for-job-posting (job-posting resume &optional consultant-note))

(contextl:define-layered-function select-for-job-posting (job-posting resume))

(contextl:define-layered-function submit-resume-for-job-application (job-application &optional consultant-note))

(contextl:define-layered-function view-job-application (job-application))

(contextl:define-layered-function post-interview-questions (job-posting interview-question))

(contextl:define-layered-function forward-interview-questions (job-application))

(contextl:define-layered-function review-interview-questions (job-application))

(contextl:define-layered-function request-personal-interview (job-application))

(contextl:define-layered-function schedule-personal-interview (job-application))

(contextl:define-layered-function hire (job-application))
