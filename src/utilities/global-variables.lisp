;;variables that we would want them to survive reload.k

;;they are the variables used in place of enums.

(defvar *address-type* '("RESIDENTIAL" "COMMERCIAL"))

;;have to use string instead of symbol because it fails when symb the string
;;from the persistent layer and compare to this list with member function

(defvar *valid-job-application-status* '("ACTIVE" "REJECTED" "APPLIED" "SUBMITTED" "VIEWED" 
					 "INFORMATION-REQUESTED" "FORWARDED" "REPLIED" "SELECTED" 
					 "INTERVIEW-QUESTION-REVIEWED" "ACKNOWLEDGED" 
					 "PERSONAL-INTERVIEW-REQUESTED" "SCHEDULED-FOR-PERSONAL-INTERVIEW" 
					 "HIRED" "EXPIRED" "DELETED"))

(defvar *valid-resume-status* '("COMPLETE" "ACTIVE" "REVIEWED" "INACTIVE" "HIRED" "EXPIRED" "DELETED"))

(defvar *valid-object-status* '("ACTIVE" "INACTIVE" "EXPIRED" "DELETED"))

(defvar *valid-phone-number-type* '("RESIDENTIAL" "BUSINESS"))

(defvar *valid-picture-type* '("DEFAULT" "SECONDARY"))

(defvar *valid-user-type* '("CANDIDATE" "EMPLOYER" "CONSULTANT" "STAFF" "ADMINISTRATOR"))

(defvar *valid-education-level* '("HIGHSCHOOL" "BACHELOR" "GRADUATE" "POSTGRADUATE" "OTHER"))

(defvar *valid-language-skill-level*  '("FLUENT" "CONVERSANT" "PRELIMINARY" "LIMITED"))

(defvar *valid-skill-level*  '("EXCELLENT" "GOOD" "MODERATE" "BEGINNER"))

(defvar *valid-job-application-status*  '("APPLIED" "RECEIVED" "REVIEWED" "SUBMITTED"))

(defvar *valid-email-type* '("BUSINESS" "HOME" "MOBILE"))

(defvar *valid-website-type* '("BUSINESS" "PERSONAL"))

(defvar *valid-employer-plan-type* '("TRIAL" "STANDARD" "GOLD" "PLATINUM"))

(defvar *valid-interview-question-status* '("ACTIVE" "ANSWERED" "EXPIRED" "TEMPLATE"))

(defvar *valid-select-block-value* '("NORMAL" "SELECTED" "BLOCKED"))


;;this is used to check if the argument passed to retrieve-current-id is valid db names.

(defconstant *valid-db-name* '(address company-profile education email interview-question job-application 
			       job-posting language phone-number picture reference resume resume-review 
			       select-block skill user web-site work-experience))

;;this is to setup the context for isaac to generate ramdom number.

(defvar *isaac-context* (isaac::init-kernel-seed))

;;this variable has to be set for the business layer functions and business rules to work

(defparameter *current-user* "set to the current user who log in it has to be set at login time")

(defconstant *maximum-number-of-resume-reviews-allowed* 5)

(defconstant *plural-symbol-associative-list* 
  (pairlis  '(address  company-profile) '(addresses  company-profiles) 
	    '((education . educations) (interview-question . interview-questions)
	      (job-application . job-applications) (job-posting . job-postings)
	      (language . languages) (phone-number . phone-numbers)
	      (picture . pictures) (references . references) (resume . resumes)
	      (resume-review . resume-reviews) (select-block . select-blocks)
	      (skill . skills) (user . users) (web-site . web-sites) 
	      (work-experience . work-experiences))))

