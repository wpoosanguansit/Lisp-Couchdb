(in-package :careerpacific)

(export '())

;;this is to retrieve the current id give the name of the database name like resume-review
;;has to use macro since we want the base to evaluate to number like 32

(defmacro retrieve-unique-number (base)
  (if (or (not (numberp `,base)) (>= 0 `,base))
      (error "base number passed to retrieve-unique-number has to be positive number.")
      (isaac::rand-bits *isaac-context* `,base)))


;;parent is expected to be in the user id + the parent object name and number - 
;;resume id + resume-review id - user@id.com_resume_isaac10_resume_review_isaac10
;;isaac is the ramdom number generator, 10 (which is isaac based 32 bits) 
;;is the lenght of the unique random number.

(defun generate-unique-id (parent self)
  (if (not *isaac-context*) 
      (setf *isaac-context* (isaac::init-kernel-seed)))
  (if parent
      (mkstr parent '_ (cl-couch::couch-urlize self)'_ (retrieve-unique-number 32))
      (mkstr (cl-couch::couch-urlize self)'_ (retrieve-unique-number 32))))

;;check the id that conforms to the format resume id + resume-review + (unique) index

(defun check-id-format (symbol)
  ())

;;this is taken from the cl-couch.  it is for the used in view retrieval
(defun prepare-keys (keys)
  "Prepare a plist to be send to server as query parameters \(put
strings in quotes and escape them)."
  (mapcar (lambda (keyword) (if (keywordp keyword) keyword (cl-couchdb-client:json keyword))) keys))

;;this will unwrap the values that got back from the view requests.
;;from this
;;((:TOTAL-ROWS . 10) (:OFFSET . 0)
;; (:ROWS
;;  ((:ID . "6cce723244462c94848eb2419b1db817") (:KEY . "watt@yahoo.com")
;;   (:VALUE (:_ID . "6cce723244462c94848eb2419b1db817") (:_REV . "3657679513")
;;    (:STATUS . "active") (:UPDATED-DATE . "12/10/20080")
;;    (:CREATED-DATE . "12/10/2008") (:REVIEW-CONTENT . "content")
;;    (:RESUME . "watt@yahoo.com") (:TYPE . "DISCO-PARSER::RESUME-REVIEW-DOC")))
;;  ((:ID . "watt17@yahoo.com") (:KEY . "watt@yahoo.com")
;;   (:VALUE (:_ID . "watt17@yahoo.com") (:_REV . "1479263711")
;;    (:STATUS . "active") (:UPDATED-DATE . "11/9/2008")
;;    (:CREATED-DATE . "11/9/2008") (:REVIEW-CONTENT . "content")
;;    (:RESUME . "watt@yahoo.com") (:TYPE . "DISCO-PARSER::RESUME-REVIEW-DOC")))))
;;to
;;(((:_ID . "6cce723244462c94848eb2419b1db817") (:_REV . "3657679513")
;;  (:STATUS . "active") (:UPDATED-DATE . "12/10/20080")
;;  (:CREATED-DATE . "12/10/2008") (:REVIEW-CONTENT . "content")
;;  (:RESUME . "watt@yahoo.com") (:TYPE . "DISCO-PARSER::RESUME-REVIEW-DOC"))
;; ((:_ID . "watt17@yahoo.com") (:_REV . "1479263711") (:STATUS . "active")
;;  (:UPDATED-DATE . "11/9/2008") (:CREATED-DATE . "11/9/2008")
;;  (:REVIEW-CONTENT . "content") (:RESUME . "watt@yahoo.com")
;;  (:TYPE . "DISCO-PARSER::RESUME-REVIEW-DOC")))

(defun unwrap-query-wrapper (result)
  "Take the result of the query and return as value of the rows
in the result returned from couchdb."
 (map 'list 
      (lambda (document) (cl-couchdb-client:@ document :value)) 
      (values (cl-couchdb-client:@ result :rows))))


;;this is the replica of the couch-request but with the prepare-keys inserted
;;this allows the keys not to be escaped for ".
;;(couch-request :get (resume-review _view resume-review resume-review-by-resume-id :key "watt@yahoo.com" :startkey "watt@yahoo.com"))

;;(defmacro couch-request (req-spec (&rest path-and-keys) &optional content)
;;  "`req-spec' is either just the method, or matches the
;;  lambda-list (method &key (content-type \"application/json\") (server
;;  *couchdb-server*)). Path is a list of either strings or symbols,
;;  which at some point are key value pairs. These keys will map to GET
;;  parameters.

;;  For example, path equal to '(foo _all_docs :count 2 :startkey
;;  \"quux\") will map to \"/foo/_all_docs?count=2&startkey=%23quux%23\"
;;
;;
;;  The following key arguments are recognized: `key', `startkey',
;;  `endkey', `count', `skip', `startkey_docid', `update', `descending',
;;  `group', `group_level'."
;;  (multiple-value-bind (path keys)
;;      (iterate:iter (iterate:for el iterate:in path-and-keys)
;;	     (iterate:with keyword-seen = nil)
;;	     (when (keywordp el) (setf keyword-seen t))
;;	     (if keyword-seen (iterate:collect el into keys) (iterate:collect el into path))
;;	     (iterate:finally (return (values path keys))))
;;   (setf path (process-list path 'path))
;;    (setf keys (process-list  keys 'keys))
;;    (setf content (process-list content 'content))
;;    (destructuring-bind (method &key (content-type cl-couchdb-client:+json-content-type+) (server cl-couchdb-client:*couchdb-server*))
;;	(cl-couchdb-client:ensure-list req-spec)
;;      `(progn 
;;	(ensure-db-connection)
;;	(handler-case
;;	    (cl-couchdb-client:couch-request* ,method ,server ',@path ',@keys ,content-type ',@content)
;;	  (cl-couchdb-client:couchdb-conflict (condition)
;;	    ())
;;	  (cl-couchdb-client:couchdb-not-found (condition)
;;	    ())
;;	  (cl-couchdb-client:couchdb-server-error (condition)
;;	    ())
;;	  (t
;;	      ()))))))
;;

;;util for converting the values of keys and content by eval before passing them on to 
;;the cl-couchdb-client:couchdb-request* in the macro db-request.  type must be either 'keys or 'content or 'path

;;(defun process-list (keys-or-content-or-path-list type)
;;  (let* ((keys (remove-if-not #'keywordp keys-or-content-or-path-list))
;;	  (keys-values (mapcar #'(lambda (key-value) (values (eval (cadr key-value)))) 
;;			       (group keys-or-content-or-path-list 2))))
;;	   (case type
;;	     ('path (list (mapcar #'(lambda (element) (if (not (listp element))
;;							  element
;;							  (values (eval element)))) keys-or-content-or-path-list)))
;;	     ('keys (list (flatten  (mapcar #'list  keys keys-values))))
;;	     ('content (list (cl-couchdb-client:plist-alist (flatten  
;;							     (mapcar #'list  keys keys-values)))))
;;	     (t (error "process-list: must be of type 'keys or 'content or 'path")))))
;;

;;this is to ensure that the cl-couchdb-client:*couchdb-server* is not nil.

(defun ensure-db-connection ()
  (if (not cl-couchdb-client:*couchdb-server*)
      (handler-case (cl-couchdb-client:open-server)
	(error 'careerpacific-db-error :error-number 1001 :error-type "db errror." 
                                       :reason "the database connection can not be established.")))
  'cl-couchdb-client:*couchdb-server*)

(defmacro with-careerpacific-db-handler (&rest body)
	   `(progn 
	     (handler-case
		 ,@body
	       (cl-couchdb-client:couchdb-conflict (condition)
		 (error 'careerpacific-db-error
				     :error-number 1005 :error-type "db"
				     :reason (format nil "with-careerpacific-db-handler: 
                                              there is a couchdb conflict - ~a" condition))
	       (cl-couchdb-client:couchdb-not-found (condition)
		 (error 'careerpacific-db-error
				     :error-number 1004 :error-type "db"
				     :reason (format nil "with-careerpacific-db-handler: 
                                              couchdb not found - ~a." condition))
	       (cl-couchdb-client:couchdb-server-error (condition)
		 (error 'careerpacific-db-error
				     :error-number 1003 :error-type "db"
				     :reason (format nil "with-careerpacific-db-handler: 
                                              there is a server error - ~a." condition))
	       (careerpacific-db-error (condition)
		 (error 'careerpacific-db-error 
			             :error-number 1002 :error-type "db"
				     :reason (format nil "with-careerpacific-db-handler: 
                                              there is a problem establishing db connection. - ~a" condition))
	       (error ()
		 (error 'careerpacific-db-error
				     :error-number 1001 :error-type "db"
				     :reason "with-careerpacific-db-handler: there is unknown error.")))))))))

;;this is to wrap up the call to cl-couchdb-client:couch-request* in careerpacific exceptional handling code.
;;it also changes the position of optional argument content and content type, so that content type can be left out
;; during call without specifying () when it is nil and content is not.

(defun db-request (method server path &optional keys content (content-type cl-couchdb-client:+json-content-type+))
  (ensure-db-connection)
  (if (null server)
      (setf server cl-couchdb-client:*couchdb-server*))
  (with-careerpacific-db-handler
      (cl-couchdb-client:couch-request* method server path keys content-type content)))
