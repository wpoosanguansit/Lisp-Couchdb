(in-package :careerpacific)

(cl-couch::defdoc user
	   ((:firstname :validator #'stringp)
	    (:middlename )
	    (:lastname :validator #'stringp)
	    (:username :validator #'emailp)
	    (:password :validator #'stringp)
	    (:status :validator #'stringp)
	    (:_id :initform (lambda (doc) (url-encode (cl-couch::@ doc :username)))))
	    (:default-db 'user))

(cl-couch::defdoc candidate
	   ((:firstname :validator #'stringp)
	    (:middlename )
	    (:lastname :validator #'stringp)
	    (:username :validator #'emailp)
	    (:password :validator #'stringp)
	    (:status :validator #'stringp)
	    (:resume )
	    (:_id :initform (lambda (doc) (url-encode (cl::couch@ doc :username)))))
	    (:default-db 'user))

(defun encode-to-json (resume resume)
  (resume))

(defmethod save (candidate candidate)
  (let result (cl-couch::make-doc-and-save 'user
	       :firstname (firstname candidate)
	       :middlename (middlename candidate)
	       :lastname (lastname candidate)
	       :username (username candidate)
	       :password (password candidate)
	       :type "candidate"
	       :resumes (encode-to-json((resumes candidate)))
       if result
       result
       (error result)))

(defmethod update (candidate candidate)
  ())

(defmethod delete (candidate candidate)
  ())

(couch-request* :post *couchdb-server* (list 'user "81f1294ba8fb141dd95ac8fe4fadc067") (list 'user) +json-content-type+ (plist-alist* :name "watt1" :lastname "poosanguansit1"))

(couch-request* :put *couchdb-server* (list 'user "81f1294ba8fb141dd95ac8fe4fadc067") () +json-content-type+ (plist-alist* :_rev "2732725491" :name "watt1" :lastname "poosanguansit1"))

(cl-couch::couch-request* :put cl-couch::*couchdb-server* (list 'user "81f1294ba8fb141dd95ac8fe4fadc067") () cl-couch::+json-content-type+ (cl-couch::plist-alist* :_rev "2732725491" :name "watt1" :lastname "poosanguansit1"))

JSON> (decode-json-to-string (encode-json-to-string "{\"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }}"))
; Evaluation aborted.
JSON> (decode-json-from-string (encode-json-to-string "{\"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }}"))
"{\"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }}"
JSON> (encode-json-to-string "{\"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }}")
"\"{\\\"JSON Test Pattern pass3\\\": {\\n        \\\"The outermost value\\\": \\\"must be an object or array.\\\"\\n    }}\""
JSON> (decode-json-from-string "{\"hello\":100,\"hi\":5}")
((:HELLO . 100) (:HI . 5))
JSON> (decode-json-from-string "{\"noon\" : {\"hello\":100,\"hi\":5}}")
((:NOON (:HELLO . 100) (:HI . 5)))
JSON> (encode-json-to-string ((:NOON (:HELLO . 100) (:HI . 5))))
; in: LAMBDA NIL
;     ((:NOON (:HELLO . 100) (:HI . 5)))
; 
; caught ERROR:
;   illegal function call
; 
; compilation unit finished
;   caught 1 ERROR condition
; Evaluation aborted.
JSON> (encode-json-to-string '((:NOON (:HELLO . 100) (:HI . 5))))
"{\"noon\":{\"hello\":100,\"hi\":5}}"
JSON> 