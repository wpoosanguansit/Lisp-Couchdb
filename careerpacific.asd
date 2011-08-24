;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:careerpacific-asd
  (:use :cl :asdf)
  (:nicknames :cp)
  (:export #:test #:test-op #:doc #:doc-op #:make-app #:make-app-op
	   #:save #:retrieve #:delete #:update))

(defvar *careerpacific-version-string* "0.0.1"
  "CareerPacific's version number as a string.")

(in-package :careerpacific-asd)

(defsystem careerpacific
  :name "careerpacific"
  :version "0.0.1"
  :maintainer "Watt P."
  :author "Watt P."
  :licence "Copyrited"
  :description "Online Career System."
  :depends-on (:closer-mop :utils-kt :cells :contextl :fare-matcher :cl-cont :parenscript 
	       :alexandria :babel :bordeaux-threads :cffi :chunga :cl-base64 :cl-cont 
	       :cl-containers :cl-couch :cl-graph :cl-json :cl-ppcre :cl+ssl
	       :defclass-star :defsystem-compatibility :drakma :dynamic-classes :fare-matcher :fare-utils
	       :flexi-streams :iterate :kmrcl :lift :lml2 :logv :lw-compat :metabang-bind 
	       :metatilities :metatilities-base :moptilities :portable-threads :puri :rt 
	       :split-sequence :tinaa :trivial-features :trivial-gray-streams 
	       :trivial-shell :trivial-utf-8 :usocket)
  :components (
	       (:module "src"
			:pathname "src/"	
			:components ((:file "packages")
				     (:module "models"
				      :pathname "models/"
				      :depends-on 	("utils")
				      :components 	((:file "user")
							 (:file "administrator")
							 (:file "staff")
							 (:file "candidte")
							 (:file "consultant")
							 (:file "candidate")
							 (:file "resume")
							 (:file "jobapplication")
							 (:file "jobposting")
							 (:file "resumereview")
							 (:file "selectblock")
							 (:file "resume-review"
								:depends-on ("defgeneric"))
							 (:file "defgeneric")))
				     (:module "utils"
					      :pathname "utils/"
					      :components ((:file "date-utils")
							   (:file "persistence-layer")
							   (:file "rule-parser")
							   (:file "validators")
							   (:file "global-variables")
							   (:file "lol")
							   (:file "isaac")))
				     (:module "dao"
					      :pathname "dao/"
					      :depends-on ("utils" "models")
					      :components ((:file "candidate-dao")
							   (:file "resume-review-dao")
							   (:file "defgeneric")))))))
