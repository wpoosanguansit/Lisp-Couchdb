;;; Code shared accross the entire CareerPacific Code base.
(in-package #:common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:careerpacific)

(defpackage :careerpacific
  (:use :cl :closer-mop :utils-kt :cells :contextl :fare-matcher :cl-cont :parenscript 
	       :alexandria :babel :bordeaux-threads :cffi :chunga :cl-base64 :cl-cont 
	       :cl-containers :cl-couch :cl-graph :cl-json :cl-ppcre :cl+ssl
	       :defclass-star :defsystem-compatibility :drakma :dynamic-classes :fare-matcher :fare-utils
	       :flexi-streams :iterate :kmrcl :lift :lml2 :logv :lw-compat :metabang-bind 
	       :metatilities :metatilities-base :moptilities :portable-threads :puri :rt 
	       :split-sequence :tinaa :trivial-features :trivial-gray-streams 
	       :trivial-shell :trivial-utf-8 :usocket)
  ;; the variable defined in the ASDF system definition
  (:import-from		     
       #+allegro #:mop
       #+clisp #:clos
       #+lispworks #:clos
       #+(or mcl ccl) #:ccl
       #+cmu #:clos-mop
       #+sbcl #:sb-mop
       #+scl #:clos
       #:class-direct-subclasses
       #:class-direct-superclasses
       #:class-precedence-list
	:careerpacific-asd :*careerpacific-version-string*)
  (:export 	:save :delete :retrieve :update))))

(unless (and (find-package :asdf)
	     (find-symbol (symbol-name 'system-relative-pathname) :asdf)
	     (fboundp (find-symbol
		       (symbol-name 'system-relative-pathname) :asdf)))
  (warn "LIFT uses asdf:system-relative-pathname which your version of ASDF 
doesn't seem to include. LIFT will define these for now but you may want to consider updating to the most recent version of ASDF (see http://www.cliki.net/asdf for details).")
  (intern (symbol-name 'system-source-file) :asdf)
  (intern (symbol-name 'system-source-directory) :asdf)
  (intern (symbol-name 'system-relative-pathname) :asdf)
  (export 'asdf::system-relative-pathname :asdf) 
  (defun asdf::system-source-file (system-name)
    (let ((system (asdf:find-system system-name)))
      (make-pathname 
       :type "asd"
       :name (asdf:component-name system)
       :defaults (asdf:component-relative-pathname system))))

  (defun asdf::system-source-directory (system-name)
    (make-pathname :name nil
		   :type nil
		   :defaults (asdf::system-source-file system-name)))

  (defun asdf::system-relative-pathname (system pathname &key name type)
    (let ((directory (pathname-directory pathname)))
      (when (eq (car directory) :absolute)
	(setf (car directory) :relative))
      (merge-pathnames
       (make-pathname :name (or name (pathname-name pathname))
		      :type (or type (pathname-type pathname))
		      :directory directory)
       (asdf::system-source-directory system)))))
