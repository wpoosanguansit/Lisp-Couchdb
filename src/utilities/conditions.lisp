(in-package :careerpacific)

;;this will be the classes for signaling error and conditions for handling
;;events in the system.

(export '(careerpacific-integration-layer-condition careerpacific-business-layer-condition careerpacific-interface-layer-condition))

(define-condition careerpacific-condition ()
  ((error-number :initarg :error-number :initform 0 :reader error-number)
   (error-type :initarg :error-type :initform "unknown" :reader error-type)
   (reason :initarg :reason :initform "unknown" :reader reason))
   (:report (lambda (condition stream)
	     (format stream "careerpacific returned an error: ~a (~a). Reason: ~a." (error-type condition) (error-number condition) (reason condition)))))

(define-condition careerpacific-integration-layer (careerpacific-condition)
  ()
  (:documentation "Error number start from 1000"))

(define-condition careerpacific-business-layer (careerpacific-condition)
  ()
  (:documentation "Error number start from 2000"))

(define-condition careerpacific-interface-layer (careerpacific-condition)
  ()
  (:documentation "Error number start from 3000"))

(define-condition careerpacific-application-layer (careerpacific-condition)
  ()
  (:documentation "Error number start from 4000"))

(define-condition careerpacific-utils-error (careerpacific-integration-layer)
  ()
  (:documentation "Error number start from 1001-2000"))

(define-condition careerpacific-invalid-arguments-error (careerpacific-integration-layer)
  ()
  (:documentation "Error number start from 2001-3000"))

(define-condition careerpacific-illegal-operation-error (careerpacific-application-layer)
  ()
  (:documentation "Error number start from 3001-4000"))
