(in-package :careerpacific)

;;this will be the classes for signaling error and conditions for handling
;;events in the system.

(export '(careerpacific-db-error))

(define-condition careerpacific-db-error (careerpacific-integration-layer)
  ()
  (:documentation "Error number start from 0-100"))

