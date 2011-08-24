(in-package :careerpacific)

(export '())

(contextl:deflayer authenticated-user ())

(contextl:deflayer candidate (authenticated-user))

(contextl:deflayer employer (authenticated-user))

(contextl:deflayer authorized-user (candidate employer))

(contextl:deflayer consultant (authorized-user))

(contextl:deflayer administrator-user (authorized-user))

(contextl:deflayer staff (administrator-user))

(contextl:deflayer super-user (staff))

(contextl:deflayer administrator (super-user))