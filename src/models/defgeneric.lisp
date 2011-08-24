(in-package :careerpacific)

;;this is the convinient methods for objects like
;;resume and company-profile to add item to sub object list.

(defgeneric add (object value)
  :document "this is the generic method for adding sub objects like addresses to main objects like resume
             or company profile")

(defgeneric erase (object value)
  :document "this is to delete the sub objects like addresses from the main objects like resume, etc")

