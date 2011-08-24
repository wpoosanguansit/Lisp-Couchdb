(in-package :careerpacific)

(defgeneric save (object) 
  (:documentation "this is the generic method for saving model object"))

(defgeneric save-or-update (object) 
  (:documentation "this is the generic method for saving or updating  model object.
    it automatically determine if the object needs a saving or an updating"))

(defgeneric erase (object) 
  (:documentation "this is the generic method for deleting model object"))

(defgeneric update (object) 
  (:documentation "this is the generic method for updating model object"))

(defgeneric retrieve (object id)
  (:documentation "this is the generic method for retrieving model object"))

