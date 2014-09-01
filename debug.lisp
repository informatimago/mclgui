(in-package "MCLGUI")

(defun function-address (function)
  (read-from-string (object-identity function)))

(defun all-functions ()
  (let ((functions '()))
    (do-symbols (sym)
      (let ((function (cond ((fboundp sym) (symbol-function sym))
                            ((ignore-errors (fdefinition (list 'setf sym)))))))
        (when function
          (push (cons (function-address function) function) functions))))
    (sort (coerce functions 'vector) (function <) :key (function car))))

(defparameter *all-functions* (all-functions))

(defparameter *p* #x3020045fa253)

(defun find-function-from-address (address)
  (multiple-value-bind (found index order)
      (com.informatimago.common-lisp.cesarum.utility:dichotomy-search
       *all-functions* address
       (lambda (a b) (cond ((< a b) -1) ((> a b) 1) (0)))
       :key (function car))
    (when (or found (plusp order))
      (let ((entry (aref *all-functions* index)))
        (values (cdr entry)
                (- address (car entry)))))))



;; (defmacro defun (name lambda-list &body body)
;;   `(cl:defun ,name ,lambda-list
;;      (profile-count-function ',name)
;;      ,@body))
