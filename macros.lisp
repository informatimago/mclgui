;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    MCLGUI internal macros.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "MCLGUI")
;;(objcl:enable-objcl-reader-macros)
(declaim (declaration stepper))

(define-modify-macro nconcf (&rest args)
  nconc "Nconc onto list")

(defmacro add-to-list (list-place element)
    "
DO:             Destructively add the ELEMENT to the LIST-PLACE in the
                last position.
"
  `(appendf ,list-place (list ,element)))


(defmacro delete-from-list (list-place element)
  "
DO:             Destructuvely delete from the list stored in place
                LIST-PLACE the ELEMENT.
"
  `(deletef ,list-place ,element))

(defmacro insert-into-list (&whole whole &environment env
                            list-place position element)
  "
DO:             Destructively insert into the LIST-PLACE the ELEMENT
                in the given position.

POSITION:       0 means insert in front of the list.
                n means after the n-th element.
"
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion list-place env)
    (when (cdr new) (error "Can't expand ~S" whole))
    (let ((vposition (gensym))
          (velement  (gensym))
          (vplace    (car new)))
      `(let* (,@(mapcar #'list dummies vals) (,vplace ,getter)
                (,vposition ,position)
                (,velement  ,element))
         (if (zerop ,vposition)
             (push ,velement ,vplace)
             (push ,velement (cdr (or (nthcdr (1- ,vposition) ,vplace)
                                      (last ,vplace)))))
         ,setter))))



(defmacro dovector ((var vector &optional result) &body body)
  (let ((vvector (gensym "vector"))
        (vindex  (gensym "index"))
        (vlength (gensym "length")))
    `(block nil
       (let* ((,vvector ,vector)
              (,vlength (length ,vvector))
              (,vindex  -1))
         (tagbody
            (go :test)
          :loop
            (let ((,var (aref ,vvector ,vindex)))
              ,@body)
          :test
            (incf ,vindex)
            (if (< ,vindex ,vlength)
                (go :loop))
            (return ,result))))))


(defvar *step-mode* :run)


(defun call-print-parseable-object (object stream type identity thunk)
  "
SEE:            PRINT-PARSEABLE-OBJECT
"
  (declare (stepper disable))
  (let ((*step-mode* :run))
    (if *print-readably*
        (error 'print-not-readable :object object)
        (progn
          (format stream "~S"
                  (append (when type
                            (list (class-name (class-of object))))
                          (funcall thunk object)
                          (when identity
                            (list (object-identity object)))))
          object))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extract-slots (ovar slots)
    "
SEE:            PRINT-PARSEABLE-OBJECT
RETURN:         A form building a plist of slot values.
"
    (cons 'list
          (loop
            :for slot :in slots
            :collect  (if (symbolp slot)
                          (intern (symbol-name slot) "KEYWORD")
                          `(quote ,(first slot)))
            :collect  (if (symbolp slot)
                        `(ignore-errors (slot-value ,ovar ',slot))
                        `(ignore-errors ,(second slot)))))))

(defgeneric slots-for-print (object)
  (:documentation "Returns a plist of slots.")
  (:method-combination append))

(defmacro define-printer (class-name-and-options &rest slots)
  (if (symbolp class-name-and-options)
      `(define-printer (,class-name-and-options) ,@slots)
      (destructuring-bind (class-name &key (type t) (identity t)) class-name-and-options
        `(progn
           (defmethod slots-for-print append ((self ,class-name))
             ,(extract-slots 'self slots))
           (defmethod print-object ((self ,class-name) stream)
             (call-print-parseable-object self stream ,type ,identity
                                          (lambda (self)
                                            (declare (ignorable self) (stepper disable))
                                            (slots-for-print self))))))))


(defmacro print-parseable-object ((object stream &key (type t) identity) &rest slots)
  "

DO:             Prints on the STREAM the object as a list.  If all the
                objects printed inside it are printed readably or with
                PRINT-PARSEABLE-OBJECT, then that list should be
                readable, at least with *READ-SUPPRESS* set to T.

OBJECT:         Either a variable bound to the object to be printed,
                or a binding list (VARNAME OBJECT-EXPRESSION), in
                which case the VARNAME is bound to the
                OBJECT-EXPRESSION during the evaluation of the SLOTS.

STREAM:         The output stream where the object is printed to.

TYPE:           If true, the class-name of the OBJECT is printed as
                first element of the list.

IDENTITY:       If true, the object identity is printed as a string in
                the last position of the list.

SLOTS:          A list of either a symbol naming the slot, or a list
                (name expression), name being included quoted in the
                list, and the expression being evalauted to obtain the
                value.

RETURN:         The object that bas been printed (so that you can use
                it in tail position in PRINT-OBJECT conformingly).

"
  `(locally (declare (stepper disable))
     ,(if (symbolp object)
         `(call-print-parseable-object ,object ,stream ,type ,identity
                                       (lambda (,object)
                                         (declare (ignorable ,object) (stepper disable))
                                         ,(extract-slots object slots)))
         (destructuring-bind (ovar oval) object
           `(let ((,ovar ,oval))
              (call-print-parseable-object ,ovar ,stream ,type ,identity
                                           (lambda (,ovar)
                                             (declare (ignorable ,ovar) (stepper disable))
                                             ,(extract-slots object slots))))))))





(define-condition simple-program-error (simple-error program-error)
  ())


(defgeneric copy-object-from (dst src)
  (:documentation "
DO:             Perform a deep copy of the slots of the SRC object to
                the DST object.  Methods are usually specialized  only
                for DST and SRC of the same class.

DST:            An instance.

SRC:            An instance.

RETURN:         DST
"))


;; The specifications of catch-cancel and throw-cancel in 3.0/4.0 are
;; contradictory.  We just use the implementation in mcl 5.1

(defmacro catch-cancel (&body body)
  "
The catch-cancel macro sets up a cancel catch and evaluates form. It
returns the value of the last form if there was no cancel throw. Otherwise,
it returns the symbol :cancel.
"
  `(catch :cancel ,@body))


(defmacro throw-cancel (&optional value)
  "
The throw-cancel macro throws the value of value-form to the most
recent outstanding catch-cancel.
"
  `(throw :cancel ,value))


(defun ensure-simple-string (s)
  (cond ((simple-string-p s) s)
        ((stringp s)
         (let* ((len  (length s))
                (base (every (lambda (ch) (typep ch 'base-char)) s))
                (new  (if base
                          (make-string len :element-type 'base-char)
                          (make-string len :element-type 'character))))
           (replace new s)))
        (t (error 'simple-type-error
                  :datum s :expected-type 'string
                  :format-control "~S: bad argument type, got ~S of type ~S, expected a ~S"
                  :format-arguments (list 'ensure-simple-string s (type-of s) 'string)))))


;;; Simplified form of with-slots.  Expands into a let instead of a symbol-macrolet
;;; Thus, you can access the slot values, but you can't setq them.
(defmacro with-slot-values (slot-entries instance-form &body body)
  (let ((instance (gensym)) var slot-name bindings)
    (dolist (slot-entry slot-entries)
      (cond ((symbolp slot-entry)
             (setq var slot-entry slot-name slot-entry))
            ((and (listp slot-entry) (cdr slot-entry) (null (cddr slot-entry))
                  (symbolp (car slot-entry)) (symbolp (cadr slot-entry)))
             (setq var (car slot-entry) slot-name (cadr slot-entry)))
            (t (error "Malformed slot-entry: ~a to with-slot-values.~@
                       Should be a symbol or a list of two symbols."
                      slot-entry)))
      (push `(,var (slot-value ,instance ',slot-name)) bindings))
    `(let ((,instance ,instance-form))
       (let ,(nreverse bindings)
         ,@body))))


(defmacro with-item-rect ((var the-item) &body body)
  `(let ((,var (view-frame ,the-item)))
     ,@body))


(defmacro cycle (&rest items)
  (let ((vitems   (gensym))
        (vcurrent (gensym)))
    `(let* ((,vitems   (list ,@items))
            (,vcurrent ,vitems))
       (when (null ,vitems)
         (setf ,vitems ,vcurrent))
       (pop ,vcurrent))))

;;;; THE END ;;;;
