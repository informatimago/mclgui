;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               debug.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Debugging stuff.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(in-package "MCLGUI.DEBUGGING")
(declaim (declaration stepper))


(defvar *mclgui-trace*   (make-synonym-stream '*trace-output*))
(defvar *mclgui-package* (load-time-value (find-package "MCLGUI")))

(defmacro unfrequently (frequency &body body)
  (let ((vcount (gensym))
        (vfrequency (gensym)))
    `(let ((*print-case* :downcase)
           (*package*    *mclgui-package*)
           (,vcount (load-time-value (list 0)))
           (,vfrequency ,frequency))
       (when (<= 1 (incf (car ,vcount) ,vfrequency))
         (setf (car ,vcount) 0)
         ,@body))))


(defmacro niy (operator &rest parameters)
  (let ((vonce (gensym)))
   `(let ((*print-case* :downcase)
          (*package*    *mclgui-package*)
          (,vonce (load-time-value (list t))))
      (when (prog1 (car ,vonce) (setf (car ,vonce) nil))
        (format *mclgui-trace* "~&(~40A (~S~:{ (~S ~S)~}))~%"
                "not implemented yet:"
                ',operator (mapcar (lambda (var) (list var (type-of var)))
                                   (list ,@parameters)))
        (force-output *mclgui-trace*)))))


(defmacro uiwarn (control-string &rest args)
  `(let ((*print-case* :downcase)
         (*package*    *mclgui-package*))
     (format *mclgui-trace* "~&(~?)~%" ',control-string (list ,@args))
     (force-output *mclgui-trace*)))


(defun format-trace (method &rest arguments)
  (declare (stepper disable))
  (let ((*print-case* :downcase)
        (*package*    *mclgui-package*))
    (flet ((out (stream)
             (format stream "~&(~40A ~{~S~^ ~})~%" method arguments)
             (force-output stream)
             t))
      ;; (patchwork.builder::print-streams)
      (or (ignore-errors (out *mclgui-trace*))
          (ignore-errors (out *trace-output*))
          (ignore-errors (out *standard-output*)))
      (let ((listeners (gui::active-listener-windows)))
        (when listeners
          (let ((hi::*current-buffer* (hi:hemlock-view-buffer
                                       (gui::hemlock-view
                                        (slot-value (first listeners)
                                                    'gui::echo-area-view)))))
            (hemlock::end-of-buffer-command nil)))))
    (first arguments)))

(defmacro time/stdout (&body body)
  `(let ((trace-output *trace-output*))
     (let ((*trace-output* *standard-output*))
       (time (let ((*trace-output* trace-output))
               ,@body)))))



(defun object-identity (object)
  "
RETURN:         A string containing the object identity as printed by
                PRINT-UNREADABLE-OBJECT.
"
  (declare (stepper disable))
  (let ((*print-readably* nil))
    (let ((ident
           (with-output-to-string (stream)
             (print-unreadable-object (object stream :type nil :identity t)))))
      (subseq ident 3 (1- (length ident))))))


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

(defvar *all-functions* '())

#-(and) (
         (defparameter *all-functions* (all-functions))
         (defparameter *p* #x3020045fa253)
         )


(defun find-function-from-address (address)
  (multiple-value-bind (found index order)
      (com.informatimago.common-lisp.cesarum.utility:dichotomy-search
       (or *all-functions* (setf *all-functions* (all-functions))) address
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



;;;; THE END ;;;;
