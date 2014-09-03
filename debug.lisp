;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               debug.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
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
;;;;    
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
