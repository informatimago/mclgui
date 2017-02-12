;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               process.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Multi processing features for MCLGUI.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-13 <PJB> Created.
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
(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)
(declaim (declaration stepper))


#-ccl (defvar *main-thread* nil)
#+ccl (define-symbol-macro *main-thread* ccl::*cocoa-event-process*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-on-main-thread-form (body wait)
    "
BODY:   Should be a list containing a single Objective-C message send
        with zero or one argument, or a body.

WAIT:   Whether we must wait for the message to return from the main
        thread.  When true, the generated form will wait and if body
        doesn't contain a single Objective-C message with zero or one
        argument, then it will return the result of the body.

RETURN: A form performing BODY on the main thread.
"
    (declare (stepper disable))
    (let ((varg (gensym)))
      (labels ((objcmsg (message)
                 (check-type message (or keyword
                                         (cons symbol (cons symbol null))))
                 (cond
                   ((keywordp message)
                    (oclo:lisp-to-objc-message (list message)))
                   ((and (listp message)
                         (eql 'cl:quote (first message))
                         (symbolp (second message)))
                    (oclo:lisp-to-objc-message (list (second message))))
                   (t
                    (error "Invalid message ~S" message))))
               (objarg (argument)
                 (if (null argument)
                     '*null*
                     `(let ((,varg ,argument))
                        (if (numberp ,varg)
                            (ccl:%int-to-ptr ,varg)
                            ,varg))))
               (unless-main-thread (form)
                 `(if (eql *main-thread* (bt:current-thread))
                      (progn ,@body)
                      ,form))
               (general-case ()
                 (if wait
                     (let ((vmb (gensym)))
                       `(let ((,vmb (make-mailbox)))
                          (application-eval-enqueue *application*
                                                    (lambda ()
                                                      (declaim (stepper disable))
                                                      (mailbox-post ,vmb (ignore-errors ,@body))))
                          (mailbox-collect ,vmb)))
                     `(application-eval-enqueue *application* (lambda ()
                                                                (declaim (stepper disable))
                                                                ,@body)))))
        (unless-main-thread
         (if (= 1 (length body))
             (let ((form (first body)))
               (cond
                 ((and (listp form)
                       (<= 3 (length form) 4)
                       (eql 'objc:send (first form)))
                  (destructuring-bind (send recipient message &optional argument) form
                    (declare (ignore send))
                    ;; TODO: eval once arguments!
                    `(progn ;; (format-trace "performSelectorOnMainThread" ',recipient ,message ,argument ,wait)
                       [,recipient performSelectorOnMainThread: (oclo:selector ,(objcmsg message))
                                   withObject: ,(objarg argument)
                                   waitUntilDone: ,wait])))
                 ((and (listp form)
                       (<= 2 (length form) 3)
                       (eql 'objc:objc-message-send-super (first form)))
                  (destructuring-bind (send message &optional argument) form
                    (declare (ignore send))
                    ;; TODO: eval once arguments!
                    `(progn ;; (format-trace "performSelectorOnMainThread" 'super ,message ,argument ,wait)
                       [super performSelectorOnMainThread: (oclo:selector ,(objcmsg message))
                              withObject: ,(objarg argument)
                              waitUntilDone: ,wait])))
                 (t
                  (general-case))))
             (general-case)))))))


(defmacro on-main-thread (&body body)
  (generate-on-main-thread-form body nil))

(defmacro on-main-thread/sync (&body body)
  (generate-on-main-thread-form body t))


;; (generate-on-main-thread-form '((print 1) (print 2)) nil)
;; (application-eval-enqueue *application* (lambda nil (print 1) (print 2)))
;;
;; (generate-on-main-thread-form '((print 1) (print 2)) t)
;; (let ((#1=#:g151003 (make-mailbox))) (application-eval-enqueue *application* (lambda nil (setf (%mailbox-message #1#) (progn (print 1) (print 2))))) (mailbox-collect #1#))
;;
;; (generate-on-main-thread-form '([o m]) nil)
;; (progn (objc:send o :perform-selector-on-main-thread (com.informatimago.objective-c.lower:selector "m") :with-object *null* :wait-until-done nil))
;;
;; (generate-on-main-thread-form '([o m]) t)
;; (progn (objc:send o :perform-selector-on-main-thread (com.informatimago.objective-c.lower:selector "m") :with-object *null* :wait-until-done t))
;;
;; (generate-on-main-thread-form '([o m:a]) nil)
;; (progn (objc:send o :perform-selector-on-main-thread (com.informatimago.objective-c.lower:selector "m:") :with-object (let ((#1=#:g151004 a)) (if (numberp #1#) (ccl:%int-to-ptr #1#) #1#)) :wait-until-done nil))
;; (generate-on-main-thread-form '([o m:a]) t)
;; (progn (objc:send o :perform-selector-on-main-thread (com.informatimago.objective-c.lower:selector "m:") :with-object (let ((#1=#:g151005 a)) (if (numberp #1#) (ccl:%int-to-ptr #1#) #1#)) :wait-until-done t))


;; (test/mailbox)

(defun initialize/process ()
  #-ccl (setf *main-thread* (on-main-thread/sync (bt:current-thread)))
  (values))

;;;; THE END ;;;;

