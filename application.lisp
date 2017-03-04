;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Application classes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-23 <PJB> Added application-name.
;;;;    2012-05-18 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
;;;;
;;;;    Some code extracted from MCL (LGPL):
;;;;    Copyright 1985-1988 Coral Software Corp.
;;;;    Copyright 1989-1994 Apple Computer, Inc.
;;;;    Copyright 1995-2000 Digitool, Inc.
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



(defgeneric application-name (application))
(defgeneric (setf application-name) (new-name application))

(defvar *application-name* "App")

(defmethod application-name ((application t))
  *application-name*)

(defmethod (setf application-name) (new-name (application t))
  (setf *application-name* new-name))


(defclass named-application-mixin ()
  ((name :initform nil :initarg :name :reader application-name
         :documentation "
RETURN:         The name of the application (a string). The default
                value is \"App\".

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")))

(defmethod (setf application-name) (new-name (application named-application-mixin))
  (when (next-method-p) (call-next-method))
  (setf (slot-value application 'name) new-name))




#-ccl
(defclass application (wrapper named-application-mixin)
  ())


#-ccl
(defclass lisp-development-system (application)
  ())


(defclass cocoa-ide-application (#-ccl lisp-development-system
                                 #+ccl wrapper #+ccl gui::cocoa-ide #+ccl named-application-mixin)
  ())




(defgeneric  application-command-line-arguments (application)
  (:method ((application application))
    #+ccl (slot-value application 'gui::command-line-arguments)
    #-ccl '()))

#-ccl
(defgeneric  application-init-file (application)
  (:method ((application application))
    nil))


(defmethod update-handle ((self application))
  (setf (handle self) [NSApplication sharedApplication]))

(defmethod unwrap ((self application))
  (unwrapping self
    (or (handle self) (update-handle self))))


(defgeneric application-error (application condition error-pointer)
  (:documentation "
The generic function APPLICATION-ERROR is called whenever a condition
is signaled that has no handler.  The method for APPLICATION quits the
application. The method for LISP-DEVELOPMENT-SYSTEM enters a
BREAK-LOOP.

You can customize your error handling by defining a subclass of
application and setting *application* to an instance of your
class. User APPLICATION-ERROR methods should have a non-local exit,
because  if APPLICATION-ERROR returns, MCL calls it again with a
condition so that it may not return.  However, if it returns from that
call, MCL throws to the toplevel.

APPLICATION:    The application. MCL standard event handling always
                uses the value of *APPLICATION*.

CONDITION:      The error condition.

ERROR-POINTER:  An integer representing the address of the stack frame
                of the function that signaled the error.  The method
                specialized on lisp-development-system uses this
                address to determine the name of the function and uses
                this address as an input to the stack backtrace
                facility.
")
  (:method ((application application) condition error-pointer)
    (declare (ignore condition error-pointer))
    (niy application-error))
  (:method ((application lisp-development-system) condition error-pointer)
    (declare (ignore error-pointer))
    (invoke-debugger condition)))


(defgeneric application-overwrite-dialog (application filename prompt)
  (:documentation "
The generic function APPLICATION-OVERWRITE-DIALOG displays a
dialog when there is an attempt to overwrite an existing file.  The dialog
asks whether to replace the file or choose a new filename.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

FILENAME:       A pathname or string that specifies an existing file.

PROMPT:         The prompt message.
")
  (:method ((application application) filename prompt)
    (declare (ignore filename prompt))
    (niy application-overwrite-dialog)))


(defgeneric find-edit-menu (application)
  (:documentation "
RETURN:         the first menu in the menu bar containing the Command-X.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy find-edit-menu)))


(defmethod view-key-event-handler ((application application) key)
  "
The generic function VIEW-KEY-EVENT-HANDLER is called with
*APPLICATION* as the first argument when there are no active windows
and the user presses a key on the keyboard.  The method for
application sounds a beeps.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

KEY:            The current keystroke character.
"
  (declare (ignore key))
  (ed-beep))


(defgeneric application-file-creator (application)
  (:documentation "
RETURN:         a four-character string or symbol for Finder file
                creator type.  The default value is :|????| (the value
                of the constant DEFAULT-APPL-CREATOR).

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    default-appl-creator))


(defgeneric application-about-view (application)
  (:documentation "
RETURN:         A view instance containing dialog items to display in
                the About dialog; the mandatory MCL redistribution
                notice is placed below this view to make the About
                dialog.  The default value is a static text item with
                the application's name.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-about-view)))


(defgeneric application-about-dialog (application)
  (:documentation "
RETURN:         A view instance containing dialog items to display in
                the About dialog; the mandatory MCL redistribution
                notice is placed below this view to make the About
                dialog.  The default value is a static text item with
                the application's name.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-about-view)))


(defgeneric application-suspend-event-handler (application)
  (:documentation "
This function is called with the value of *APPLICATION* when MCL is
suspended.  The application method converts the scrap, deactivates
windows, and hides windoids if *HIDE-WINDOIDS-ON-SUSPEND* is true.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-suspend-event-handler)))


(defgeneric application-resume-event-handler (application)
  (:documentation "
This function is called with the value of *APPLICATION* when MCL is
resumed. The application method converts the scrap, reactivates the
front window, and shows hidden windoids if *HIDE-WINDOIDS-ON-SUSPEND*
is true.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.
")
  (:method ((application application))
    (niy application-resume-event-handler)))


(defgeneric application-eval-enqueue (application form)
  (:documentation "
This function is called with the value of *APPLICATION* by the
EVAL-ENQUEUE function.  The application method calls funcall (for a
function or symbol) or eval (for a list) on form.  The
LISP-DEVELOPMENT-SYSTEM method adds form to the eval queue of the
frontmost active listener if one exists, otherwise invokes
CALL-NEXT-METHOD.

APPLICATION:    The application.  MCL standard event handling always
                uses the value of *APPLICATION*.

FORM:           A symbol, function or lisp form.
")
  (:method ((application t) form)
    (let ((evaluator [[MclguiEvaluator alloc] init]))
      (setf (evaluator-thunk evaluator) (typecase form
                                          ((or symbol cl:function) form)
                                          (otherwise   (lambda () (eval form))))
            (evaluator-source evaluator) form)
      (on-main-thread [evaluator evaluate])
      [evaluator autorelease]))
  (:method ((application lisp-development-system) form)
    (declare (ignorable form))
    ;; TODO: see how to integrate with ccl::lisp-development-system
    (call-next-method)))


(defgeneric application-did-finish-launching (application)
  (:method ((application t))
    (values)))

;;;---------------------------------------------------------------------
;;;
;;; Run Loop task.
;;;
;;; We install a timer on the main run loop to process the events.
;;;


(defvar *run-loop-modes*     nil)
(defvar *run-loop-timer*     nil)
(defvar *run-loop-evaluator* nil)

(defun run-loop-task ()
  (with-event-environment
    (event-dispatch)))


;; (application-eval-enqueue *application* '(invoke-debugger (make-condition 'error)))
;; (setf (evaluator-thunk *run-loop-evaluator*) (function run-loop-task))

(defun initialize-run-loop-evaluator ()
  (when *run-loop-timer*
    [*run-loop-timer* invalidate])
  (let* ((evaluator [[MclguiEvaluator alloc] init])
         (timer [NSTimer timerWithTimeInterval: (cgfloat 1/60)
                         target:evaluator
                         selector:(objc:@selector |evaluate|)
                         userInfo:*null*
                         repeats:t]))
    (setf (evaluator-thunk evaluator) (function run-loop-task))
    (setf *run-loop-modes*    (list #$NSDefaultRunLoopMode
                                    #$NSRunLoopCommonModes
                                    ;; #$NSConnectionReplyMode
                                    #$NSModalPanelRunLoopMode
                                    #$NSEventTrackingRunLoopMode)
          *run-loop-timer*     timer
          *run-loop-evaluator* evaluator)
    (dolist (mode *run-loop-modes*)
      [[NSRunLoop mainRunLoop] addTimer:timer forMode:mode])
    (values)))


;;;---------------------------------------------------------------------
;;;
;;; Initialization
;;;

(defun initialize/application ()
  ;; Now (eq 'ui::*application* 'ccl::*application*), so *application* should already be set.
  (unless *application*
    (setf *application* (make-instance 'application))) ;; or what subclass?
  (initialize-run-loop-evaluator)
  (values))

;;;; THE END ;;;;
