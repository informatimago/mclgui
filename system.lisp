;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               system.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    System functions
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
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

(in-package "MCLGUI.SYSTEM")
(objcl:enable-objcl-reader-macros)



;;;
;;; The hairy subject of saving and reloading images, with the
;;; restoration of resources and pointers.  
;;;


;; We define on-* macros that let us define functions that are called
;; at specific times, and in the following order:
;;
;; quit
;;     on-quit
;; 
;; save-application
;;     on-quit
;;     on-save
;; 
;; launch
;;     on-restore
;;     on-load-and-now
;;     on-startup





#-ccl
(defvar *lisp-cleanup-functions* '()
  "
The *LISP-CLEANUP-FUNCTIONS* variable contains a list of functions of
no arguments on which funcall is run just before Macintosh Common Lisp
exits (via QUIT or SAVE-APPLICATION).  These functions are called just
after the windows are closed.

When saving an application, the functions in *LISP-CLEANUP-FUNCTIONS*
are run, then the functions in *SAVE-EXIT-FUNCTIONS* are run.
")


#-ccl
(defvar *save-exit-functions* '()
  "
The *SAVE-EXIT-FUNCTIONS* variable contains a list of functions to be
called when an image is saved.  These functions should perform any
preparation necessary for the image saving.  The functions are called
in the order in which they appear in the list.

When saving an application, the functions in *LISP-CLEANUP-FUNCTIONS*
are run, then the functions in *SAVE-EXIT-FUNCTIONS* are run.
")


#-ccl
(defvar *restore-lisp-functions* '())


#-ccl
(defvar *lisp-user-pointer-functions* '())


#-ccl
(defvar *lisp-startup-functions* '()
  "
The *LISP-STARTUP-FUNCTIONS* variable contains a list of functions of
no arguments on which funcall is run after Macintosh Common Lisp
starts, just before it enters the top-level function (usually the
Listener’s read loop).  The functions contained in
*LISP-STARTUP-FUNCTIONS* are run after the functions specified by
DEF-LOAD-POINTERS and before the init file is loaded.  The functions
are called in reverse order from the order in which they appear in the
list. 
")

(defvar *application-should-terminate-functions* '()
  "Functions called when the delegate of NSApplication receives -applicationShouldTerminate:.
If :CANCEL is thrown, the application doesn't terminate.")

(defvar *application-did-finish-launching-functions* '()
  "Functions called when NSApplication sends the -applicationDidFinishLaunching: notification.")


#| in ccl:

;; Add function to lisp system pointer functions, and run it if it's
;; not already there.

(defmacro def-ccl-pointers (name arglist &body body &aux (old (gensym)))
  `(flet ((,name ,arglist ,@body))
     (let ((,old (member ',name *lisp-system-pointer-functions* :key #'function-name)))
       (if ,old
         (rplaca ,old #',name)
         (progn
           (push #',name *lisp-system-pointer-functions*)
           (,name))))))

(defmacro def-load-pointers (name arglist &body body &aux (old (gensym)))
  `(flet ((,name ,arglist ,@body))
     (let ((,old (member ',name *lisp-user-pointer-functions* :key #'function-name)))
       (if ,old
         (rplaca ,old #',name)
         (progn
           (push #',name *lisp-user-pointer-functions*)
           (,name))))))

|#


(defmacro define-on-operators (base-name list-var &key and-now)
  (let ((fname (intern (format nil "ON-~A*" (symbol-name base-name))))
        (mname (intern (format nil "ON-~A"  (symbol-name base-name)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,fname (function-name thunk and-now)
         (when thunk
           (setf (symbol-function function-name) thunk))
         (pushnew function-name ,list-var)
         (when and-now (funcall function-name)))
       (defmacro ,mname (function-name &body body)
         (multiple-value-bind (docstring declarations body) (parse-body :lambda body)
           `(,',fname ',function-name
                    ,(when body `(lambda ()
                                   ,@docstring
                                   ,@declarations
                                   (block ,function-name ,@body)))
                    ,,and-now)))
       ',mname)))


(define-on-operators quit         *lisp-cleanup-functions*)
(define-on-operators save         *save-exit-functions*)

(define-on-operators restore      *restore-lisp-functions*)
(define-on-operators application-did-finish-launching  *application-did-finish-launching-functions*)
(define-on-operators application-should-terminate      *application-should-terminate-functions*)


#-ccl
(define-on-operators load-and-now *lisp-user-pointer-functions*  :and-now t)

#+ccl
(defmacro on-load-and-now (function-name &body body)
  `(progn
     (setf (symbol-function ',function-name) (lambda () (block ,function-name ,@body)))
     (def-load-pointers ,function-name () ,@body)))
(define-on-operators startup      *lisp-startup-functions*)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *initializer* nil)

  @[NSObject subClass:MclguiInitializer
             slots:()]

  @[MclguiInitializer
    method:(applicationDidFinishLaunching:(:id)notification)
    resultType:(:void)
    body:
    (declare (ignore notification))
    (with-simple-restart (abort "Abort (possibly crucial) startup functions.")
      (flet ((call-with-restart (f)
               (with-simple-restart 
                   (continue "Skip (possibly crucial) startup function ~S."
                             (if (symbolp f)
                                 f
                                 #+ccl(ccl::function-name f)
                                 #-ccl f))
                 (funcall f))))
        (map nil (function call-with-restart) *application-did-finish-launching-functions*)))
    [[NSNotificationCenter defaultCenter] removeObserver:self]
    [self release]
    (setf *initializer* nil)]
  
  );;eval-when


(on-restore add-application-did-finish-launching-initializer
  (setf *initializer* [MclguiInitializer new])
  [[NSNotificationCenter defaultCenter]
   addObserver:*initializer*
   selector:(objc:@selector "applicationDidFinishLaunching:")
   name:#$NSApplicationDidFinishLaunchingNotification
   object:nil])


;;;
;;;---------------------------------------------------------------------
;;;

(defmacro without-interrupts (&body body)
  "
The WITHOUT-INTERRUPTS special form executes form with all event
processing disabled, including abort.

You should use WITHOUT-INTERRUPTS sparingly because anything executed
dynamically within it cannot be aborted or easily debugged.  However,
you must often use WITHOUT-INTERRUPTS in code that causes a window to
be redisplayed.  If you need to invalidate a number of regions in a
window, do it inside a without-interrupts form to prevent multiple
redisplays.
"
  ;; Note: the mcl implementation doesn't seem to do anything more:
  #+ccl `(ccl:without-interrupts ,@body)
  #-ccl `(progn ,@body))


;;;
;;;---------------------------------------------------------------------
;;;

(defun fixnump (object)
  (typep object 'fixnum))


(defun ed-beep (&optional (duration 3) &rest args)
  (declare (ignorable duration args))
  (format-trace 'edbeep duration args)
  #+ccl (#_NSBeep)
  #-ccl (niy ed-beep duration args))


(defun get-sys-just ()
  0)



(defun ensure-list (object)
  "
RETURN:         If OBJECT is a list then OBJECT, otherwise a fresh
                list containing OBJECT.
"
  (if (listp object)
      object
      (list object)))


(defun list-designator (object)
  "
RETURN:         If the OBJECT is a list containing a single non-NIL
                atom, then this first element is returned, else OBJECT.
"
  (if (and (listp object)
           object
           (endp (rest object))
           (first object)
           (atom (first object)))
      (first object)
      object))


(defun standard-generic-function-p (object)
  (typep object 'common-lisp:standard-generic-function))


(defun method-exists-p (op object)
  #+ccl (ccl:method-exists-p op object)
  #-ccl (error "method-exists-p not implemented on ~S" (lisp-implementation-type)))

;;;; THE END ;;;;
