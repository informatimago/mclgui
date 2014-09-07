;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               debugger-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Debugger Dialog
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-05 <PJB> Created.
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

(define-condition user-interrupt (error)
  ()
  (:report "User Interruption"))




(defclass debugger-dialog (dialog)
  ((condition :initarg :condition :reader debugger-dialog-condition)
   (restarts  :initarg :restarts  :reader debugger-dialog-restarts))
  (:default-initargs :window-title "Debugger" :size #@(512 300)))

(defmethod initialize-instance :after ((dialog debugger-dialog) &key &allow-other-keys)
  (add-debugger-dialog-subviews dialog))

(defun make-debugger-dialog-restart-button (i restart)
  (let ((button (make-instance 'button-dialog-item
                        :view-size (make-point 50 20)
                        :dialog-item-text (princ-to-string (restart-name restart))
                        :dialog-item-action (lambda (item)
                                              (declare (ignore item))
                                              [(handle *application*) stopModalWithCode:i]))))
    button))


(defgeneric add-debugger-dialog-subviews (dialog)
  (:method ((dialog debugger-dialog))
    (let ((layout (make-instance 'linear-layout
                                 :left-margin 10 :right-margin 10
                                 :top-margin 10 :bottom-marign 10
                                 :orientation :vertical
                                 :direction :top-down
                                 :horizontal-alignment :left
                                 :vertical-alignment :top
                                 :spacing 20
                                 :subviews (list
                                            (make-instance 'static-text-dialog-item
                                                           :text-truncation :end
                                                           :view-size (make-point 500 40)
                                                           :dialog-item-text (princ-to-string (debugger-dialog-condition dialog)))
                                            (make-instance 'flow-layout
                                                           :view-size (make-point 500 200)
                                                           :orientation :horizontal
                                                           :vertical-direction :bottom-up
                                                           :horizontal-direction :right-to-left
                                                           :spacing 16 :line-spacing 8
                                                           :subviews (loop
                                                                       :for i :from 0
                                                                       :for restart :in (debugger-dialog-restarts dialog)
                                                                       :collect (make-debugger-dialog-restart-button i restart)))))))
      (add-subviews dialog layout)
      (adjust-layout-to-parent layout))))


(defmethod set-view-size :after ((dialog debugger-dialog) h &optional v)
  (declare (ignore h v))
  (adjust-layout-to-parent (aref (view-subviews dialog) 0)))




(defun invoke-debugger-window (condition)
  (let ((restarts (compute-restarts (make-condition 'user-interrupt))))
    (let ((index (modal-dialog (make-instance 'debugger-dialog
                                              :view-size (make-point 500 300)
                                              :close-box-p nil 
                                              :grow-box-p t
                                              :condition condition
                                              :restarts restarts))))
      (print `(debugger-dialog returned ,index))
      (cond
        ((< -1 index (length restarts))
         (invoke-restart-interactively (elt restarts index)))
        (t
         '(this should not occur))))))


(defun run-debugger ()
  (restart-case
      (invoke-debugger-window (make-condition 'user-interrupt))
    (continue ()
      :report "Continue")
    (abort-run-modal ()
      :report "Abort Run Modal"
      [(handle *application*) abortModal])
    (stop-run-modal ()
      :report "Stop Run Modal"
      [(handle *application*) stopModal])
    (stop-run-modal-with-code (code)
      :report "Stop Run Modal With Code"
      :interactive read-integer-code
      [(handle *application*) stopModalWithCode:code])))


#-(and) (progn

          (invoke-debugger-window (make-condition 'user-interrupt))

          (run-debugger)

          )


;;;; THE END ;;;;
