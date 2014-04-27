;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mailbox.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    MAILBOX: to send one message between two threads.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-27 <PJB> Extracted from process.
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
(declaim (declaration stepper))


;;; The consummer may call MAILBOX-COLLECT before or after the
;;; producer calls MAILBOX-POST.   If it calls before, then it waits
;;; until the producer notifies the mailbox is full.

(defstruct (mailbox
            (:conc-name %mailbox-)
            (:constructor %make-mailbox (lock condition)))
  lock condition message full)

(defun make-mailbox (&optional (name "mailbox"))
  (declare (stepper disable))
  (%make-mailbox (bt:make-lock name) (bt:make-condition-variable :name name)))

(defun mailbox-collect (mailbox)
  (declare (stepper disable))
  (let (result)
    (bt:with-lock-held ((%mailbox-lock mailbox))
      (unless (%mailbox-full mailbox)
        (bt:condition-wait (%mailbox-condition mailbox) (%mailbox-lock mailbox)))
      (setf result (%mailbox-message mailbox)))
    result))

(defun mailbox-post (mailbox message)
  (declare (stepper disable))
  (bt:with-lock-held ((%mailbox-lock mailbox))
    (setf (%mailbox-message mailbox) message
          (%mailbox-full mailbox) t)
    (bt:condition-notify (%mailbox-condition mailbox))))


;;; --------------------------------------------------------------------

(defun test/mailbox ()
  (let ((start (get-universal-time)))
    (assert (equal (let ((mb (make-mailbox)))
                     (bt:make-thread (lambda () (sleep 4) (mailbox-post mb 42)))
                     (mailbox-collect mb))
                   42))
    (let ((end (get-universal-time)))
      (assert (<= 3 (- end start) 5))))
  (let ((start (get-universal-time)))
    (assert (equal (let ((mb (make-mailbox)))
                     (bt:make-thread (lambda () (mailbox-post mb 42)))
                     (mailbox-collect mb))
                   42))
    (let ((end (get-universal-time)))
      (assert (<= 0 (- end start) 1))))
  :success)

;;;; THE END ;;;;
