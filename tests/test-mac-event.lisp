;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-mac-event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests for mac-event.lisp
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-11-09 <PJB> Extracted from mac-event.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "MCLGUI")


(defun test/post-event/dequeue-event ()
  (let ((e1 (make-event :what mouse-down :when (tick-count) :where #@(100 100)))
        (e2 (make-event :what key-down :when (+ 10 (tick-count)) :message (char-code #\a)))
        (e3 (make-event :what mouse-up :when (+ 20 (tick-count))  :where #@(200 150))))
    (let ((*event-queue* (make-event-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null  e) (e))))
    :success))

(defun test/%find-event ()
  (let ((e1 (make-event :what mouse-down :when (tick-count) :where #@(100 100)))
        (e2 (make-event :what key-down :when (+ 10 (tick-count)) :message (char-code #\a)))
        (e3 (make-event :what mouse-up :when (+ 20 (tick-count))  :where #@(200 150))))
    (let ((*event-queue* (make-event-queue)))
      (post-event (make-event))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (post-event (make-event))
      (with-mutex (event-queue-mutex *event-queue*)
        (let ((e (%find-event mouse-down-mask))) (assert (eql e e1) (e e1)))
        (let ((e (%find-event key-down-mask)))   (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-up-mask)))   (assert (eql e e3) (e e3)))
        (let ((e (%find-event activate-mask)))   (assert (null e)  (e)))
        (let ((e (%find-event (+ mouse-down-mask
                                 mouse-up-mask)))) (assert (eql e e1) (e e1)))))
    :success))

(defun test/%extract-event ()
  (let ((e1 (make-event :what mouse-down :when (tick-count) :where #@(100 100)))
        (e2 (make-event :what key-down :when (+ 10 (tick-count)) :message (char-code #\a)))
        (e3 (make-event :what mouse-up :when (+ 20 (tick-count))  :where #@(200 150))))
    (let ((*event-queue* (make-event-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (event-queue-mutex *event-queue*)
        (let* ((x (%find-event mouse-down-mask))
               (e (%extract-event x)))
          (assert (eql e e1) (e e1)))
        (let ((e (%find-event mouse-down-mask))) (assert (null e)  (e)))
        (let ((e (%find-event key-down-mask)))   (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-up-mask)))   (assert (eql e e3) (e e3))))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    (let ((*event-queue* (make-event-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (event-queue-mutex *event-queue*)
        (let* ((x (%find-event key-down-mask))
               (e (%extract-event x)))
          (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-down-mask))) (assert (eql e e1) (e e1)))
        (let ((e (%find-event key-down-mask)))   (assert (null e) (e)))
        (let ((e (%find-event mouse-up-mask)))   (assert (eql e e3) (e e3))))
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    (let ((*event-queue* (make-event-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (event-queue-mutex *event-queue*)
        (let* ((x (%find-event mouse-up-mask))
               (e (%extract-event x)))
          (assert (eql e e3) (e e3)))
        (let ((e (%find-event mouse-down-mask))) (assert (eql e e1) (e e1)))
        (let ((e (%find-event key-down-mask)))   (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-up-mask)))   (assert (null e)  (e))))
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    (let ((*event-queue* (make-event-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (event-queue-mutex *event-queue*)
        (let ((e (%extract-event (make-event))))
          (assert (null e) (e))))
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    :success))


(defun test/mac-event ()
  (test/post-event/dequeue-event)
  (test/%find-event)
  (test/%extract-event))

;;;; THE END ;;;;
