;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               computed-view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;    
;;;;    A generic view whose drawing is performed by a closure.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(defpackage "MCLGUI.EXAMPLE.COMPUTED-VIEW"
  (:use "COMMON-LISP" "MCLGUI")
  (:export "COMPUTED-VIEW"
           "COMPUTED-VIEW-DRAW-IT"
           "EXAMPLE"))
(in-package "MCLGUI.EXAMPLE.COMPUTED-VIEW")

(defclass computed-view (view)
  ((draw-it   :initarg :drawn-as
              :initarg :draw-it
              :initform nil
              :accessor computed-view-draw-it)))

(defmethod view-draw-contents ((view computed-view))
  (with-focused-view view
    (when (next-method-p) (call-next-method))
    (let ((draw-it (computed-view-draw-it view)))
      (when draw-it (funcall draw-it view)))))

(defun example ()
  (let ((win (make-instance 'window
                            :window-title "Example: Computed View"
                            :view-size #@(400 300))))
    
    (make-instance 'computed-view
                   :view-container win
                   :view-position #@(0 0)
                   :view-size #@(200 100)
                   :drawn-as (lambda (view)
                               (draw-rect (view-bounds view))
                               (with-pen-state (:pattern *gray-pattern*)
                                 (fill-rect* 20 20 160 60))))
    
    (make-instance 'computed-view
                   :view-container win
                   :view-position #@(200 100)
                   :view-size #@(200 100)
                   :drawn-as (lambda (view)
                               (draw-rect (view-bounds view))
                               (with-pen-state (:pattern *dark-gray-pattern*)
                                 (fill-ellipse 20 20 160 60))))
    win))

;; (example)

;;;; THE END ;;;;
