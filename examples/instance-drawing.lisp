;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               instance-view.lisp
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

(defpackage "MCLGUI.EXAMPLE.INSTANCE-DRAWING"
  (:use "COMMON-LISP" "MCLGUI"
        "MCLGUI.EXAMPLE.PICTURE-VIEW")
  (:export "INSTANCE-DRAWING-VIEW"
           "RUN"))
(in-package "MCLGUI.EXAMPLE.INSTANCE-DRAWING")
(enable-sharp-at-reader-macro)


(defclass instance-drawing-view (picture-view)
  ())


(defun draw-test-rect (view where)
  (with-focused-view view
    (with-pen-state (:pattern *white-pattern*
                     :size #@(3 3)
                     :mode :patCopy)
      (draw-rect* (- (point-h where) 20) (- (point-v where) 20) 40 40))))


#-(and)
(defmethod view-draw-contents :after ((view instance-drawing-view))
  (let ((ui::*allow-print-backtrace* t))
    (ui::with-error-file
      (format *trace-output* "~&~S~%" `(view-draw-contents ,view))
      (format *trace-output* "~&~A = ~S~%" 'erase-region   (window-erase-region   (view-window view)))
      (format *trace-output* "~&~A = ~S~%" 'invalid-region (window-invalid-region (view-window view))))))


(defmethod view-click-event-handler ((view instance-drawing-view) where)
  (with-instance-drawing view
    (loop
      :with container := (view-window view)
      :with start-pt := where
      :while (still-down)
      :for old-pt := start-pt :then pt
      :for pt := (convert-coordinates (get-mouse) container view)
      :unless (eql pt old-pt)
        :do (drawing-instance view
              (with-pen-state (:pattern *gray-pattern*
                               :size #@(3 3)
                               :mode :patCopy)
                (draw-rect* (point-h start-pt) (point-v start-pt)
                            (- (point-h pt)
                               (point-h start-pt))
                            (- (point-v pt)
                               (point-v start-pt))))))))




(defclass instance-drawing-window (window)
  ())

(defmethod window-size-parts ((window instance-drawing-window))
  (let* ((size (view-size window))
         (h    (point-h size))
         (v    (point-v size)))
    (dovector (subview (view-subviews window))
      (typecase subview
        (static-text-dialog-item (set-view-size subview h 18))
        (instance-drawing-view   (set-view-size subview (- h 40) (- v 60)))))))


(eval-when (:compile-toplevel :execute)
  (defmacro source-directory ()
    #.(namestring (or *compile-file-truename*
                      *load-truename*
                      #P"./"))))

(defun run ()
  (initialize)
  (let ((win  (make-instance 'instance-drawing-window
                             :window-title "Example: Instance Drawing"
                             :view-size #@(512 532)))
        (path (merge-pathnames "../tests/test-picture-1--512x512.jpg"
                               (source-directory))))

    (make-instance 'static-text-dialog-item
                   :view-container win
                   :view-position #@(4 0)
                   :view-size #@(512 18)
                   :dialog-item-text "Click-and-drag rectangles!")

    (make-instance 'instance-drawing-view
                   :view-container win
                   :view-position (add-points  #@(0 20)    #@(20 20))
                   :view-size (subtract-points #@(512 512) #@(40 40))
                   :image-file path)

    (view-draw-contents win)
    win))

;;;; THE END ;;;;
