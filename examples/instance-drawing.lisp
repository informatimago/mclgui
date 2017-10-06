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
    (draw-rect* (+ 40 (point-h where)) (point-v where)
                (+ 50 (point-h where)) (+ 20 (point-v where)))
    (with-pen-state (:pattern *gray-pattern*
                     :size #@(3 3)
                     :mode :patCopy)
      (draw-rect* (point-h where) (point-v where)
                  (+ 10 (point-h where)) (+ 20 (point-v where))))))

(defmethod view-draw-contents ((view instance-drawing-view))
  (call-next-method)
  (let ((ui::*allow-print-backtrace* t))
    (ui::with-error-file
      (format *trace-output* "~&~S~%" `(view-draw-contents ,view))
      (format *trace-output* "~&~A = ~S~%" 'erase-region   (window-erase-region   (view-window view)))
      (format *trace-output* "~&~A = ~S~%" 'invalid-region (window-invalid-region (view-window view)))))
  (y-or-n-p "Continue?"))

(defmethod view-click-event-handler ((view instance-drawing-view) where)
  (let ((container (view-window view)))
    (ui::with-error-file (format *trace-output* "~&where = ~S~%" (ui::point-to-list where)))
    (draw-test-rect view where)
    (with-instance-drawing view
      (loop
        :while (still-down)
        :for pt := (convert-coordinates (get-mouse) container view)
        :do (new-instance view)
            (with-focused-view view
              (with-pen-state (:pattern *gray-pattern*
                               :size #@(3 3)
                               :mode :patCopy)
                (draw-rect* (point-h where) (point-v where)
                            (point-h pt)    (point-v pt))))
        :finally (new-instance view)))))


(eval-when (:compile-toplevel :execute)
  (defmacro source-directory ()
    #.(namestring (or *compile-file-truename*
                      *load-truename*
                      #P"./"))))

(defun run ()
  (initialize)
  (let ((win  (make-instance 'window
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
                   :view-position #@(0 20)
                   :view-size #@(512 512)
                   :image-file path)

    win))

;;;; THE END ;;;;
