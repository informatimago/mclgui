;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;    
;;;;    A little view test.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-12 <PJB> Added this header.
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
(defpackage "MCLGUI.TEST.VIEW"
  (:use  "COMMON-LISP" "MCLGUI"))
(in-package "MCLGUI.TEST.VIEW")


(defclass box (view)
  ())

(defmethod ui::update-handle ((view view)) nil)

(defmethod view-draw-contents ((view box))
  (with-focused-view view
    (let ((size (view-size view)))
      (fill-rect* 0 0 (point-h size) (point-v size)))))

(defvar *w* nil)
(defun initialize/test-view ()
  (when *w* (window-close *w*))
  (setf *w* (make-instance 'window :window-title "Test")))


#-(and) (progn
          (initialize/test-view)
          (apply (function remove-subviews) *w* (coerce (view-subviews *w*) 'list))

          (add-subviews *w*
                        
                        (make-instance
                         'box
                         :view-position (make-point 20 10)
                         :view-size     (make-point 100 20))

                        (make-instance
                         'box
                         :view-position (make-point 20 30)
                         :view-size     (make-point 20 20)))
          );;progn
#-(and) (progn
          (view-subviews *w*)

          #((box :view-position (20 10) :position/window nil :view-size (100 20) :view-scroll-position (0 0) "#x302004B42F0D") (box :view-position (20 30) :position/window nil :view-size (20 20) :view-scroll-position (0 0) "#x302004B42D6D") (box :view-position (20 10) :position/window (40 20) :view-size (100 20) :view-scroll-position (0 0) "#x302004B4AF4D") (box :view-position (20 30) :position/window (40 60) :view-size (20 20) :view-scroll-position (0 0) "#x302004B4AE3D"))

          #((box :view-position (20 10) :position/window nil :view-size (100 20) :view-scroll-position (0 0) "#x302004B42F0D")
            (box :view-position (20 30) :position/window nil :view-size (20 20) :view-scroll-position (0 0) "#x302004B42D6D")
            (box :view-position (20 10) :position/window (40 20) :view-size (100 20) :view-scroll-position (0 0) "#x302004B4AF4D")
            (box :view-position (20 30) :position/window (40 60) :view-size (20 20) :view-scroll-position (0 0) "#x302004B4AE3D"))
          );;progn

;;;; THE END ;;;;
