;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-scroller.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;    
;;;;    Tests scrollers.
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
(in-package :ui)
;; (require 'quickdraw)


;;;;;;;;;;;;;;;;;;;;;;
;;
;;  a dialog with a scroller in it
;;


(defclass scroller1 (scroller) ())

(defmethod scroll-bar-limits ((view scroller1))
  (normal-scroll-bar-limits view 200 200))

(defmethod view-draw-contents ((self scroller1))
  (frame-rect self 10 10 50 50)
  (paint-oval self 30 30 200 200)
  (erase-oval self 30 30 70 70)
  (call-next-method))



(defvar *scroller-window-1* '())
(defun test/scroller/1 ()
  (let* ((window (make-instance 'dialog))
         ;; How to make the same thing with a scroller-pane 
         (pane (make-instance 'scroller-pane
                              :scroller-class 'scroller1
                              :view-size #@(125 125)
                              :view-position #@(150 0)
                              :track-thumb-p t
                              :view-container window)))
    (declare (ignorable pane))
    (when *scroller-window-1*
      (window-close *scroller-window-1*))
    (setq *scroller-window-1* window)))

#-(and) (progn
    (setq bar (make-instance 'scroller1
                            :view-container foo
                            :view-size #@(125 125)
                            :track-thumb-p t))
   (set-view-position bar 30 30)
   (set-view-position bar 00 00)
   (set-view-position bar 05 05)

   (set-view-size bar 150 150))


;;;;;;;;;;;;;;;;;;;;;;
;;
;;  nested scrollers
;;

(defclass scroller2 (scroller) ())

(defmethod scroll-bar-limits ((view scroller2))
  (normal-scroll-bar-limits view 300 300))

(defmethod view-draw-contents ((self scroller2))
  (frame-rect self 110 10 170 170)
  (call-next-method))

(defvar *scroller-window-2* nil)
(defun test/scroller/2 ()
  (let* ((dial (make-instance 'dialog))
         (first-scroller (make-instance 'scroller2
                                        :view-container dial
                                        :view-size #@(180 180)
                                        :view-position #@(5 5)
                                        :track-thumb-p t)))
    (when *scroller-window-2*
      (window-close *scroller-window-2*))
    (setf *scroller-window-2* dial)
    (values dial first-scroller)))




(defclass scroller3 (scroller) ())

(defmethod scroll-bar-limits ((view scroller3))
  (normal-scroll-bar-limits view 170 170))

(defmethod view-draw-contents ((self scroller3))
  (paint-oval self 10 10 70 70)
  (paint-oval self 70 70 170 170)
  (call-next-method))

(defun test/scroller/3 ()
  (multiple-value-bind (dial first-scroller) (test/scroller/2)
    (let ((second-scroller (make-instance 'scroller3
                                          :view-container first-scroller
                                          :view-size #@(75 155)
                                          :view-position #@(10 10)
                                          :track-thumb-p t)))
      (values dial first-scroller second-scroller))))


;;;;;;;;;;;;;;;;;;;;;;
;;
;;  scrollers with only one scroll bar
;;


(defclass scroller4 (scroller) ())

(defmethod scroll-bar-limits ((view scroller4))
  (normal-scroll-bar-limits view 200 200))

(defmethod view-draw-contents ((self scroller4))
  (frame-rect self 10 10 50 50)
  (paint-oval self 30 30 200 200)
  (erase-oval self 30 30 70 70)
  (call-next-method))

(defun test/scroller/4 ()
  (let* ((foo1 (make-instance 'dialog))
         (bar1 (make-instance 'scroller4 :grow-icon-p t
                                         :view-container foo1
                                         :h-scrollp nil)))
    (set-view-position bar1 50 50)
    (set-view-size bar1 150 150)
    (let* ((foo2 (make-instance 'dialog))
           (bar2 (make-instance 'scroller4
                                :view-container foo2
                                :v-scrollp nil)))
      (set-view-position bar2 50 50)
      (set-view-size bar2 125 125))))

;;;; THE END ;;;;


