;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-clip.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Test cliping.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-30 <PJB>
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
(defpackage "MCLGUI.TEST.CLIP"
  (:use  "COMMON-LISP" "MCLGUI")
  (:export
   "CLIP-VIEW"
   "CLIP-VIEW-REGION"
   "CLIP-VIEW-COLOR"
   "CLIP-WINDOW"
   "TEST/CLIP-IN-WINDOW"))
(in-package "MCLGUI.TEST.CLIP")


(defclass clip-view (view)
  ((clip-region :initform nil          :initarg :clip-region :accessor clip-view-region
                :documentation "In the coordinates system of the clip-view.")
   (clip-color  :initform *gray-color* :initarg :clip-color  :accessor clip-view-color)))

(defun print-clip (window)
  (let ((rgn            (new-region))
        (*current-view* window))
    (ui::get-clip rgn)
    (print `(ui::get-clip -> ,rgn)))
  (print `(clip-region -> ,(clip-region window)))
  (print `(ui::view-clip-region-slot -> ,(ui::view-clip-region-slot window))))

(defmethod view-draw-contents ((view clip-view))
  (flet ((draw ()
           (with-focused-view view      ; into view coords
             (let ((bounds (view-bounds view)))
               (with-fore-color (clip-view-color view)
                 (fill-rect* (rect-left bounds) (rect-top bounds) (rect-width bounds) (rect-height bounds)))
               (with-fore-color *black-color*
                 (draw-rect* (rect-left bounds) (rect-top bounds) (rect-width bounds) (rect-height bounds)))))))
    (let ((region  (clip-view-region view)))
      (if region
          (with-clip-region region
            (draw))
          (draw))))
  (call-next-method))

(defclass clip-window (ui::coordinated-window)
  ())

(defmethod view-draw-contents ((win clip-window))
  (let ((bounds (view-bounds win)))
    (with-focused-view win
      (with-fore-color *yellow-color*
        (fill-rect* (rect-left bounds) (rect-top bounds) (rect-width bounds) (rect-height bounds))))
    (call-next-method)))


(defun test/clip-in-window ()
  (let* ((win    (make-instance 'clip-window
                                :view-size #@(300 200)
                                :window-title "Test Clip Window"))
         (v1 (make-instance 'clip-view
                            :view-position #@(20 10)
                            :view-size #@(51 41)
                            :clip-region (set-disc-region (new-region) 30 0 50)
                            :clip-color *blue-color*
                            :view-container win))
         (v2  (make-instance 'clip-view
                             :view-position #@(20 60)
                             :view-size #@(200 160)
                             :clip-region nil
                             :clip-color *white-color*
                             :view-container win))
         (v21 (make-instance 'clip-view
                             :view-position #@(10 10)
                             :view-size #@(52 42)
                             :clip-region nil
                             :clip-color *green-color*
                             :view-container v2))
         (v22 (make-instance 'clip-view
                             :view-position #@(70 10)
                             :view-size #@(53 43)
                             :clip-region nil
                             :clip-color *red-color*
                             :view-container v2))
         (v221 (make-instance 'clip-view
                              :view-position #@(10 10)
                              :view-size #@(24 14)
                              :clip-region nil
                              :clip-color *orange-color*
                              :view-container v22)))
    (declare (ignorable v1 v2 v21 v22 v221))
    win))


#-(and) (progn
          (in-package "MCLGUI.TEST.CLIP")
          (on-main-thread (test/clip-in-window))

          (clip-view-color (elt (view-subviews (front-window)) 0))


          )

(in-package :ui)

#-(and)(progn

         (pprint (maptree (lambda (node)
                            (list (point-to-list (view-position node))
                                  (point-to-list (view-scroll-position node))
                                  (point-to-list (view-origin node))
                                  (point-to-list (convert-coordinates 0 node (view-window node)))
                                  (view-frame node)
                                  (view-clip-region node)))
                          (subview-tree (front-window))))

         (((14 45) (0 0) (0 0) (0 0) (rect :left 14 :top 45 :right 335 :bottom 445)
           #S(region :bounds (rect :left 0 :top 0 :right 321 :bottom 400)
                     :segments #1=#()))
          (((58 307) (0 0) (-58 -307) (58 307)
            (rect :left 58 :top 307 :right 166 :bottom 323)
            #S(region :bounds (rect :left 0 :top 0 :right 108 :bottom 16)
                      :segments #1#)))
          (((20 10) (0 0) (-20 -10) (20 10) (rect :left 20 :top 10 :right 70 :bottom 50)
            #S(region :bounds (rect :left 0 :top 0 :right 50 :bottom 40)
                      :segments #())))
          (((20 60) (0 0) (-20 -60) (20 60)
            (rect :left 20 :top 60 :right 220 :bottom 220)
            #S(region :bounds (rect :left 0 :top 0 :right 200 :bottom 160)
                      :segments #1#))
           (((10 10) (0 0) (-30 -70) (30 70)
             (rect :left 10 :top 10 :right 60 :bottom 50)
             #S(region :bounds (rect :left 0 :top 0 :right 50 :bottom 40)
                       :segments #1#)))
           (((70 10) (0 0) (-90 -70) (90 70)
             (rect :left 70 :top 10 :right 120 :bottom 50)
             #S(region :bounds (rect :left 0 :top 0 :right 50 :bottom 40)
                       :segments #1#))
            (((10 10) (0 0) (-100 -80) (100 80)
              (rect :left 10 :top 10 :right 30 :bottom 20)
              #S(region :bounds (rect :left 0 :top 0 :right 20 :bottom 10)
                        :segments #1#))))))

         (list (- 90 70) (- 60 50))
         (20 10)
         (list (- 240 220) (- 280 220))
         (20 60)

         )


