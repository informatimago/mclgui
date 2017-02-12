;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-rect.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;
;;;;    Test the rect functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-02 <PJB> Extracted from rect.lisp
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2015
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

(define-test test/interactive/rect-difference ()
  (let ((r1 (make-rect 0 0 200 100))
        (r2 (make-rect 1000 0 200 100))
        (r3 (make-rect 0 50 200 150))
        (r4 (make-rect 100 50 300 150))
        (r5 (make-rect 20 50 160 200))
        (r6 (make-rect 20 20 160 60))
        (colors (list *red-color* *green-color* *blue-color* *yellow-color*)))
    (loop :with view = (front-window)
          :for or :in (list r1 r2 r3 r4 r5 r6)
          :for diffs  = (rect-difference r1 or)
          :do (with-focused-view view
                (view-draw-contents view)
                (loop
                  :for r :in diffs
                  :for c :in colors
                  :do (with-fore-color c
                        (fill-rect* (rect-left r) (rect-top r) (rect-width r) (rect-height r))))
                (draw-rect* (rect-left r1) (rect-top r1) (rect-width r1) (rect-height r1))
                (draw-rect* (rect-left or) (rect-top or) (rect-width or) (rect-height or)))
              (sleep 3))))


(define-test test/point-to-angle ()
  (flet ((test (r x y angle)
           (assert-true (= angle (point-to-angle r x y))
                   ()
                   "rect = ~S ; point = ~S ; expected angle = ~S ; obtained angle = ~S~%"
                   (rect-to-list r)
                   (point-to-list (make-point x y))
                   angle
                   (point-to-angle r  x y))))

    (let ((r (make-rect 0 0 10 10)))
      (test r  5   0   0)
      (test r 10 -10  18)
      (test r 10   0  45)
      (test r 10   5  90)
      (test r 10  10 135)
      (test r  5  10 180)
      (test r  0  10 225)
      (test r  0   5 270)
      (test r  0   5 270)
      (test r  0   0 315)
      (test r  0  -5 333)
      (test r  5   5   0)                 ; degenerate: center
      (test (make-rect 0 0 0 0)  10 10 0) ; degenerate: empty rect
      (test (make-rect 0 0 0 10) 10 10 0) ; degenerate: empty flat rect
      (test (make-rect 0 0 10 0) 10 10 0) ; degenerate: empty flat rect
      )))


(define-test test/rect ()
  (test/point-to-angle))

;;;; THE END ;;;;
