;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-region.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Test regions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-04-25 <PJB> Added this header.
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
(in-package "MCLGUI")


(define-test test/segments-operate ()

  (check equalp (loop
                  with s = (make-array 10 :adjustable t :fill-pointer 0)
                  with w = (segment-writer s)
                  for (a b) in '((1 2) (3 4) (4 5) (6 7))
                  do (funcall w a b)
                  finally (return s))
         #(1 2 3 5 6 7))

  (check equalp (loop with r = (segment-reader #(1 3 5 7 9 10 11 13 14 15))
                      with c = 0 with v = 0
                      do (multiple-value-setq (c v) (funcall r))
                      collect (list c v)
                      while c)
         '((1 nil) (3 t) (5 nil) (7 t) (9 nil) (10 t) (11 nil)
           (13 t) (14 nil) (15 t) (nil nil)))

  (check equalp (segments-operate (lambda (a b) (and a b))
                                  #(1 3 5 7 9 10 11 13 14 15)
                                  #(0 2 4 6 8 11 12 13))
         #(1 2 5 6 9 10 12 13))

  (check equalp (segments-operate (lambda (a b) (or a b))
                                  #(1 3 5 7 9 10 11 13 14 15)
                                  #(0 2 4 6 8 11 12 13))
         #(0 3 4 7 8 13 14 15))

  (check equalp (segments-operate (lambda (a b) (or (and a (not b)) (and (not a) b)))
                                  #(1 3 5 7 9 10 11 13 14 15)
                                  #(0 2 4 6 8 11 12 13))
         #(0 1 2 3 4 5 6 7 8 9 10 12 14 15))

  (check equalp (segments-operate (lambda (a b) (and a (not b)))
                                  #(1 3 5 7 9 10 11 13 14 15)
                                  #(0 2 4 6 8 11 12 13))
         #(2 3 6 7 11 12 14 15))

  (check equalp (segments-operate (lambda (a b) (and a (not b)))
                                  #(62 138)
                                  #(85 115))
         #(62 85 115 138))

  (check equalp (segments-operate (lambda (a b) (and a (not b)))
                                  #(62 138) #())
         #(62 138))

  (check equalp (segments-operate (lambda (a b) (and a (not b)))
                                  #() #(62 138))
         #()))


(define-test test/update-segments ()
  (check equalp (loop
                  :for invpt :across #(#( 0   7    23)
                                       #( 2 0 7    23 30)
                                       #( 4   7 17      )
                                       #( 6   7 17 27 30)
                                       #( 8 0   17 23 27)
                                       #(10     17 23   ))
                  :for s = (subseq invpt 1) :then (update-segments s invpt)
                  :collect (cons (aref invpt 0) s))
         '((0 . #(7 23))
           (2 . #(0 30))
           (4 . #(0 7 17 30))
           (6 . #(0 27))
           (8 . #(17 23))
           (10 . #()))))


(define-test test/segments-trim ()
  (check equalp (segments-trim #((10 . #(10 100))
                                 (20 . #())))
         #((10 . #(10 100))
           (20 . #())))
  (check equalp (segments-trim #((0 . #())
                                 (1 . #())
                                 (10 . #(10 100))
                                 (50 . #(10 100 200 400))
                                 (60 . #(10 50 350 400))
                                 (80 . #(10 100 200 400))
                                 (100 . #(200 400))
                                 (150 . #())
                                 (200 . #())
                                 (201 . #())))
         #((10 . #(10 100))
           (50 . #(10 100 200 400))
           (60 . #(10 50 350 400))
           (80 . #(10 100 200 400))
           (100 . #(200 400))
           (150 . #()))))


(define-test test/region-operate ()
  (check equalp (region-operate :intersection
                                (set-rect-region (new-region) 0 0 200 100)
                                (set-rect-region (new-region) 50 20 250 120)
                                (new-region))
         #S(region :bounds #S(rect :topleft 1310770 :bottomright 6553800)
                   :segments #()))
  (check equalp (region-operate
                 :difference
                 (region-operate
                  :union
                  (set-rect-region (new-region) 10 10 100 100)
                  (set-rect-region (new-region) 200 50 400 150)
                  (new-region))
                 (set-rect-region (new-region) 50 60 350 80)
                 (new-region))
         #S(region :bounds #S(rect :topleft 655370 :bottomright 9830800)
                   :segments #((10 . #(10 100))
                               (50 . #(10 100 200 400))
                               (60 . #(10 50 350 400))
                               (80 . #(10 100 200 400))
                               (100 . #(200 400))
                               (150 . #())))))


(define-test test/xor-region ()
  (let ((circle (set-disc-region (new-region) 0 0 10)))
    (assert-true (equal-region-p
                  (xor-region circle (inset-region (copy-region circle) 1 1) (new-region))
                  (xor-region (inset-region (copy-region circle) 1 1) circle (new-region))))))


(define-test test/region ()
  (test/segments-operate)
  (test/segments-trim)
  ;; (test/update-segments)
  (test/region-operate)
  (test/xor-region))


;;;; THE END ;;;;
