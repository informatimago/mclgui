;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-region-pattern.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;
;;;;    Draws a region with a pattern.
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
(defpackage "MCLGUI.TEST.REGION-PATTERN"
  (:use "COMMON-LISP" "MCLGUI" "MCLGUI.EXAMPLE.COMPUTED-VIEW")
  (:export "TEST/REGION-PATTERN"))
(in-package "MCLGUI.TEST.REGION-PATTERN")


(defun test/region-pattern ()
  (let ((win (make-instance 'window
                            :window-title "Test Region Pattern"
                            :view-size #@(400 300))))
    (make-instance 'computed-view
                   :view-container win
                   :drawn-as (let ((region (offset-region (xor-region (reduce (function union-region)
                                                                              (list (rect-region 0 0 100 20)
                                                                                    (rect-region 0 0 20 80)
                                                                                    (rect-region 80 0 100 80)))
                                                                      (disc-region 50 -20 30))
                                                          50 100)))
                               (lambda (view)
                                 (with-pen-state (:pattern *dark-gray-pattern*)
                                   (erase-rect* 50 100 200 200)
                                   (fill-rect* 80 130 40 40)
                                   (fill-region view *gray-pattern* region)))))
    win))

;; (test/region-pattern)

;;;; THE END ;;;;


