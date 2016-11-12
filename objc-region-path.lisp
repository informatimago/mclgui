;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-region-path.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines low level graphic primitives for regions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-09-11 <PJB> Added this header.
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
(objcl:enable-objcl-reader-macros)

(defun bezier-path-from-region (region)
  (flet ((tpoint-to-point (tpoint)
           (ns:make-ns-point (tpoint-x tpoint) (tpoint-y tpoint))))
    (declare (inline tpoint-to-point))
    (let ((tpaths (paths-from-region region))
          (path [NSBezierPath bezierPath]))
      [path setLineCapStyle:#$NSSquareLineCapStyle]
      ;; [path setLineJoinStyle:#$NSRoundLineJoinStyle]
      [path setLineJoinStyle:#$NSBevelLineJoinStyle]
      (loop
        :for tpath :in tpaths
        :for start = (tpath-lines tpath)
        :do (let ((first t))
              (tline-dolines (line start)
                (when first
                  [path moveToPoint:(tpoint-to-point (tline-from-point line))]
                  (setf first nil))
                [path lineToPoint:(tpoint-to-point (tline-to-point line))])))
      [path closePath]
      path)))


;;;; THE END ;;;;
