;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               picture-view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A picture-view draws a picture read from a file.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-03 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
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

(defpackage "MCLGUI.EXAMPLE.PICTURE-VIEW"
  (:use "COMMON-LISP" "MCLGUI")
  (:export "PICTURE-VIEW"))
(in-package "MCLGUI.EXAMPLE.PICTURE-VIEW")
(enable-sharp-at-reader-macro)

(defclass picture-view (view)
  ((image :reader picture-view-image)
   (file  :reader picture-view-image-file)))

(defmethod initialize-instance :after ((view picture-view) &key image-file &allow-other-keys)
  (when image-file
    (setf (picture-view-image-file view) image-file)))

(defmethod (setf picture-view-image-file) (image-file (view picture-view))
  (check-type image-file (or string pathname file-stream))
  (let ((path (pathname image-file)))
    (setf (slot-value view 'image) (load-image path)
          (slot-value view 'file)  path)))

(defmethod view-draw-contents ((view picture-view))
  (call-next-method)
  (with-focused-view view
    (draw-image (picture-view-image view)
                (view-bounds view))))

;;;; THE END ;;;;
