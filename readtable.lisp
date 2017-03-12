;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               readtable.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Define a macro to set up objcl+ccl readtable.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-02-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2016 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "MCLGUI.READTABLE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mclgui-readtable*
    (let ((rt (copy-readtable com.informatimago.objective-cl:*objective-cl-readtable*)))
      (set-dispatch-macro-character #\# #\@ (function mclgui::sharp-at-dispatch-reader-macro) rt)
      rt)))

(defmacro enable-objcl+ccl-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (copy-readtable *mclgui-readtable*))))

;;;; THE END ;;;;
