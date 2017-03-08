;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cocoa.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file loads Cocoa interfaces.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-05-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

;; We'll try to catch in this variable the objective-c reader macros
;; installed by ccl require cocoa.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar mclgui.readtable:*objc-readtable*
    (copy-readtable com.informatimago.objective-cl:*objc-readtable*)))

#+(and ccl darwin); for now, not on non-darwin
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable*
        #-(and ccl darwin)
        (copy-readtable nil)
        #+(and ccl darwin) ; #+ccl (require :cocoa) needs the botched readtable.
        (copy-readtable ccl::%initial-readtable%))
  (require :cocoa)
  (unless mclgui.readtable:*objc-readtable*
    (setf mclgui.readtable:*objc-readtable* (copy-readtable *readtable*)))
  (pushnew :objc-support *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

;;;; THE END ;;;;
