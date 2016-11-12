;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               scrap.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Scrapbook, ie. clipboard.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-11 <PJB> Implemented with NSPasteboard.
;;;;    2012-05-18 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
;;;;
;;;;    Some code extracted from MCL (LGPL):
;;;;    Copyright 1985-1988 Coral Software Corp.
;;;;    Copyright 1989-1994 Apple Computer, Inc.
;;;;    Copyright 1995-2000 Digitool, Inc.
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

(defclass scrap-handler ()
  ((pasteboard :initarg :pasteboard :reader pasteboard)))

(defvar *scrap-handler* nil)
(defvar *nspasteboardtypestring* nil)

(defun get-scrap (scrap-type)
  "

RETURN:         Two values: The first value is the current scrap of
                SCRAP-TYPE.  The second value is T if some scrap is
                found or NIL if no scrap is found.

                The GET-SCRAP function looks up the scrap handler for
                SCRAP-TYPE and calls GET-INTERNAL-SCRAP with the
                handler.  Before calling GET-INTERNAL-SCRAP, GET-SCRAP
                checks to see whether data needs to be imported from
                the external Macintosh system scrap.

SCRAP-TYPE:     A scrap type. In the initial MCL environment, the
                three predefined scrap types are :TEXT, :FRED, and
                :LISP.  The file pict-scrap.lisp in your Examples
                folder adds the :PICT type.
"
  (check-type scrap-type (member :text :fred :lisp))
  (get-internal-scrap *scrap-handler*))


(defun zero-scrap ()
  "
DO:     clear previous contents of desk scrap.
"
  [(pasteboard *scrap-handler*) clearContents])


(defun put-scrap (scrap-type scrap-value &optional (overwrite-p t))
  "
DO:             Store SCRAP-VALUE in the scrap, as type SCRAP-TYPE.
                If the value of OVERWRITE-P is true (the default),
                then all other entries (of any type) in the scrap are
                cleared; if the value of OVERWRITE-P is false, scrap
                entries of other types are not cleared.  The PUT-SCRAP
                function works by looking up the scrap handler for
                scrap-type and calling SET-INTERNAL-SCRAP with the
                handler and scrap value.

                The PUT-SCRAP function pushes SCRAP-TYPE onto the
                *SCRAP-STATE* list and sets the variable @ to
                SCRAP-VALUE.

SCRAP-TYPE:     A scrap type. In the initial MCL environment, the three
                predefined scrap types are :text, :fred, and :lisp.
                The file pict-scrap.lisp in your Examples folder
                adds the :pict type.

SCRAP-VALUE:    The value of the new scrap: that is, what is stored in
                the scrap. This should be in a format compatible with
                SCRAP-TYPE.

OVERWRITE-P:    A Boolean variable indicating whether scrap values of
                other types should be cleared. The default value is
                true, which clears all other types from the scrap.
"
  (check-type scrap-type (member :text :fred :lisp))
  (check-type scrap-value string)
  (when overwrite-p
    (zero-scrap))
  (set-internal-scrap *scrap-handler* scrap-value))


(defgeneric get-internal-scrap (handler)
  (:documentation "
RETURN:         the value of the scrap of a given type. This function
                is called by get-scrap.
")
  (:method ((handler scrap-handler))
    (objcl:lisp-string [(pasteboard handler) stringForType:*nspasteboardtypestring*])))


(defgeneric set-internal-scrap (handler value)
  (:documentation "
DO:             Set the value of the scrap of a given type.  This
                function is called by put-scrap.
")
  (:method ((handler scrap-handler) value)
    [(pasteboard handler) setString: (objcl:objc-string value) forType:*nspasteboardtypestring*]
    value))


(defgeneric internalize-scrap (handler)
  (:documentation "
The INTERNALIZE-SCRAP generic function converts the scrap from
external to internal format.  This function is called when the user
switches into Macintosh Common Lisp from another application or from a
desk accessory.  The function retrieves data from the Macintosh system
heap using the appropriate system calls and then calls
SET-INTERNAL-SCRAP on the result.
")
  (:method ((handler scrap-handler))
    (values)))


(defgeneric externalize-scrap (handler)
  (:documentation "
The EXTERNALIZE-SCRAP generic function converts the scrap from
internal to external format.  This function is called when the user
switches from Macintosh Common Lisp to another application or to a
desk accessory.  The function copies data to the Macintosh system heap
using the appropriate system calls.  The default method for
SCRAP-HANDLER does nothing.
")
  (:method ((handler scrap-handler))
    (values)))


(defun initialize/scrap ()
  (setf *nspasteboardtypestring* #$NSPasteboardTypeString
        *scrap-handler* (make-instance 'scrap-handler
                                       :pasteboard  [NSPasteboard generalPasteboard])))


#-(and) (
         (get-scrap :text)
         (put-scrap :text "Hello World")
         )

;;;; THE END ;;;;
