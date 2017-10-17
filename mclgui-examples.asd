;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui-examples.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    Loads the examples of MCLGUI.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-03-12 <PJB> Created.
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

(asdf:defsystem :mclgui-examples
  :name "mclgui-examples"
  :description "Loads the examples of the Macintosh Common Lisp Graphical User Interface for OpenStep"
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "GPL3"
  :depends-on ("mclgui"
               "com.informatimago.common-lisp.cesarum")
  :components ((:file "examples/computed-view")
               (:file "examples/coordinated-window")
               (:file "examples/slowatch")
               (:file "examples/picture-view")
               (:file "examples/instance-drawing"
                      :depends-on ("examples/picture-view")))

  ;; :perform (asdf:prepare-op
  ;;           :after (operation system) (declare (ignore operation system))
  ;;           (ui:initialize))

  :perform (asdf:test-op
            (operation system) (declare (ignore operation system))
            (dolist (p '("MCLGUI.EXAMPLE.COMPUTED-VIEW"
                         "MCLGUI.EXAMPLE.COORDINATES-WINDOW"
                         "MCLGUI.EXAMPLE.SLOWATCH"
                         "MCLGUI.EXAMPLE.INSTANCE-DRAWING"))
              (let ((pack (find-package p)))
                (if pack
                    (let ((*package* pack))
                      #+asdf3 (uiop:symbol-call pack "RUN")
                      #-asdf3 (let ((fname (find-symbol "RUN" pack)))
                                (if fname
                                    (funcall fname)
                                    (warn "Cannot find symbol RUN in package ~A" p))))
                    (warn "Cannot find package ~A" p))))))

;;;; THE END ;;;;
