;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui-test.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    Defines the MCLGUI Test asdf system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-12 <PJB> Created.
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

(asdf:defsystem :mclgui-test
  :name "mclgui-test"
  :description "Test system for Macintosh Common Lisp Graphical User Interface for OpenStep"
  :author "Pascal J. Bourguignon"
  :version "1.1.5"
  :license "GPL3"
  :depends-on ("mclgui"
               "com.informatimago.common-lisp.cesarum")
  :components (
               ;; ---
               ;; automatic tests:
               (:file "tests/test-rect"               :depends-on ())
               (:file "tests/test-region"             :depends-on ())
               (:file "tests/test-region-path"        :depends-on ())
               (:file "tests/test-pattern"            :depends-on ())
               (:file "tests/test-menu"               :depends-on ())
               ;; ---
               (:file "tests/test-graphics"           :depends-on ())
               ;; (:file "tests/test-little"             :depends-on ())
               ;; ---
               ;; later we'll make an mclgui-example system:
               (:file "examples/coordinated-window"   :depends-on ())
               (:file "examples/computed-view"        :depends-on ())
               (:file "layout"                        :depends-on ())
               (:file "scratch/sdi"                   :depends-on ("layout"))
               (:file "scratch/dump"                  :depends-on ())
               ;; ---
               (:file "tests/test-clip"               :depends-on ("examples/coordinated-window" "scratch/dump"))
               (:file "tests/test-scroller"           :depends-on ())
               (:file "tests/test-view"               :depends-on ())
               (:file "tests/test-spring-view"        :depends-on ())
               (:file "tests/test-text-edit"          :depends-on ())
               (:file "tests/test-region-interactive" :depends-on ("examples/coordinated-window"
                                                                   "scratch/sdi"
                                                                   "tests/test-region-path"))
               (:file "tests/test-region-pattern"     :depends-on ("examples/computed-view"))
               ;; ---
               (:file "tests/test" :depends-on ("tests/test-rect"
                                                "tests/test-region"
                                                "tests/test-region-path"
                                                "tests/test-pattern"
                                                "tests/test-menu")))
  :perform (asdf:prepare-op
            :after (operation system) (declare (ignore operation system))
            (ui:initialize))

  :perform (asdf:test-op
            (operation system) (declare (ignore operation system))
            (ui:initialize)
            (dolist (p '("MCLGUI"))
              (let ((*package* (find-package p)))
                #+asdf3 (uiop:symbol-call p "TEST/ALL")))))

;;;; THE END ;;;;
