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
  :version "1.1.4"
  :license "GPL3"
  :depends-on ("mclgui"
               "com.informatimago.common-lisp.cesarum")
  :components ((:file "tests/test-graphics"           :depends-on ())
               ;; (:file "tests/test-little"             :depends-on ())
               (:file "tests/test-menu"               :depends-on ())
               (:file "tests/test-pattern"            :depends-on ())
               (:file "tests/test-scroller"           :depends-on ())
               (:file "tests/test-view"               :depends-on ())
               (:file "tests/test-spring-view"        :depends-on ())
               (:file "tests/test-region-path"        :depends-on ())
               (:file "examples/coordinated-window"   :depends-on ())
               (:file "layout"                        :depends-on ())
               (:file "scratch/sdi"                   :depends-on ("layout"))
               (:file "tests/test-region-interactive" :depends-on ("examples/coordinated-window"
                                                                   "scratch/sdi"
                                                                   "tests/test-region-path"))
               (:file "tests/test-text-edit"          :depends-on ())))

;;;; THE END ;;;;
