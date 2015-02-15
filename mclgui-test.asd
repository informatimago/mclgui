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
  :version "1.1.3"
  :license "GPL3"
  :depends-on ("mclgui"
               "com.informatimago.common-lisp.cesarum")
  :components ((:file "tests/test-graphics"           :depends-on nil)
               ;; (:file "tests/test-little"             :depends-on nil)
               (:file "tests/test-menu"               :depends-on nil)
               (:file "tests/test-pattern"            :depends-on nil)
               (:file "tests/test-scroller"           :depends-on nil)
               (:file "tests/test-view"               :depends-on nil)
               (:file "tests/test-spring-view"        :depends-on nil)
               (:file "tests/test-region-path"        :depends-on nil)
               (:file "examples/coordinated-window"   :depends-on nil)
               (:file "layout"                        :depends-on ())
               (:file "scratch/sdi"                   :depends-on ("layout"))
               (:file "tests/test-region-interactive" :depends-on ("examples/coordinated-window"
                                                                   "scratch/sdi"
                                                                   "tests/test-region-path"))
               #-(and) (:file "tests"
                        :depends-on ("tests/test-graphics"           
                                     "tests/test-menu"               
                                     "tests/test-pattern"            
                                     "tests/test-scroller"           
                                     "tests/test-view"               
                                     "tests/test-spring-view"        
                                     "tests/test-region-path"        
                                     "examples/coordinated-window"   
                                     "scratch/sdi"                   
                                     "tests/test-region-interactive"))))

;;;; THE END ;;;;
