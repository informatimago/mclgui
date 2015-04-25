;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-pattern.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests patterns.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-03-02 <PJB> Added this header.
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

(define-test test/pattern ()
  (assert-true (equalp (cursor-premultiplied-data
                        (make-instance 'cursor
                                       :data (make-array '(16 16)
                                                         :element-type 'bit
                                                         :initial-contents '(#*0000000010000000
                                                                             #*1111111111111111
                                                                             #*0000000010000000
                                                                             #*0000000010000000
                                                                             
                                                                             #*0000000010000000
                                                                             #*0000000010000000
                                                                             #*0000000010000000
                                                                             #*0000000010000000

                                                                             #*1111111111111111
                                                                             #*1111111111111111
                                                                             #*1111111111111111
                                                                             #*1111111111111111

                                                                             #*1111111111111111
                                                                             #*1111111111111111
                                                                             #*1111111111111111
                                                                             #*1111111111111111
                                                                             ))
                                       :mask (make-array '(16 16)
                                                         :element-type 'bit
                                                         :initial-contents '(#*1111111111111111
                                                                             #*1111111111111111
                                                                             #*1111111111111111
                                                                             #*0000000111000000

                                                                             #*0000000111000000
                                                                             #*0000000111000000
                                                                             #*0000000111000000
                                                                             #*0000000111000000

                                                                             #*0000000111000000
                                                                             #*0000000000000000
                                                                             #*0000000000000000
                                                                             #*0000000000000000

                                                                             #*0000000000000000
                                                                             #*0000000000000000
                                                                             #*0000000000000000
                                                                             #*0000000000000000
                                                                             ))))
                       
                       (make-array '(16 16)
                                   :element-type 'bit
                                   :initial-contents '(#*0000000010000000
                                                       #*1111111111111111
                                                       #*0000000010000000
                                                       #*0000000010000000
                                                       #*0000000010000000
                                                       #*0000000010000000
                                                       #*0000000010000000
                                                       #*0000000010000000
                                                       #*0000000111000000
                                                       #*0000000000000000
                                                       #*0000000000000000
                                                       #*0000000000000000
                                                       #*0000000000000000
                                                       #*0000000000000000
                                                       #*0000000000000000
                                                       #*0000000000000000)))))

;;;; THE END ;;;;
