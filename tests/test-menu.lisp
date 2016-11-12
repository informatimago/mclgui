;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-menu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;
;;;;    Test the menus.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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



(define-test test/menu/encode-key-mask ()
  (assert-true (equal (decode-key-mask (encode-key-mask (decode-key-mask #xffff0000)))
                      '(:alpha-shift :shift :control :option :command
                        :numeric-pad :help :function)))
  (assert-true (equal (decode-key-mask (encode-key-mask '(:shift :control :command)))
                      '(:shift :control :command)))
  (assert-true (equal (decode-key-mask (encode-key-mask '(:command :shift)))
                      '(:shift :command)))
  (assert-true (equal (decode-key-mask (encode-key-mask :shift))
                      :shift))
  (assert-true (equal (decode-key-mask (encode-key-mask '(:shift)))
                      :shift))
  (assert-true (equal (decode-key-mask 0)
                      nil)))


(define-test test/menu ()
  (test/menu/encode-key-mask))

;;;; THE END ;;;;
