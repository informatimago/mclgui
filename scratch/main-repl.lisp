;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               main-repl.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A REPL in the main thread.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-12-28 <PJB> Created.
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
(defpackage "REPL-WINDOW"
  (:use "COMMON-LISP"
        "UI"
        "COM.INFORMATIMAGO.CLEXT.PIPE"))
(in-package "REPL-WINDOW")

#|

With pipes, the repl would run in a background thread, but we want to
run the repl in the main thread!

|#

(defun make-listener (&optional (name "Listener"))
  (let* ((keyboard-pipe (make-pipe :name (concatenate 'string name "-keyboard")))
         (screen-pipe   (make-pipe :name (concatenate 'string name "-screen")
                                   :buffer-size 4096))
         (stdin     (pipe-input-stream keyboard-pipe))
         (stdout    (pipe-output-stream screen-pipe))
         (to-repl   (pipe-output-stream keyboard-pipe))
         (from-repl (pipe-input-stream screen-pipe))

         )
    ))

(defun create-repl-views (window)
  (let* ((width  (point-h (view-size window)))
         (height (point-v (view-size window)))
         (x      (- width  40))
         (y      (- height 30))
         (output (make-instance
                  'static-text-dialog-item
                  :dialog-item-text ""
                  :view-position (make-point 0 0)
                  :view-size     (make-point width (- y 2))
                  :view-nick-name 'repl-output
                  :view-container window))
         (source (make-instance
                  'editable-text-dialog-item
                  :dialog-item-text ""
                  :dialog-item-action (lambda (source)
                                        (evaluate source output))
                  :view-position (make-point 2 (+ y 2))
                  :view-size     (make-point (- x 2) (- 30 4))
                  ::view-nick-name 'repl-input
                  :view-container window))
         (button  (make-instance
                   'button-dialog-item
                   :view-position (make-point (+ x 2) (+ y 2))
                   :view-size     (make-point (- 40 4) (- 30 4))
                   :dialog-item-action (lambda (button)
                                         (declare (ignore button))
                                         (evaluate source output))
                   :dialog-item-text "EVAL"
                   :view-nick-name 'repl-eval
                   :view-container window)))
    (list output source button)))



(defclass repl-window (window)
  ())

(defmethod initialize-instance :after ((self repl-window) &key &allow-other-keys)
  (create-repl-views self))


(on-main-thread (make-instance 'repl-window :window-title "REPL"
                                            :view-size (make-point 300 200)))

#-(and) (progn
         (window-close (front-window)))
