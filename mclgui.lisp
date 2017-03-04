;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines general MCLGUI functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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



(defun initialize/screen ()
  (multiple-value-bind (sx sy sw sh) (main-screen-frame)
    (declare (ignore sx sy))
    (setf *screen-width*   sw
          *screen-height*  sh))
  (values))


(defvar *initialized* nil)

(defun initialize ()
  "Initialize the MCL GUI."
  (unless *initialized*
    (progn
      (ui::reporting-errors (mclgui.wrapper::initialize/wrapper))
      (ui::reporting-errors (initialize-event-environment-bindings))
      (ui::reporting-errors (initialize/process))
      (ui::reporting-errors (initialize/application))
      (ui::reporting-errors (initialize/paragraph-style))
      (ui::reporting-errors (initialize/screen))
      (ui::reporting-errors (initialize/region))
      (ui::reporting-errors (initialize/color))
      (ui::reporting-errors (initialize/pattern))
      (ui::reporting-errors (initialize/pen))
      (ui::reporting-errors (initialize/cursor))
      (ui::reporting-errors (initialize/scrap))
      (ui::reporting-errors (initialize/font))
      (ui::reporting-errors (initialize/menu))
      (ui::reporting-errors (initialize/view))
      (ui::reporting-errors (initialize/window))
      (ui::reporting-errors (te-init))
      (ui::reporting-errors (initialize/table-dialog-item))
      (ui::reporting-errors (initialize/file))
      (ui::reporting-errors (initialize/event))
      (ui::reporting-errors (initialize/eval))
      (ui::reporting-errors (initialize/pop-up-menu-dialog-item)))

    #-(and) (progn
              (initialize-event-environment-bindings)
              (initialize/process)
              (initialize/application)
              (initialize/paragraph-style)
              (initialize/screen)
              (initialize/region)
              (initialize/color)
              (initialize/pattern)
              (initialize/pen)
              (initialize/cursor)
              (initialize/scrap)
              (initialize/font)
              (initialize/menu)
              (initialize/view)
              (initialize/window)
              (te-init)
              (initialize/table-dialog-item)
              (initialize/file)
              (initialize/event)
              (initialize/eval)
              (initialize/pop-up-menu-dialog-item)

              #+has-appleevent (when (fboundp 'initialize/apple-event)
                                 (initialize/apple-event))
              )
    (setf *initialized* t))
  (values))


;;; --- ccl repl

(defun safe-repl (&rest arguments &key &allow-other-keys)
  (loop
    (handler-bind ((error (function invoke-debugger)))
      (apply (function ccl::read-loop) arguments))))

(on-restore ccl-repl
  (setf ccl::*read-loop-function* 'safe-repl
        ccl::*inhibit-greeting*    t))

;;; --- trace output saved to file.

(defvar *patchwork-trace-output* *trace-output*)

(defun redirect-trace-output-to-file (pathname)
  (setf *patchwork-trace-output* (open pathname
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :append
                                       #+ccl :sharing #+ccl :lock))
  (setf *trace-output* *patchwork-trace-output*)
  (setf (ui::aget ui::*event-environment-bindings* '*trace-output*)
        *patchwork-trace-output*)
  (format *trace-output* "~%~A~2%" (date)))

(on-restore patchwork-trace
  (ui::reporting-errors
    (redirect-trace-output-to-file (merge-pathnames #P"Desktop/Patchwork-trace.txt"
                                                    (user-homedir-pathname)))
    (format-trace "Welcome to the Machine!")
    ;; (com.informatimago.common-lisp.interactive.interactive:repl)
    ))

;;; --------------------------------------------------------------------
;;; Initialization of patchwork

;; (on-startup patchwork-initialization
;;   (ui::reporting-errors
;;     ;; in ccl-1.11, ccl::*application* is still nil here.
;;     (let ((ccl::*application* (or ccl::*application* t)))
;;       (eval-enqueue '(initialize-patchwork)))))
;;
;; (setf (symbol-function 'patchwork-initialization)
;;       (lambda nil (block patchwork-initialization
;;                     (let ((ccl::*application*
;;                             (or ccl::*application* t)))
;;                       (eval-enqueue '(initialize-patchwork))))))

;;;; THE END ;;;;
