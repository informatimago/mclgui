;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ccl-patches.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Patches startup-ccl, so that if init-file is a list, all the
;;;;    files in the list are loaded instead of a single one.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-08-04 <PJB> Added this header.
;;;;BUGS
;;;;    * this is not enough for the wanted feature of loading
;;;;      init files in some order.
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

(in-package "CCL")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *saved-warn-if-redefine-kernel* ccl::*warn-if-redefine-kernel*)
  (setf ccl::*warn-if-redefine-kernel* nil))

(defun startup-ccl (&optional init-file)
  ;; Many of the things done here could enter a break loop on error.
  ;; If that break loop is exited via :q, quietly exit to here.
  (catch :toplevel
    (with-simple-restart (abort "Abort startup.")
      (let ((init-files (if (listp init-file) init-file (list init-file))))
        (dolist (init-file init-files)
          (with-simple-restart (continue "Skip loading init file.")
            ;; Don't return on success, but keep loading all the init-files provided.
            (load init-file :if-does-not-exist nil :verbose nil))))
      (flet ((eval-string (s)
               (with-simple-restart (continue "Skip evaluation of ~a" s)
                 (eval (read-from-string s))))
             (load-file (name)
               (with-simple-restart (continue "Skip loading ~s" name)
                 (load name))))
        (dolist (p *lisp-startup-parameters*)
          (let* ((param (cdr p)))
            (case (car p)
              (:gc-threshold
               (multiple-value-bind (n last) (parse-integer param :junk-allowed t)
                 (when n
                   (if (< last (length param))
                       (case (schar param last)
                         ((#\k #\K) (setq n (ash n 10)))
                         ((#\m #\M) (setq n (ash n 20)))))
                   (set-lisp-heap-gc-threshold n)
                   (use-lisp-heap-gc-threshold))))
              (:eval (eval-string param))
              (:load (load-file param)))))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf ccl::*warn-if-redefine-kernel* *saved-warn-if-redefine-kernel*))

;;;; THE END ;;;;
