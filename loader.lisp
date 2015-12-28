;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads and initialize MCLGUI.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-27 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

(defun make-logical-pathname-translations (logical-base physical-base)
  (list
   (list (format nil "~A**;*.*.*"    logical-base) (merge-pathnames #P"**/*.*" physical-base nil))
   (list (format nil "~A**;*.*"      logical-base) (merge-pathnames #P"**/*.*" physical-base nil))
   (list (format nil "~A**;*"        logical-base) (merge-pathnames #P"**/*"   physical-base nil))
   (list (format nil "~A**;"         logical-base) (merge-pathnames #P"**/"    physical-base nil))))

(defun directory-relative-to-source (relative-directory &optional (default-base (user-homedir-pathname)))
  (let ((this-file (or *compile-file-truename* *load-truename*)))
    (if this-file
        (merge-pathnames relative-directory
                         (make-pathname :name nil :type nil :version nil :defaults this-file)
                         nil)
        default-base)))


(setf (logical-pathname-translations "PATCHWORK")
      (make-logical-pathname-translations
       ""
       #.(directory-relative-to-source
          #P"../patchwork/"
          #P"~/works/patchwork/src/patchwork/")))

(setf (logical-pathname-translations "MCLGUI")
      (make-logical-pathname-translations
       ""
       #.(directory-relative-to-source
          #P""
          #P"~/works/patchwork/src/mclgui/")))


(cd   (translate-logical-pathname #P"MCLGUI:"))
(push (translate-logical-pathname #P"MCLGUI:") asdf:*central-registry*)
;; (pop asdf:*central-registry*)
(ql:quickload :mclgui)

(ui:initialize)

(defun load-sdi ()
  (load "scratch/dump")
  ;; (load "scratch/init-streams")
  (load "layout")
  (load "scratch/sdi")
  (in-package "MCLGUI")
  (print '(test-text-box)))
