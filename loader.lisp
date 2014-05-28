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
      (make-logical-pathname-translations ""  #.(directory-relative-to-source #P"../patchwork/"
                                                                              #P"~/works/patchwork/src/patchwork/")))
(setf (logical-pathname-translations "MCLGUI")
      (make-logical-pathname-translations ""  #.(directory-relative-to-source #P"./"
                                                                              #P"~/works/patchwork/src/mclgui/")))


(cd   (translate-logical-pathname #P"MCLGUI:"))
(push (translate-logical-pathname #P"MCLGUI:") asdf:*central-registry*)
(ql:quickload :mclgui)


(load "scratch/dump")
(load "scratch/init-streams")
(load "scratch/sdi")
(in-package :ui)
(test-text-box)
