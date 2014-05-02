;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               view-event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    View events.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
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


(defmethod view-activate-event-handler ((view simple-view))
  (values))

(defmethod view-deactivate-event-handler ((view simple-view))
  (values))

(defmethod view-activate-event-handler ((view view))
  (dovector (v (view-subviews view))
    (view-activate-event-handler v))
  (call-next-method))

(defmethod view-deactivate-event-handler ((view view))
  (dovector (v (view-subviews view))
    (view-deactivate-event-handler v))
  (call-next-method))





(defgeneric view-double-click-event-handler (view where)
  (:method (view where)
    (declare (ignore where))
    view))

(defmethod view-click-event-handler ((view simple-view) where)
  ;; (declare (ignore where))
  (format-trace 'view-click-event-handler 'simple-view view (point-to-list where))
  view)

;; (defmethod view-click-event-handler ((view view) where)
;;   (unless (do* ((subviews (view-subviews view))
;;                 (i (1- (length subviews)) (1- i))
;;                 subview)
;;                ((< i 0) nil)
;;             (setq subview (aref subviews i))
;;             (when (point-in-click-region-p subview where)
;;               (view-convert-coordinates-and-click subview where view)
;;               (return t)))
;;     (call-next-method)))

(defmethod view-click-event-handler ((view view) where)
  (let ((subview (find-if (lambda (subview)
                            (point-in-click-region-p subview where))
                          (view-subviews view)
                          :from-end t)))
    (format-trace 'view-click-event-handler 'view view (point-to-list where) :subview subview)
    (if subview
        (progn (view-convert-coordinates-and-click subview where view)
               nil)
        (call-next-method))))


(defmethod view-key-event-handler ((view simple-view) key)
  (declare (ignore key))
  (ed-beep))



(defmacro %get-current-key-handler (window)
  `(view-get ,window '%current-key-handler))

(defmethod current-key-handler ((w null))
  nil)

(defmethod current-key-handler ((w window))
  (%get-current-key-handler w))




(defmacro %get-key-handler-list (window)
  `(view-get ,window '%key-handler-list))

(defmethod key-handler-list ((view simple-view))
  (let ((w (view-window view)))
    (and w (%get-key-handler-list w))))


(defmacro %get-cancel-button (window)
  `(view-get ,window '%cancel-button))


(defmethod add-key-handler ((item simple-view) &optional (dialog (view-window item)))
  (let ((items (%get-key-handler-list dialog)))
    (unless (member item items)
      (setf (%get-key-handler-list dialog) (nconc items (list item)))))
  (when (key-handler-p item)
    (unless (current-key-handler dialog)
      (set-current-key-handler dialog item))))


(defmethod remove-key-handler ((item simple-view) &optional (dialog (view-window item)))
  (without-interrupts
    (when dialog
      (when (eq item (%get-current-key-handler dialog))
        (change-key-handler dialog)
        (when (eq item (%get-current-key-handler dialog)) ;still current, so only one
          (set-current-key-handler dialog nil)))
      (setf (%get-key-handler-list dialog) (delete item (%get-key-handler-list dialog))))))



(defmethod view-draw-contents ((view view))
  (call-next-method)
  (dovector (subview (view-subviews view))
    (view-focus-and-draw-contents subview)))


;;;; THE END ;;;;
