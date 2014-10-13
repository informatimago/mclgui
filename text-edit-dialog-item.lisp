;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               text-edit-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Text-edit-dialog-item using the CL Text Edit Manager.
;;;;
;;;;    This file implements text-edit-dialog-item's.  If Fred is too big
;;;;    for your application, you may wish to replace editable-text-dialog-item's
;;;;    with text-edit-dialog-item's.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    <akh>
;;;;    <slh>
;;;;    <gb>
;;;;    <bill>
;;;;MODIFICATIONS
;;;;    2014-09-07 <PJB>  Implemented using CL text-edit manager.
;;;;    1997-06-02 <akh>  see below
;;;;    1995-11-29 <slh>  merged in Alice's changes (below)
;;;;    1995-06-23 <akh>  added te-typein-menu class, fixed some  set size and position bugs
;;;;                      No luck with body color
;;;;                      more carbon-compat - use wptr-font-codes
;;;;    1994-02-27 <akh>  merge with d13
;;;;    -------- 4.4B3
;;;;    1991-07-13 <akh> more carbon-compat in get-*te-handle*
;;;;    1991-05-11 <akh> carbon-compat _textbox => tetextbox
;;;;    ------- 4.3f1c1
;;;;    1997-05-05 <akh> better luck with body color
;;;;    1996-03-26 <gb>  lowmem accessors.
;;;;    1993-07-14 <bill> JooFung Wong's fix (slightly modified) that makes justification work
;;;;                      No longer need color-list in view-key-event-handler
;;;;    ------------- 3.0d11
;;;;    1992-06-01 <bill> support :body color: with-fore-color -> with-text-color
;;;;    1992-04-14 <bill> modernize dialog-item-text. Add dialog-item-text-length
;;;;    ------------- 2.0
;;;;    1992-01-07 <gb>   don't require "RECORDS".
;;;;    1991-12-12 <bill> miner's fix to paste
;;;;    ------------- 2.0b4
;;;;    1991-10-30 <bill> remove "-iv" on the end of slot names
;;;;    1991-10-24 <bill> Blake Meike's fix to dialog-te-handle.
;;;;                      Prevent flashing in view-key-event-handler
;;;;    1991-09-13 <bill> with-focused-font-view -> with-focused-dialog-item
;;;;    1991-08-26 <bill> :pointer -> :ptr, indentation
;;;;    1991-08-24 <gb>   use new trap syntax.
;;;;    1991-05-17 <bill> # in front of $TEScrpHandle & $TEScrpLength thanx to UEDA masaya
;;;;    1991-01-05 <bill> add TOGGLE-BLINKERS
;;;;    ----------- 2.0b1
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
;;;;    Copyright 1990-1994, Apple Computer, Inc
;;;;    Copyright 1995 Digitool, Inc.
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


(defclass text-edit-dialog-item (basic-editable-text-dialog-item)
  ((text-justification :initform 0 :initarg :text-justification)
   (sel-start :initform 0)
   (sel-end :initform 0)))


;; use this class if you want to have typein menus that use text-edit-dialog-item 
(defclass te-typein-menu (typein-menu)
  ()
  (:default-initargs    
   :editable-text-class 'text-edit-dialog-item))

(defmethod part-color ((item text-edit-dialog-item) key)
  (or (getf (slot-value item 'color-list) key nil)
      (case key
        (:body *white-color*)
        (:text *black-color*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allocate one text-edit record for sharing by all. Process wise does this make sense??
;;
(defvar *te-handle* nil)
(defvar *null-text-handle* nil)
(defvar *te-handle-dialog-item* nil)

(defun get-*te-handle* ()
  (or *te-handle*
      (let ((rect (if *current-view*
                      (view-frame (or (view-window *current-view*) *current-view*))
                      (make-rect 0 0 40 20))))
        (setf *null-text-handle* ""
              *te-handle-dialog-item* nil
              *te-handle* (te-new rect rect (and *current-view* (view-window *current-view*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Update the text-edit record for the current-key-handler of a window
;;
(defgeneric dialog-te-handle (window &optional select))
(defmethod dialog-te-handle ((w window) &optional select)
  (without-interrupts
    (let* ((hTE (get-*te-handle*))
           (item *te-handle-dialog-item*)
           (current-text (current-key-handler w)))
      (cond ((not (typep current-text 'text-edit-dialog-item))   ; ignore fred-dialog-items
             (setq *te-handle-dialog-item* nil))
            ((eql current-text item)
             ;; no change, nothing to do. 
             (setq *te-handle-dialog-item* current-text))
            (t 
             (when item
               ;; leave the old item
               (setf (slot-value item 'sel-start) (te-sel-start hTE)
                     (slot-value item 'sel-end)   (te-sel-end   hTE))
               (with-focused-view (view-container item)
                 (with-fore-and-back-color (part-color item :text) (part-color item :body)
                   (TE-Deactivate hTE))))
             (if current-text
                 (with-focused-view (view-container current-text)
                   (with-fore-and-back-color (part-color current-text :text) (part-color current-text :body)
                     ;; install into the new item
                     (with-slot-values (dialog-item-handle line-height font-ascent text-justification) current-text
                       (setf (te-in-port hTE) w)
                       (te-set-just text-justification hTE)   ; JooFung Wong's fix
                       ;; (format-trace '(dialog-te-handle window) :dialog-item-handle dialog-item-handle :current-text current-text)
                       (te-set-text dialog-item-handle hTE)
                       (setf (te-click-loc hTE) -1)
                       (multiple-value-bind (ff ms) (view-font-codes current-text)
                         (with-font-codes ff ms
                           (te-set-font-info ff ms hTE)
                           (when line-height (setf (te-line-height hTE) line-height))
                           (when font-ascent (setf (te-font-ascent hTE) font-ascent))
                           (with-item-rect (rect current-text)
                             (te-set-rects rect rect hTE))
                           (TE-Auto-View t hTE)
                           (TE-Cal-Text hTE)
                           (if select
                               (te-set-select 0 32000 hTE)
                               (te-set-select (slot-value current-text 'sel-start)
                                              (slot-value current-text 'sel-end)
                                              hTE))
                           (when (window-active-p w)
                             (TE-Activate hTE)))))))
                 (progn
                   ;; leave all items
                   (te-set-text *null-text-handle* hTE)
                   (setf (te-in-port hTE) w)))
             (setq *te-handle-dialog-item* current-text)))
      hTE)))


#-(and) (

         *te-handle-dialog-item*
         (editable-text-dialog-item :view-nick-name nil :view-position (6 5) :position/window (38 57) :view-size (273 154) :view-scroll-position (0 0) "#x302005018DCD")
         
         (let ((te (get-*te-handle*)))
           (with-focused-view (view-container *te-handle-dialog-item*)
             (view-font-codes  *te-handle-dialog-item*))
           (values (te-ff te) (te-ms te)))
         0
         0
         262144
         9


         (
          (multiple-value-call (function te-set-font-info) (with-focused-view (view-container *te-handle-dialog-item*)
                                                             (view-font-codes  *te-handle-dialog-item*)) te)
          (te-cal-text te)
          (values (te-ff te) (te-ms te)))
         262144
         9
         262144
         9
         (multiple-value-call (function font-spec) (with-focused-view (view-container *te-handle-dialog-item*)
                                                     (view-font-codes  *te-handle-dialog-item*)))
         ("Monaco" 9 :srccopy :plain (:color-index 0))
         ("Monaco" 9 :srccopy :plain (:color-index 0))
         )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The guts
;;

(defmethod key-handler-idle ((item text-edit-dialog-item)
                             &optional (dialog (view-window item)))
  (let ((hTE (dialog-te-handle dialog)))
    (with-focused-dialog-item (item)
      (TE-Idle hTE))))


;; Should never be called unless the item is contained in a window.
(defmethod install-view-in-window ((item text-edit-dialog-item) view)
  (declare (ignorable view))
  (let* ((text (ensure-simple-string (slot-value item 'dialog-item-text))))
    (setf (slot-value item 'dialog-item-handle) text
          (slot-value item 'dialog-item-text) nil))
  (call-next-method))


(defmethod remove-view-from-window ((item text-edit-dialog-item))
  (setf (slot-value item 'dialog-item-text) (te-get-text (dialog-te-handle (view-window item)))
        (slot-value item 'dialog-item-handle) nil)
  (call-next-method))


(defmethod remove-key-handler :after ((item text-edit-dialog-item)
                                      &optional (dialog (view-window item)))
  (when dialog
    (dialog-te-handle dialog)))   ; update the *te-handle*


;; This is not always necessary, but the code that knows if it is
;; is in the method for basic-editable-text-dialog-item.
(defmethod dialog-item-disable :before ((item text-edit-dialog-item))
  (let ((dialog (view-window item)))
    (when (and dialog (dialog-item-handle item))
      (dialog-te-handle dialog))))


(defmethod set-view-font-codes :after ((item text-edit-dialog-item) ff ms
                                       &optional ff-mask ms-mask)
  (declare (ignorable ff ms ff-mask ms-mask))
  #+debug-views (format-trace '(set-view-font-code :after text-edit-dialog-item)
                             :ff-ms (list ff ms)
                             :view-ff-ms (multiple-value-list (view-font-codes item)))
  (when (view-window item)
    (multiple-value-bind (ff ms) (view-font-codes item)
      (te-set-font-info ff ms (dialog-te-handle (view-window item))))))


(defmethod set-view-position :before ((item text-edit-dialog-item) h &optional v)
  (let ((my-dialog (view-window item))
        (position  (view-position item))
        (new-pos   (make-point h v)))    
    (when my-dialog
      (if (eql item (current-key-handler my-dialog))
          (let ((te-handle (dialog-te-handle my-dialog)))
            (if position
                (let* ((diff (subtract-points new-pos position))
                       (view (offset-rect (te-view-rect te-handle) diff))
                       (dest (offset-rect (te-dest-rect te-handle) diff)))
                  (te-set-rects dest view te-handle))
                (let ((view (te-view-rect te-handle))
                      (dest (te-dest-rect te-handle)))
                  (setf (rect-topleft view) position
                        (rect-bottomright dest) (add-points position (view-size item)))
                  (te-set-rects dest view te-handle))))))))


(defmethod set-view-size ((item text-edit-dialog-item) h &optional v)
  (let ((new-size (make-point h v)))
    (without-interrupts
      (invalidate-view item t)
      (setf (slot-value item 'view-size) new-size)
      (when (and (installed-item-p item) (view-position item))
        (with-focused-dialog-item (item)
          (let* ((my-dialog  (view-window item))
                 (hTE        (dialog-te-handle my-dialog))
                 (position   (view-position item))
                 (new-corner (add-points position new-size)))
            (when (eql item (current-key-handler my-dialog))
              (let ((view (te-view-rect hTE))
                    (dest (te-dest-rect hTE)))
                (setf (rect-bottomright view) new-corner
                      (rect-bottomright dest) new-corner)
                (te-set-rects dest view hTE))
              (invalidate-view item))))))
    new-size))


(defmethod view-click-event-handler ((item text-edit-dialog-item) where)
  (let ((my-dialog (view-window item)))
    ;; (with-quieted-view item             ; prevents flashing
    (unless (eql item (current-key-handler my-dialog)) 
      (set-current-key-handler my-dialog item nil))
    (with-focused-dialog-item (item)
      (with-fore-and-back-color (part-color item :text) (part-color item :body)
        (TE-Click where (shift-key-p) (dialog-te-handle my-dialog))))))


(defmethod view-activate-event-handler ((item text-edit-dialog-item))
  (let ((my-dialog (view-window item)))
    (when (eql item (current-key-handler my-dialog))
      (with-focused-dialog-item (item)
        (with-fore-and-back-color (part-color item :text) (part-color item :body)
          (TE-Activate (dialog-te-handle my-dialog)))))))


(defmethod view-deactivate-event-handler ((item text-edit-dialog-item))
  (let ((my-dialog (view-window item)))
    (when (and my-dialog (eql item (current-key-handler my-dialog)))
      (with-focused-dialog-item (item)
        (with-fore-and-back-color (part-color item :text) (part-color item :body)
          (TE-Deactivate (dialog-te-handle my-dialog)))))))


(defgeneric toggle-blinkers (item on-p))
(defmethod toggle-blinkers ((item text-edit-dialog-item) on-p)
  (if on-p
      (view-activate-event-handler item)
      (view-deactivate-event-handler item)))


(defmethod set-dialog-item-text ((item text-edit-dialog-item) text)
  (setq text (ensure-simple-string text))
  (if (installed-item-p item)
      (progn
        (setf (dialog-item-handle item) text)
        (let ((my-dialog (view-window item)))
          (when (eql item (current-key-handler my-dialog))
            (with-focused-dialog-item (item)
              (TE-Cal-Text (dialog-te-handle my-dialog))))
          (when (plusp (length text))
            (set-selection-range item 0 32000))
          (invalidate-view item)))
      (setf (slot-value item 'dialog-item-text) text))
  text)


(defun update-dialog-item-handle (item)
  (without-interrupts
    (when (eql item *te-handle-dialog-item*)
      ;;  (assert (eql item (current-key-handler (view-window item))))
      (let ((hTE (get-*te-handle*)))
        (setf (dialog-item-handle item) (te-get-text hTE))))))

(defmethod dialog-item-text ((item text-edit-dialog-item))
  (update-dialog-item-handle item)
  (let ((handle (dialog-item-handle item)))
    (if (and handle (wptr item))
        handle
        (slot-value item 'dialog-item-text))))

(defgeneric dialog-item-text-length (item))
(defmethod dialog-item-text-length ((item text-edit-dialog-item))
  (length (dialog-item-text item)))      


(defmethod view-draw-contents :after ((item text-edit-dialog-item))
  (let ((my-dialog     (view-window item))
        (item-position (view-position item))
        (item-size     (view-size item))
        (enabled-p     (dialog-item-enabled-p item))
        (colorp        (color-or-gray-p item)))
    (when (installed-item-p item)
      (with-slot-values (dialog-item-handle text-justification) item
        (without-interrupts
          (let ((rect (make-rect item-position (add-points item-position item-size)))
                (te   (dialog-te-handle my-dialog)))
            (with-fore-and-back-color (if (and colorp (not enabled-p))
                                          *gray-color* 
                                          (part-color item :text))
                (part-color item :body)
              (if (eql item (current-key-handler my-dialog))
                  (progn
                    (multiple-value-bind (ff ms) (view-font-codes my-dialog)
                      (te-set-font-info ff ms te))
                    (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
                    (te-update rect te))
                  (Text-Box dialog-item-handle rect text-justification)))))))))


(defmethod view-key-event-handler ((item text-edit-dialog-item) char)
  (when (integerp char)
    (setq char (code-char char)))
  (let ((container (view-container item)))
    (with-focused-dialog-item (item container)
      (with-text-colors item
        (TE-Key char (dialog-te-handle (view-window item))))
      (dialog-item-action item))))


(defmethod selection-range ((item text-edit-dialog-item))
  (without-interrupts
    (if (eql item *te-handle-dialog-item*)
        (let ((teh *te-handle*))
          (values
           (te-sel-start teh)
           (te-sel-end   teh)))
        (values (slot-value item 'sel-start)
                (slot-value item 'sel-end)))))


(defmethod set-selection-range ((item text-edit-dialog-item) &optional start end)
  (multiple-value-bind (s e) (selection-range item)
    (unless start (setq start e))
    (unless end   (setq end   e))
    (when (< end start) (rotatef start end))
    (unless (and (= start s) (= end e))
      (setf (slot-value item 'sel-start) start
            (slot-value item 'sel-end)   end)
      (without-interrupts
        (when (eql item *te-handle-dialog-item*)
          (let ((teh *te-handle*))
            (with-focused-view (view-container item)
              (with-fore-color (part-color item :text)
                (with-back-color (part-color item :body)
                  (TE-Set-Select start end teh))))))))))


(defmethod cut ((item text-edit-dialog-item))
  (let ((my-dialog (view-container item)))
    (with-focused-view my-dialog
      (with-fore-color (part-color item :text)
        (with-back-color (part-color item :body)
          (with-font-codes nil nil
            (TE-Cut (dialog-te-handle (view-window item))))))))
  (te-scrap-to-lisp-scrap)
  (dialog-item-action item))


(defmethod copy ((item text-edit-dialog-item))
  (let ((my-dialog (view-container item)))
    (with-focused-view my-dialog
      (with-font-codes nil nil
        (TE-Copy (dialog-te-handle (view-window item))))))
  (te-scrap-to-lisp-scrap)
  (dialog-item-action item))


(defun te-scrap-to-lisp-scrap ()
  (values))


(defmethod paste ((item text-edit-dialog-item))
  (let ((my-dialog (view-container item))
        (scrap     (get-scrap :text))
        (te-handle (dialog-te-handle (view-window item))))
    (when scrap
      (with-focused-view my-dialog
        (with-fore-color (part-color item :text)
          (with-back-color (part-color item :body)
            (with-font-codes nil nil
              (TE-Delete te-handle) 
              (TE-Insert scrap te-handle)))))))
  (dialog-item-action item))


(defmethod select-all ((item text-edit-dialog-item))
  (set-selection-range item 0 32000))


(defmethod clear ((item text-edit-dialog-item))
  (let ((my-dialog (view-container item)))
    (with-focused-view my-dialog
      (with-fore-color (part-color item :text)
        (with-back-color (part-color item :body)
          (with-font-codes nil nil
            (TE-Delete (dialog-te-handle (view-window item))))))))
  (dialog-item-action item))


;;;---------------------------------------------------------------------
;;; Tests
;;;---------------------------------------------------------------------

(defun test/tedi ()
  (let ((window (make-instance 'dialog
                               :window-title "test/tedi"
                               :view-size #@(200 400)
                               :view-font '("Geneva" 12))))
    (make-instance 'text-edit-dialog-item :view-position #@(20 50) :view-size #@(180 20) :dialog-item-text "Input"
                                          :view-container window)
    window))

#-(and) (
         (test/tedi)
         (map nil 'print (mapcar (function first) *font-families*))
         (
          "Andale Mono" 
          "Arial" 
          "Arial Black" 
          "Courier" 
          "Courier New" 
          "Didot" 
          "Geneva" 
          "Helvetica" 
          "Lucida Grande" 
          "Menlo" 
          "Monaco" 
          "Optima" 
          "Osaka" 
          "PT Mono" 
          )
         )



;;;; THE END ;;;;
