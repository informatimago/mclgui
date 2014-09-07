;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               get-string-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Get String From User Dialog
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
;;;;    
;;;;    Some code extracted from MCL (LGPL):
;;;;    Copyright 1985-1988 Coral Software Corp.
;;;;    Copyright 1989-1994 Apple Computer, Inc.
;;;;    Copyright 1995-2000 Digitool, Inc.
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


(defclass string-dialog (dialog)
  ((allow-empty-strings :initform nil :initarg :allow-empty-strings)))

(defclass get-string-dialog (string-dialog)
  ())




;; for dialogs which require non-empty strings to enable the default-button.
;; used by apropos, get-string-from-user and search-files

(defmethod view-key-event-handler :after ((d string-dialog) ch)
  (declare (ignore ch))
  (update-default-button d))


(defgeneric update-default-button (d)
  (:method ((d string-dialog))
    (let ((debutton (default-button d)))
      (when debutton
        (let ((text-items (subviews d 'editable-text-dialog-item)))
          (when text-items
            (let ((empties (slot-value d 'allow-empty-strings)))
              (if (or (eql empties t)
                      (dolist (item text-items t) ; enables if no text-items but there should be some
                        (unless (and (consp empties)
                                     (member (view-nick-name item) empties))
                          (when (zerop (dialog-item-text-length item))
                            (return nil)))))
                (dialog-item-enable debutton)
                (dialog-item-disable debutton)))))))))


(defgeneric view-line-height (view)
  (:method ((view simple-view))
    (multiple-value-bind (ff ms) (view-font-codes view)
      (font-codes-line-height ff ms))))

(defun get-string-from-user (message 
                             &key
                               (initial-string "")
                               (size #@(365 100))
                               (position '(:bottom 140))
                               (ok-text "OK")
                               (cancel-text "Cancel")
                               (modeless nil)
                               (window-title "")
                               (window-type :document-with-grow)
                               (back-color *tool-back-color*)
                               (allow-empty-strings nil)
                               (action-function #'identity)
                               (cancel-function nil)
                               (theme-background t))
  (when message 
    (flet ((act-on-text (item)
             (let ((e-item (find-subview-of-type (view-container item)
                                                 'editable-text-dialog-item)))
               (funcall action-function (dialog-item-text e-item)))))
      (let* ((dialog           (make-instance 'get-string-dialog
                                              :view-position position
                                              :view-size size
                                              :close-box-p (if modeless t nil)
                                              :grow-box-p t
                                              :window-type window-type
                                              :window-title window-title
                                              :window-show nil
                                              :back-color back-color
                                              :theme-background theme-background
                                              :allow-empty-strings allow-empty-strings))
             (top-margin       12)
             (left-margin       6)
             (right-margin      6)
             (bottom-margin    12)
             (interline        12)
             (button-height    (+ 6 (view-line-height dialog)))
             (input-height     (+ 2 (view-line-height dialog)))
             (input-min-width  300)
             (message-item     (make-instance 'static-text-dialog-item
                                              :text-truncation :end
                                              :view-position (make-point left-margin top-margin)
                                              :dialog-item-text message))
             (message-size     (view-default-size message-item))
             (min-width        (+ left-margin input-min-width right-margin))
             (mh               (max input-min-width
                                    (point-h message-size)
                                    (- (point-h size) left-margin right-margin)))
             (wh               (+ left-margin mh right-margin))
             (edit-top         (+ top-margin (point-v message-size) interline))
             (edit-item        (make-dialog-item 'editable-text-dialog-item 
                                                 (make-point left-margin edit-top)
                                                 (make-point mh input-height) 
                                                 initial-string))
             (button-top       (+ edit-top input-height interline))
             (ok-item          (make-dialog-item 'default-button-dialog-item
                                                 (make-point (- wh right-margin 62) button-top)
                                                 (make-point 62 button-height)
                                                 ok-text
                                                 (if modeless
                                                     (function act-on-text)
                                                     (lambda (item)
                                                       (return-from-modal-dialog (act-on-text item))))))
             (cancel-item      (make-dialog-item 'button-dialog-item
                                                 (make-point (- wh right-margin 62 interline 62) button-top)
                                                 (make-point 62 button-height)
                                                 cancel-text
                                                 (or cancel-function
                                                     (lambda (item)
                                                       (if modeless 
                                                           (window-close (view-window item))
                                                           (return-from-modal-dialog :cancel))))
                                                 :cancel-button t))
             (ok-size          (view-default-size ok-item))
             (cancel-size      (view-default-size cancel-item))
             (min-height       (+ button-top button-height bottom-margin))
             (wsize            (make-point wh (max min-height (point-v size)))))
        (setf (window-minimum-size dialog) (make-point min-width min-height))
        (set-view-size dialog wsize)
        (set-view-size ok-item ok-size)
        (set-view-size cancel-item cancel-size)
        (view-put dialog 'size-part (lambda ()
                                      (set-view-size edit-item (- (point-h (view-size dialog))
                                                                  left-margin right-margin)
                                                     input-height)
                                      (set-view-position ok-item (- (point-h (view-size dialog))
                                                                    (* 2 right-margin) (point-h ok-size))
                                                         button-top)
                                      (set-view-position cancel-item (- (point-h (view-position ok-item))
                                                                        interline (point-h cancel-size))
                                                         button-top)))
        (funcall (view-get dialog 'size-part))
        ;; subviews:
        (add-subviews dialog message-item edit-item cancel-item ok-item)
        (update-default-button dialog)
        (cond (modeless
               (window-show dialog)
               dialog)
              (t             
               (modal-dialog dialog)))))))

(defmethod window-size-parts ((dialog get-string-dialog))
  (funcall (view-get dialog 'size-part (lambda ()))))


;; (defmethod set-view-size ((dialog get-string-dialog) h &optional v)
;;   (let ((subviews   (view-subviews dialog))
;;         (new-size   (make-point h v))
;;         (old-size   (view-size dialog))
;;         (min-width  (view-get dialog 'min-width  0))
;;         (min-height (view-get dialog 'min-height 0)))
;;     (call-next-method dialog (max min-width (point-h new-size)) (max min-height (point-v new-size)))
;;     (when subviews
;;       (let* ((new-size (view-size dialog))
;;              (hdelta   (make-point (- (point-h new-size) (point-h old-size)) 0)))
;;         (loop
;;           :for item :across subviews
;;           :do (typecase item
;;                 (button-dialog-item
;;                  (set-view-position item (add-points (view-position item) hdelta)))
;;                 (editable-text-dialog-item
;;                  (set-view-size item (add-points (view-size item) hdelta)))))))))


;;;; THE END ;;;;
