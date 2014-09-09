;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The view class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-13 <PJB> Created.
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
(enable-sharp-at-reader-macro)
(objcl:enable-objcl-reader-macros)



(defgeneric view-draw-contents (view)
  (:documentation "
The generic function VIEW-DRAW-CONTENTS is called whenever a view
needs to redraw any portion of its contents.  The VIEW method for
VIEW-DRAW-CONTENTS erases the area in the window’s erase region (for
new windows, this is the entire content area) and then calls
VIEW-DRAW-CONTENTS on each subview.  You can specialize this function
so that a user-defined view can be redrawn when portions of it are
covered and uncovered.

When VIEW-DRAW-CONTENTS is called by the event system, the view’s clip
region is set so that drawing occurs only in the portions that need to
be updated.  This normally includes areas that have been covered by
other windows and then uncovered.

VIEW:           A simple view.
")
  (:method (view)
    (declare (ignore view))
    nil))


;; (defun nsview-to-view-position (frame-nsrect size-point)
;;   "
;; RETURN: The view-position POINT.
;; "
;;   #-(and)
;;   (let ((screen-pos (main-screen-frame)))
;;     (make-point (- (round (ns:ns-rect-x frame-nsrect)) (point-h screen-pos))
;;                 (- (point-v screen-pos) (round (ns:ns-rect-y frame-nsrect))
;;                    (point-v size-point)))))
;; 
;; 
;; (defun view-to-nsview-origin (position size)
;;   "
;; RETURN: A NSPoint containing the origin of the nsview.
;; "
;;   #-(and)
;;   (multiple-value-bind (screen-pos screen-siz) (main-screen-frame)
;;     (ns:make-ns-point (+ (point-h screen-pos) (point-h position))
;;                       (- (+ (point-v screen-pos) (point-v screen-siz))
;;                          (point-v position) (point-v size)))))
;; 
;; 
;; (defun view-to-nsview-frame (position size)
;;   "
;; RETURN: A NSRect containing the frame of the view.
;; "
;;   #-(and)
;;   (multiple-value-bind (screen-pos screen-siz) (main-screen-frame)
;;     (ns:make-ns-rect (+ (point-h screen-pos) (point-h position))
;;                      (- (+ (point-v screen-pos) (point-v screen-siz))
;;                         (point-v position) (point-v size))
;;                      (point-h size)
;;                      (point-v size))))

(defmethod initialize-instance ((view simple-view) &key view-font &allow-other-keys)
  (declare (stepper trace))
  (call-next-method)
  (unless (and (slot-boundp view 'view-position)
               (slot-value view 'view-position))
    (setf (%view-position view) (view-default-position view)))
  (unless (and (slot-boundp view 'view-size)
               (slot-value view 'view-size))
    (setf (%view-size view) (view-default-size view)))
  (let ((container (slot-value view 'view-container)))
    (when container
      (setf (slot-value view 'view-container) nil)
      (set-view-container view container)))
  (when view-font
    (set-initial-view-font view view-font)))


(defmethod initialize-instance ((view view) &key &allow-other-keys)
  (declare (stepper trace))
  (call-next-method)
  (setf (slot-value view 'view-subviews) #())
  view)


(defmethod initialize-instance :after  ((view view) &key view-subviews subviews &allow-other-keys)
  ;; We need to do that after the subclasses such as windows have initialized.
  (let ((subviews (or view-subviews subviews)))
    ;; (format-trace "initialize-instance" view subviews)
    (setf (slot-value view 'view-subviews)
          (make-array (length subviews) :adjustable t :fill-pointer 0))
    (apply (function add-subviews) view (coerce subviews 'list)))
  view)



(defgeneric wptr (simple-view)
  (:documentation "
The wptr generic function holds the pointer to a window record on the
Macintosh heap. 
This generic function returns a window pointer if the view is contained in a
window, or nil if the view is not contained in a window.
All views contained in a given window have the same wptr.
")
  (:method ((view simple-view))
    (let ((window (view-window view)))
      (when (and window (handle window))
        (window-ptr window)))))



;; if container
;; (view-origin view) = (view-scroll-position view) - (view-position view) +  (view-origin container)
;; else
;; (view-origin view) = (view-scroll-position view)

(defgeneric compute-view-origin (view)
  (:method ((view simple-view))
    (let ((container (view-container view)))
      (setf (view-origin-slot view)
            (if container
                (add-points (subtract-points (view-scroll-position view)
                                             (view-position view))
                            (view-origin container))
                (view-scroll-position view))))))

(defgeneric view-origin (view)
  (:method ((view simple-view))
    (setf (view-origin-slot view) (compute-view-origin view)))
  (:method ((view view))
    (if (view-valid-p view)
        (view-origin-slot view)
        (prog1 (compute-view-origin view)
          (make-view-valid view)
          (compute-view-region view (view-clip-region-slot view)  (view-container view))))))


(defmethod view-allocate-clip-region ((view view))
  (let ((rgn (view-clip-region view)))
    (or rgn
        (setf (view-clip-region-slot view) (new-rgn)))))

(defgeneric view-clip-region (view)
  (:method ((view view))
    (let ((rgn (view-clip-region-slot view)))
      (unless (or (null rgn) (view-valid-p view))
        (compute-view-origin view)
        (make-view-valid view)
        (compute-view-region view rgn (view-container view)))
      rgn)))


(defgeneric compute-view-region (view rgn container)
  (:method ((view simple-view) rgn container)
    (declare (ignore container))
    (when rgn
      (let* ((topleft (view-origin view))
             (botright (add-points topleft (view-size view))))
        (set-rect-region rgn (point-h topleft) (point-v topleft) (point-h botright) (point-v botright))))
    rgn))


(defun set-font (font-view)
  "
FONT-VIEW: a view or NIL.
DO:        Set the view-font as the current font in the graphic environment.
RETURN:    the view-font-codes of the font-view or of the application-font.
"
  (multiple-value-bind (ff ms) (when font-view
                                 (view-font-codes font-view))
    (let ((ff (or ff 65536)) ; application-font
          (ms (or ms 0)))
      (multiple-value-bind (font mode) (font-from-codes ff ms)
        (declare (ignorable mode))
        ;; TODO: manage mode (:srcOr …)
        [font set])
      (list ff ms))))

;; [(font-from-codes 65536 0) set]


(defun get-at (at)
  #-ccl (error "~S is not implemented on ~A" 'get-at (lisp-implementation-type))
  #+ccl (list (list (ccl::pref at #>CGAffineTransform.a)
                    (ccl::pref at #>CGAffineTransform.b))
              (list (ccl::pref at #>CGAffineTransform.c)
                    (ccl::pref at #>CGAffineTransform.d))
              (list (ccl::pref at #>CGAffineTransform.tx)
                    (ccl::pref at #>CGAffineTransform.ty))))

(defun get-ctm (window)
  (get-at (cg:context-get-ctm  [[(handle window) graphicsContext] graphicsPort])))

(defun graphics-flush ()
  [[NSGraphicsContext currentContext] flushGraphics])
(declaim (inline get-ctm graphics-flush))


(defgeneric call-with-focused-view (view function &optional font-view)
  ;; Note: be careful to return the FUNCTION results in all cases.
  (:method ((view simple-view) function &optional (font-view *current-font-view*))
    (labels ((call-it ()
               (call-with-pen-state (lambda () (funcall function view))
                                    (view-pen view)))
             (call-it/trans ()
               (let* ((window (view-window view))
                      (wtrans (window-affine-transform window))
                      (vtrans (make-affine-transform))
                      (origin (convert-coordinates #@(0 0) view window)))
                 (unless wtrans
                   (warn "(window-affine-transform window) is nil :window ~S" window)
                   (return-from call-it/trans (values)))
                 [wtrans set]
                 [vtrans translateXBy:(cgfloat 0.0) yBy:(cgfloat (point-v (view-size (view-window view))))]
                 [vtrans scaleXBy:(cgfloat 1.0) yBy:(cgfloat -1.0)]
                 [vtrans translateXBy:(cgfloat (point-h origin)) yBy:(cgfloat (point-v origin))]
                 [vtrans concat]
                 [vtrans release]
                 ;; (setf *trace-output* cl-user::*to*)
                 #+debug-views (format-trace 'call-with-focused-view (list (class-name (class-of view))
                                                                           :origin (point-to-list origin)
                                                                           :frame (rect-to-list (view-frame view))
                                                                           :bounds (rect-to-list (view-bounds view)))
                                             :ctm (get-ctm window))
                 (unwind-protect
                      (call-it)
                   [wtrans set]))))
      (declare (inline call-it call-it/trans))
      (let ((same-font (eql *current-font-view* font-view)))
        (if (eql *current-view* view)
            (if same-font
                (let ((*current-view*       view)
                      (*current-font-view*  font-view)
                      (*current-font-codes* (copy-list *current-font-codes*)))
                  (call-it))
                (unwind-protect
                     (let ((*current-view*       view)
                           (*current-font-view*  font-view)
                           (*current-font-codes* (set-font font-view))) ; change font
                       (call-it))
                  (set-font *current-font-view*))) ; revert font
            (with-view-handle (winh view)
              (let ((unlock   nil))
                (unwind-protect
                     (let ((*current-view*      view)
                           (*current-font-view* font-view)
                           (*current-font-codes* (copy-list *current-font-codes*)))
                       (when (setf unlock [winh lockFocusIfCanDraw])
                         (focus-view *current-view* *current-font-view*)
                         (unless same-font (apply (function set-current-font-codes) (set-font *current-font-view*)))
                         (call-it/trans)))
                  (graphics-flush)
                  (when unlock
                    (unless same-font (set-font *current-font-view*))
                    [winh unlockFocus])
                  (focus-view *current-view* *current-font-view*)))))))))



(defgeneric focus-view (view &optional font-view)
  (:documentation "
DO:             The FOCUS-VIEW function installs the GrafPort of view
                as the current GrafPort and sets the clip region and
                origin so that drawing will occur in the coordinate
                system of view.  The FOCUS-VIEW function is not
                normally called directly. In general,
                WITH-FOCUSED-VIEW should be used when drawing to
                views.

VIEW:           A view installed in a window, or NIL.  If NIL, the
                current GrafPort is set to an invisible GrafPort.

FONT-VIEW:      A view or NIL. If NIL, the font is unchanged.  If
                non-NIL, the view-font-codes of font-view are
                installed after the rest of the focusing is completed.
                The default is NIL.
")
  (:method ((null null) &optional font-view)
    (setf *current-font-view* font-view
          *current-view* nil))
  (:method ((view simple-view) &optional font-view)
    (setf *current-font-view* font-view
          *current-view* view)))



(defmacro with-focused-view (view &body body &environment env)
  "
DO:             The WITH-FOCUSED-VIEW macro executes BODY with the
                current GrafPort set for drawing into view.  This
                involves setting the current GrafPort and setting the
                origin and clip region so that drawing occurs in VIEW.
                When the BODY exits (normally or abnormally), the old
                view is restored.

VIEW:           A view installed in a window, or NIL.  If NIL, the
                current GrafPort is set to an invisible GrafPort.
"
  (let ((sym (if (and view (symbolp view) (eql view (macroexpand view env)))
                 view
                 (gensym))))
    `(call-with-focused-view ,view
                             (lambda (,sym)
                               (declare (ignorable ,sym))
                               ,@body))))



(defmacro with-font-focused-view (view &body body &environment env)
  "
DO:             The macro with-font-focused-view focuses on the font
                of view, then calls with-focused-view.

VIEW:           A view installed in a window, or NIL.  If NIL, the
                current GrafPort is set to an invisible GrafPort.
"
  (let ((sym (if (and view (symbolp view) (eql view (macroexpand view env)))
                 view
                 (gensym)))
        (vview (gensym)))
    `(let ((,vview ,view))
       (call-with-focused-view ,vview
                               (lambda (,sym)
                                 (declare (ignorable ,sym))
                                 ,@body)
                               ,vview))))


(defun refocus-view (view)
  (when (eql view *current-view*)
    (setq *current-view* nil)
    (focus-view view *current-font-view*)))



(defgeneric install-view-in-window (view window)
  (:documentation "
DO:             Installs VIEW in the WINDOW window.

                This function performs initialization tasks that
                require the containing window.  It should never be
                called directly by user code.  However, it may be
                shadowed.  Specialized versions of
                INSTALL-VIEW-IN-WINDOW should always perform
                CALL-NEXT-METHOD.

VIEW:           A view or subview, but not a window. Instances of
                window cannot have containers.

WINDOW:         A window.
")
  (:method :before ((view simple-view) window)
    (assert (eql (view-window view) window)))
  (:method :before ((view view) window)
    (declare (ignore window))
    (setf (view-clip-region-slot view) (new-region)
          (view-valid view) (cons nil (view-valid (view-container view)))))
  (:method ((view simple-view) window)
    (declare (ignore window)))
  (:method ((view view) window)
    (dovector (subview (view-subviews view))
      (install-view-in-window subview window))))


(defgeneric remove-view-from-window (view)
  (:documentation "
DO:             Remove view from its container.  It should never be
                called directly by user code.  However, it may be
                shadowed.  Specialized versions of
                REMOVE-VIEW-FROM-WINDOW should dispose of any
                Macintosh data the item uses (that is, data not
                subject to garbage collection) and should always
                perform a CALL-NEXT-METHOD.

VIEW:           A view or subview, but not a window.  Instances of
                window cannot have containers.
")
  (:method ((view simple-view)))
  (:method ((view view))
    (dovector (subview (view-subviews view))
      (remove-view-from-window subview)))
  (:method :after ((view view))
    (let ((rgn (view-clip-region-slot view)))
      (when rgn
        (setf (view-clip-region-slot view) nil)
        (dispose-region rgn)))
    (setf (view-valid view) nil)))


(defgeneric add-view-to-container (view container)
  (:documentation "
DO:             Add the VIEW to container.

POST:           (eql (view-container view) container)

RETURN:         VIEW.
")
  (:method ((view simple-view) (container view))
    (let ((siblings (view-subviews container)))
      (vector-push-extend view siblings))
    (setf (slot-value view 'view-container) container)
    view))


(defgeneric remove-view-from-superview (view)
  (:documentation "
DO:             Remove the VIEW from its superview.
NOTE:           the view-container is not changed.
RETURN:         VIEW.
")
  (:method ((view simple-view))
    (let ((old-container (view-container view)))
      (deletef (slot-value old-container 'view-subviews) view))
    view))


(defgeneric set-view-container (view new-container)
  (:documentation "
DO:             Set the view that contains to NEW-CONTAINER.  If the
                window of the view is changed by giving it a new
                container, REMOVE-VIEW-FROM-WINDOW is called on view
                and the old window, and INSTALL-VIEW-IN-WINDOW is
                called on view and the new window.

VIEW:           A view or subview, but not a window. Instances of
                window cannot have containers. If SET-VIEW-CONTAINER
                is called on a window, it signals an error.

NEW-CONTAINER:  The new container of the view.
")
  (:method ((view simple-view) new-container)
    ;; If container is nil, removes view from container
    ;; Note: The dialog code depends on the fact that the view-container slot is
    ;; changed AFTER the WPTR is changed.
    (let ((old-container (view-container view)))
      (unless (eql new-container old-container)    
        (when new-container
          (check-type new-container view)
          (when (or (eql new-container view)
                    (view-contains-p view new-container))
            (error 'view-error :view view
                               :format-control "Attempt to make ~S contain itself."
                               :format-arguments (list view))))
        (let* ((new-window (and new-container (view-window new-container)))
               (old-window (and old-container (view-window old-container)))
               (current-view *current-view*)
               (current-font-view *current-font-view*))
          (when old-container
            (invalidate-view view t)
            (when (eql view current-view)
              (focus-view nil))
            ;; Note: remove-view-from-superview doesn't modify the
            ;;       view-container.  This is important because
            ;;       remove-view-from-window methods use
            ;;       view-container (eg. to focus-view).
            (remove-view-from-superview view)
            (unless (eql new-window old-window)
              (remove-view-from-window view))
            (setf (slot-value view 'view-container) nil))
          ;; -
          (when (and (null new-container) (eql *mouse-view* view))
            (setf *mouse-view* nil))
          (when new-container
            (add-view-to-container view new-container)
            (unless (eql new-window old-window)
              (install-view-in-window view new-window))
            (invalidate-view view)
            (when (eql view current-view)
              (focus-view view current-font-view))
            (if (and new-window (window-active-p new-window))
                (view-activate-event-handler view)
                (view-deactivate-event-handler view)))))))
  
  (:method ((w window) new-container)
    (unless (null new-container)
      (error "Container must always be ~S for windows." nil))
    new-container))



(defgeneric add-subviews (view &rest subviews)
  (:documentation "
DO:             Set the container of each of subviews to view.  If any
                of the subviews are already owned by view,
                add-subviews does nothing.

VIEW:           A view.

SUBVIEWS:       A list of view or simple view, but not a window;
                SUBVIEWS must be able to be contained within view.
")
  (:method ((view view) &rest subviews)
    (unless (find view subviews :test (function view-contains-p))
      (dolist (subview subviews)
        (check-type subview simple-view)
        (set-view-container subview view)))))


(defgeneric remove-subviews (view &rest subviews)
  (:documentation  "
DO:             Remove each of SUBVIEWS from view.  If a subview is
                not in view, an error is signaled.

VIEW:           A view.

SUBVIEWS:       A list of view or simple view, but not a window;
                SUBVIEWS must be able to be contained within view.
")
  (:method ((view view) &rest subviews)
    (unless (find view subviews :test (complement (function view-contains-p)))
      (dolist (subview subviews)
        (check-type subview simple-view)
        (set-view-container subview nil)))))



(defgeneric view-contains-p (view contained-view)
  (:documentation "Whether CONTAINED-VIEW is a sub+view of VIEW.")
  (:method ((view simple-view) contained-view)
    (when contained-view
      (loop
        :for container = (view-container contained-view)
          :then (view-container container)
        :while container 
          :thereis (eql container view))))
  (:method ((view null) contained-view)
    (declare (ignore contained-view))
    nil))


(defmacro do-subviews ((subview-var view &optional (subview-type t))
                       &body body)
  "
DO:             For each subview of VIEW of the given SUBVIEW-TYPE,
                the macro DO-SUBVIEWS executes BODY with SUBVIEW-VAR
                bound to the subview.

SUBVIEW-VAR:    A variable.

VIEW:           A view.

SUBVIEW-TYPE:   A Common Lisp type specifier.
"
  (let ((vview         (gensym "view"))
        (vsubview-type (gensym "subview-type"))
        (vsubviews     (gensym "subviews")))
    `(let* ((,vview         ,view)
            (,vsubview-type ,subview-type)
            (,vsubviews     (copy-seq (view-subviews ,vview))))
       (dovector (,subview-var ,vsubviews (values))
         (when (typep ,subview-var ,vsubview-type)
           ,@body)))))


(defgeneric map-subviews (view function &optional subview-type)
  (:documentation "
DO:             For each subview of view of the given SUBVIEW-TYPE,
                call FUNCTION with the subview as its single argument.

VIEW:           A view.

FUNCTION:       A function.

SUBVIEW-TYPE:   A Common Lisp type specifier.
")
  (:method ((view simple-view) function &optional (subview-type t))
    (do-subviews (subview view subview-type)
      (funcall function subview))))




(defgeneric subviews (view &optional subview-type)
  (:documentation "
RETURN:         A list of the subviews of view.  If subview-type is
                present, only subviews matching that type are
                returned.

VIEW:           A view.

SUBVIEW-TYPE:   A Common Lisp type specifier.
")
  (:method ((view simple-view) &optional (subview-type t))
    (declare (ignore subview-type))
    '())
  (:method ((view view) &optional (subview-type t))
    (let ((result nil))
      (dovector (subview (view-subviews view) (nreverse result))
        (when (typep subview subview-type)
          (push subview result))))))

(defgeneric view-named (name view)
  (:documentation "
RETURN:         The first subview of view whose nickname is name. The
                subviews are searched in the order in which they were
                added to view.

NAME:           Any object, but usually a symbol.  Nicknames are
                compared using EQL.

VIEW:           A view.
")
  (:method (name (view simple-view))
    (dovector (subview (view-subviews view))
      (if (eql name (view-nick-name subview))
          (return subview)))))


(defgeneric find-named-sibling (view name)
  (:documentation "

DO:             Performs a search in view’s container and returns the
                first item in the container whose nickname is name.
                For example, given a dialog item view, it performs a
                search in the view that is view’s container to find
                another item with the nickname name.  The items are
                searched in the order in which they were added to
                view’s container.

VIEW:           A simple view.

NAME:           Any object, but usually a symbol.  Nicknames are
                compared using EQL.
")
  (:method ((view simple-view) name)
    (let ((container (view-container view)))
      (and container (view-named name container)))))



(defgeneric find-clicked-subview (view where)
  (:documentation "
RETURN:         The subview of view that contains the point where in
                its click region.  The method for null searches all
                windows for a subview containing where in its click
                region.  This function is similar to
                find-view-containing-point, but FIND-CLICKED-SUBVIEW
                calls POINT-IN-CLICK-REGION-P, and
                FIND-VIEW-CONTAINING-POINT calls
                VIEW-CONTAINS-POINT-P.  The default method of
                POINT-IN-CLICK-REGION-P for views or simple views
                simply calls VIEW-CONTAINS-POINT-P, but users can
                write methods to make views invisible to mouse clicks.

VIEW:           A view or subview.

WHERE:          A point in the local coordinate system of the view’s container.
")
  (:method ((view simple-view) where)
    (declare (ignore where))
    view)
  (:method ((view view) where)
    (loop
      :for subview :across (view-subviews view)
      :when (point-in-click-region-p subview where)
        :do (return (find-clicked-subview subview (convert-coordinates where view subview)))
      :finally (return nil)))
  (:method ((view null) where)
    (map-windows (lambda (w)
                   (when (point-in-click-region-p w where)
                     (return-from find-clicked-subview
                       (find-clicked-subview w (subtract-points where (view-position w))))))
                 :include-windoids t)
    nil))



(defgeneric view-corners (view)
  (:documentation "
RETURN:         Two points, the upper-left and lower-right corners of
                view, in the coordinate system of the container.
                The method for window returns the #(0 0) and the view size.

VIEW:           A simple view or subclass of simple-view.

WINDOW:         A window.
")
  (:method ((view simple-view))
    (let ((pos  (or (view-position view) #@(0 0)))
          (size (if (view-position view)
                    (or (view-size view)  #@(0 0))
                    #@(0 0))))
      (values pos (add-points pos size))))
  (:method ((wind window))
    (values #@(0 0) (view-size wind))))




;; (defun box (min value max)
;;   (min max (max min value)))
;; (declaim (inline box))

(defgeneric view-frame (view)
  (:documentation "
RETURN:  The VIEW rectangle in the view container coordinates.
")
  (:method ((view simple-view))
    (let ((topleft (view-position view)))
      (make-rect topleft (add-points topleft (view-size view))))))


(defgeneric view-bounds (view)
  (:documentation "
RETURN:  The VIEW rectangle in the view coordinates.
")
  (:method ((view simple-view))
    (let ((topleft (view-scroll-position view)))
      (make-rect topleft (add-points topleft (view-size view))))))

(defun erase (window bounds)
  (with-focused-view window
    (with-back-color (or (slot-value window 'back-color) *white-color*)
      (erase-rect* (rect-left bounds) (rect-top bounds) (rect-right bounds) (rect-bottom bounds)))))

(defgeneric invalidate-region (view region &optional erase-p)
  (:documentation "
DO:             The INVALIDATE-REGION generic function focuses on the
                view and calls #_InvalRgn.  If the value of ERASE-P is
                true, the function adds this region to the erase
                region of the window of the view; the next time
                WINDOW-UPDATE-EVENTHANDLER runs, it will be erased.
                If ERASE-P is NIL and the window was created with the
                :ERASE-ANONYMOUS-INVALIDATIONS initarg set to true
                (the default), the function adds this region to the
                window’s explicit invalidation region;
                WINDOW-UPDATE-EVENT-HANDLER will not erase it.  The
                function INVALIDATE-REGION is called by
                INVALIDATE-VIEW and INVALIDATE-CORNERS, and indirectly
                by SET-VIEW-POSITION, SET-VIEW-SIZE, and
                SET-VIEW-CONTAINER.

VIEW:           A simple view.

REGION:         The region to invalidate.

ERASE-P:        A value indicating whether or not to add the
                invalidated view to the erase region of the window of
                the view. The default is NIL.
")
  (:method ((view simple-view) region &optional erase-p)
    ;; TODO: for now we invalidate the region bounds or the view-bounds.
    (let ((window (view-window view)))
      (when window
        
        (let ((bounds (convert-rectangle
                       (if region
                           (region-bounds region)
                           (view-bounds view))
                       view window)))
          #+debug-views
          (format-trace "invalidate-region"
                        (not (not region))
                        :position (point-to-list (view-position view)) :size (point-to-list (view-size view))
                        :bounds (rect-to-list (if region
                                                  (region-bounds region)
                                                  (view-bounds view)))
                        :converted (rect-to-list bounds))
          (when erase-p (erase window bounds))
          (needs-to-draw-rect window bounds))))

    #-(and)
    (let* ((wptr (wptr view)))
      (when wptr
        (let* ((window (view-window view))
               (view-clip-region (and window (view-clip-region view))))    
          (when (and window view-clip-region)
            (with-focused-view view         
              (let* ((rgn *temp-rgn*)
                     (update-rgn *temp-rgn-2*)
                     ;; (window (view-window view)) ;; redundant - but why did it cause a problem????
                     (invalid-rgn (window-invalid-region window))
                     (org (view-origin view))
                     (offset (unless (eql #@(0 0) org) (subtract-points (view-origin window) org))))
                (#_SectRgn region view-clip-region rgn)
                (let ((erase-rgn (window-erase-region window)))
                  (when erase-rgn
                    (when offset (#_offsetrgn rgn (point-h offset)(point-v offset))) ; to window coords
                    (when erase-p
                      (#_UnionRgn rgn erase-rgn erase-rgn))                   
                    (get-window-updatergn wptr update-rgn)
                    (let ((offset (subtract-points #@(0 0) (view-position window))))
                      (#_OffsetRgn update-rgn (point-h offset)(point-v offset)))
                    (when invalid-rgn
                      (#_DiffRgn update-rgn invalid-rgn update-rgn))
                    (#_UnionRgn update-rgn erase-rgn erase-rgn))
                  (when offset
                    (let ((now-offset (subtract-points #@(0 0) offset)))
                      (#_offsetrgn rgn (point-h now-offset)(point-v now-offset)))))
                (#_invalwindowrgn wptr rgn)
                (when invalid-rgn                 
                  (let ((rgn3 *temp-rgn-3*))
                    (get-window-visrgn wptr rgn3)
                    (#_sectrgn rgn3 rgn rgn))
                                        ; view coordinates
                  (when offset (#_offsetrgn  rgn (point-h offset)(point-v offset))) ; to window coords
                  (#_UnionRgn rgn invalid-rgn invalid-rgn))))))))
    (values))
  
  (:method ((window window) region &optional erase-p)
    (let ((bounds (if region
                      (region-bounds region)
                      (view-bounds window))))
      (when erase-p (erase window bounds))
      (needs-to-draw-rect window bounds))
    (values)))


(defgeneric invalidate-corners (view topleft bottomright &optional erase-p)
  (:documentation "
DO:             Invalidate the rectangle formed by topleft and bottomright in VIEW.

VIEW:           A simple view.

TOPLEFT:        The upper-left corner of the rectangle to invalidate.

BOTTOMRIGHT:    The lower-right corner of the rectangle to invalidate.

ERASE-P:        A value indicating whether or not to add the
                invalidated rectangle to the erase region of the
                window of the view.  The default is NIL.
")

  (:method ((view simple-view) topleft bottomright &optional erase-p)
    (let ((rgn *temp-rgn*)
          (left   (min 32767 (max -32768 (point-h topleft))))
          (top    (min 32767 (max -32768 (point-v topleft))))
          (right  (min 32767 (max -32768 (point-h bottomright))))
          (bottom (min 32767 (max -32768 (point-v bottomright)))))
      (set-rect-region rgn left top right bottom)
      (invalidate-region view rgn erase-p))
    (values)))


(defgeneric invalidate-view (view &optional erase-p)
  (:documentation "
DO:             Invalidate VIEW by running INVALIDATE-CORNERS on the
                region bounded by its view-corners.

VIEW:           A view or simple view.

ERASE-P:        A value indicating whether or not to add the
                invalidated region to the erase region of view’s
                window.  The default is NIL.
")
  (:method ((view simple-view) &optional erase-p)
    (invalidate-corners view #@(0 0) (view-size view) erase-p))
  (:method ((window window) &optional erase-p)
    (invalidate-corners window #@(0 0) (view-size window) erase-p)))




(defgeneric validate-region (view region)
  (:documentation "

DO:             Focus on the view and removes the region from view’s
                window erase region and explicit invalidation region.

VIEW:           A simple view.

REGION:         A region. The region must be a region handle, that is,
                the result of (new-region).

")
  (:method ((view simple-view) region)
    (declare (ignore region))
    ;; TODO
    (values)))


(defgeneric validate-corners (view topleft bottomright)
  (:documentation "
DO:             Erase the previous contents of the rectangle formed by
                topleft and bottomright and calls #_ValidRgn on the
                rectangle.  It also removes the rectangle from the
                erase region of the view of the view.

VIEW:           A view or simple view.

TOPLEFT:        The upper-left corner of the view to invalidate.

BOTTOMRIGHT:    The lower-right corner of the view to invalidate.
")
  (:method ((view simple-view) topleft bottomright)
    (let ((rgn *temp-rgn*))
      (set-rect-region rgn
                       (point-h topleft) (point-v topleft)
                       (point-h bottomright) (point-v bottomright))
      (validate-region view rgn))))


(defgeneric validate-view (view)
  (:documentation "
DO:             Validates view by running validate-corners on the
                region bounded by its view-corners.

VIEW:           A view or simple view.
")
  (:method ((view simple-view))
    (multiple-value-bind (topleft bottomright) (view-corners view)
      (let ((container (view-container view)))
        (unless container
          (setf container view)
          (unless (typep view 'window)
            (let ((pos (view-position view)))
              (setq topleft     (subtract-points topleft     pos)
                    bottomright (subtract-points bottomright pos)))))
        (validate-corners container topleft bottomright)))))



(defgeneric maybe-erase (view)
  (:method ((view simple-view))
    nil)
  (:method ((view view))
    (when (view-window view)
      (window-invalid-region (view-window view))))
  (:method ((view window))
    nil))


(defgeneric set-view-position (view h &optional v)
  (:documentation "
DO:             Set the position of the view in its container.  The
                positions are given in the container’s coordinate
                system.

VIEW:           A view or simple view.

H:              The horizontal coordinate of the new position, or the
                complete position (encoded as a point) if V is NIL or
                not supplied.

V:              The vertical coordinate of the new position, or NIL if
                the complete position is given by H.

RETURN:         (make-point h v)
")
  (:method ((view simple-view) h &optional v)
    (let ((pos (make-point h v)))
      (unless (eql pos (view-position view))
        (invalidate-view view t)         
        (setf (%view-position view) pos)
        (make-view-invalid view)
        (invalidate-view view (maybe-erase view)))
      (refocus-view view)
      pos)))

#+pjb-debug
(defmethod (setf %view-position) :around (new-position view)
  ;; (when (= 0 new-position) (break "set-view-position (0 0) !"))
  (format-trace '(setf %view-position) (point-to-list new-position) view)
  (prog1 (call-next-method)
    (format-trace '(setf %view-position) (point-to-list (view-position view)) view)))

(defgeneric set-view-size (view h &optional v)
  (:documentation "
DO:             Set the size of the view.

VIEW:           A simple view or subclass of simple-view.

H:              The width of the new size, or the complete size
                (encoded as an integer) if V is NIL or not supplied.

V:              The height of the new size, or NIL if the complete
                size is given by H.
")
  (:method ((view simple-view) h &optional v)
    (let ((siz (make-point h v)))
      (unless (eql siz (view-size view))
        (invalidate-view view t)
        (setf (slot-value view 'view-size) siz)
        (make-view-invalid view)
        (invalidate-view view t))
      (refocus-view view)
      siz)))


(defgeneric view-default-position (view)
  (:documentation "
DECRIPTION:     When a window is created, the VIEW-DEFAULT-POSITION
                generic function is called if no position is
                explicitly specified either as the :VIEW-POSITION
                initialization argument to MAKE-INSTANCE or as a
                default initialization argument in the class
                definition.  The value returned is used as the initial
                position of the window.  It must be a valid position
                specifier, either a point or a centering specifier as
                documented under SET-VIEW-POSITION.

RETURN:         The system-supplied method specialized on WINDOW
                returns the value of *WINDOW-DEFAULT-POSITION*.

RETURN:         The method of VIEW-DEFAULT-POSITION for simple-view
                returns #@(0 0). This function is called to determine
                the default value of the :view-position initarg of
                view.

VIEW:           A simple-view or subclass of simple-view.
")
  (:method ((view simple-view))
    #@(0 0)))


(defgeneric view-default-size (view)
  (:documentation "
DESCRIPTION:    When a window is created, the VIEW-DEFAULT-SIZE
                generic function is called if no size is explicitly
                specified either as the :VIEW-SIZE initialization
                argument to make-instance or as a default
                initialization argument in the class definition.  The
                value returned is used as the initial size of the
                window.  It must be a point.

RETURN:         The method of view-default-size for simple-view
                returns #@(100 100). This function is called to
                determine the default value of the :viewsize initarg
                of view.

RETURN:         The system-supplied method specialized on WINDOW
                returns the value of *WINDOW-DEFAULT-SIZE*.

VIEW:  A simple view or subclass of simple-view.
")
  (:method ((view simple-view))
    #@(100 100)))


(defgeneric set-view-scroll-position (view h &optional v scroll-visibly)
  (:documentation "
DO:             Set the position of the view’s scroll position. It is
                usually called in response to a mouse click in a
                scroll bar.

VIEW:           A simple view or subclass of simple-view.

H:              The horizontal coordinate of the new scroll position,
                or the complete scroll position (encoded as a point)
                if V is NIL or not supplied.

V:              The vertical coordinate of the new scroll position, or NIL
                if the complete scroll position is given by H.

SCROLL-VISIBLY: An argument specifying whether the scrolling is done
                immediately. If true, the function calls #_ScrollRect to
                do the scrolling immediately.  Otherwise, the function
                invalidates the view so that it is redrawn the next time
                WINDOW-UPDATE-EVENT-HANDLER is called.

NOTE:           H and V are in VIEW's coordinates.

RETURN:         (make-point h v)
")
  (:method ((view simple-view) h &optional v scroll-visibly)
    (declare (ignore scroll-visibly))
    (make-point h v))
  (:method ((view view) h &optional v (scroll-visibly t))
    (let* ((pt         (make-point h v))
           ;; (container  (view-container view))
           (old-sc-pos (view-scroll-position view))
           (delta      (subtract-points old-sc-pos pt)))
      (with-focused-view view
        (unless (eql delta #@(0 0))
          (if scroll-visibly
              (scroll-rect view (view-bounds view) delta)
              (invalidate-view view t))))
      (setf (%view-scroll-position view) pt)
      (make-view-invalid view)
      (refocus-view view)
      pt)))



(defgeneric set-view-nick-name (view new-name)
  (:documentation "
DO:             Set the nickname of VIEW to NEW-NAME.

VIEW:           A view or simple-view.

NEW-NAME:       A name, usually a symbol or string.
")
  (:method ((view simple-view) new-name)
    (setf (slot-value view 'view-nick-name) new-name)))




(defgeneric view-contains-point-p (view point)
  (:documentation "
RETURN:         Whether VIEW contains POINT.  The method for
                simple-view takes POINT in the coordinates of the
                container view; the method for window uses its own
                coordinates.
")
  (:method ((view simple-view) point)
    (let* ((position (view-position view))
           (ph       (point-h position))
           (h        (point-h point)))
      (and (<= ph h)
           (let ((pv    (point-v position))
                 (v     (point-v point)))
             (and (<= pv v)
                  (let ((size  (view-size view)))
                    (and (< h  (+ ph (point-h size)))
                         (< v  (+ pv (point-v size))))))))))
  (:method ((window window) point)
    (and (point<= 0 point) (point<= point (view-size window)))))



(defun %convert-to-window (view pt)
  (if (or (typep view 'window)
          (null (view-container view)))
      (add-points (view-scroll-position view) pt)
      (%convert-to-window (view-container view) (add-points (view-scroll-position view) pt))))


(defun convert-coordinates (point source-view destination-view)
  "
The CONVERT-COORDINATES function converts point from the coordinate
system of SOURCE-VIEW to the coordinate system of DESTINATION-VIEW.
The source view and destination view should be in the same view
hierarchy (that is, they should have a common container, or one should
be contained in the other).

POINT:          A point, encoded as an integer.

SOURCE-VIEW:    A view in whose coordinate system point is given.
"
  (declare (stepper disable))
  (add-points point (subtract-points (view-origin destination-view)
                                     (view-origin source-view))))


(defun convert-rectangle (rect source-view destination-view)
  (declare (stepper disable))
  (let ((delta   (subtract-points (view-origin destination-view)
                                  (view-origin source-view))))
    (make-rect (add-points delta (rect-topleft rect))
               (add-points delta (rect-bottomright rect)))))


(defgeneric find-view-containing-point (view h &optional v direct-subviews-only)
  (:documentation "
RETURN:         The view containing the point specified by H and
                V.  This may be the VIEW or one of its subviews.  The
                NULL method searches all windows for a view that
                contains the point.

VIEW:           A view.

H:              The horizontal coordinate of the point, or the complete
                point if V is not supplied.

V:              The vertical coordinate of the point.

DIRECT-SUBVIEWS-ONLY:
                If DIRECT-SUBVIEWS-ONLY is NIL (the default), the most
                specific view is returned; subviews are searched for
                subviews, and so on.  If true, then only the view or
                one of its direct subviews is returned.
")

  (:method ((view simple-view) h &optional v
                                   (direct-subviews-only nil))
    (declare (ignore h v))
    (unless direct-subviews-only
      view))

  (:method ((view view) h &optional v (direct-subviews-only nil))
    (let* ((point (make-point h v))
           (subviews (view-subviews view)))
      (loop
        :for subview :across subviews
        :when (view-contains-point-p subview point)
          :do (return-from find-view-containing-point
                (if direct-subviews-only
                    subview
                    (find-view-containing-point
                     subview
                     (convert-coordinates point view subview)
                     nil
                     nil)))))
    (unless direct-subviews-only
      view))

  (:method ((view null) h &optional v (direct-subviews-only nil))
    (let ((point (make-point h v)))
      (map-windows (lambda (window)
                     (when (view-contains-point-p window point)
                       (return-from find-view-containing-point
                         (if direct-subviews-only
                             window
                             (find-view-containing-point 
                              window
                              (subtract-points point (view-position window)))))))
                   :include-windoids t)
      nil)))

#+pjb-debug
(defmethod find-view-containing-point :around (view h &optional v direct-subviews-only)
  (declare (ignorable view h v direct-subviews-only))
  (let ((result (call-next-method)))
    (format-trace 'find-view-containing-point result)
    (with-focused-view result
      (with-pen-state (:pattern *black-pattern* :mode :srcCopy)
        (fill-rect* 0 0 (point-h (view-size result)) (point-v (view-size result)))))
    result))

(defmethod find-view-containing-point :around (view h &optional v direct-subviews-only)
  (declare (ignorable view h v direct-subviews-only))
  (let ((result (call-next-method)))
    #+debug-views
    (format-trace 'find-view-containing-point result
                  (subviews result))
    result))





(defgeneric point-in-click-region-p (view where)
  (:documentation "
The generic function point-in-click-region-p is called by
VIEW-CLICK-EVENT-HANDLER to determine whether where is in view. The
default method calls VIEW-CONTAINS-POINT-P.

VIEW:           A simple view or view.

WHERE:          For a view, the cursor position of the view in the
                local coordinate system when the mouse is clicked. For
                a simple view, the cursor position of the simple view
                in the local coordinate system of the view’s container
                when the mouse is clicked.
")
  (:method ((view simple-view) where)
    (view-contains-point-p view where)))


(defgeneric view-convert-coordinates-and-click (view where container)
  (:documentation "
DO:             Run VIEW-CLICK-EVENT-HANDLER on the cursor position
                within the view’s container.

VIEW:           A simple view or view.

WHERE:          For a view, the mouse click position (the position
                when the mouse is clicked) of the view in the local
                coordinate system.  For a simple view, the mouse click
                position of the simple view in the local coordinate
                system of the view’s container.

CONTAINER:      The container of the view.
")
  (:method ((view simple-view) where container)
    (declare (ignore container))
    (view-click-event-handler view where))

  (:method ((view view) where container)
    (view-click-event-handler view (convert-coordinates where container view))))



(defmacro with-view-frame ((x y w h) view &body body)
  (let ((vpos (gensym))
        (vsiz (gensym))
        (vview (gensym)))
    `(let* ((,vview ,view)
            (,vpos (view-position ,vview))
            (,vsiz (view-size ,vview))
            (,x (point-h ,vpos))
            (,y (point-v ,vpos))
            (,w (point-h ,vsiz))
            (,h (point-v ,vsiz)))
       ,@body)))



(defun %view-draw-contents-with-focused-view (view focused-view visrgn cliprgn)
  (declare (ignore visrgn cliprgn)) ; for now ; we could compute a clip rect.
  (unless *deferred-drawing*
   (let ((window (view-window view)))
     (when window #-(and) (and window
                               (with-handle (winh window)
                                 [winh isVisible]))
           (with-focused-view focused-view
             (view-draw-contents view))
           #-(and) (with-temp-rgns (visrgn cliprgn)
                     (get-window-visrgn wptr visrgn)
                     (get-window-cliprgn wptr cliprgn)
                     (when (regions-overlap-p visrgn cliprgn)
                       (view-draw-contents view)))))))

(defgeneric view-focus-and-draw-contents (view &optional visrgn cliprgn)
  (:documentation "
The generic function VIEW-FOCUS-AND-DRAW-CONTENTS is used whenever a
view needs to be focused on before any portion of its contents is
redrawn. The method for VIEW focuses on the view, then calls
VIEW-DRAW-CONTENTS if the VISRGN and CLIPRGN region records
overlap. The method for SIMPLE-VIEW focuses on the view’s container,
then calls VIEW-DRAW-CONTENTS.

VIEW:           A simple view or view.
VISRGN, CLIPRGN Region records from the view’s wptr.
")
  (:method ((view simple-view) &optional visrgn cliprgn)
    (%view-draw-contents-with-focused-view view (view-container view) visrgn cliprgn))

  (:method ((view view) &optional visrgn cliprgn)
    (%view-draw-contents-with-focused-view view view visrgn cliprgn)))


(defmethod view-draw-contents :before ((view simple-view))
  #+debug-views
  (format-trace 'view-draw-contents
                :already-current (eql view *current-view*)
                :view view
                :current *current-view*))

(defmethod view-draw-contents ((view simple-view))
  (values))

(defmethod view-draw-contents ((view view))
  ;; bug for bug compatibility :-(
  (when (wptr view)
    (dovector (subview (view-subviews view))
      (view-focus-and-draw-contents subview))
    (call-next-method)))



(defgeneric view-valid-p (view)
  (:method ((view simple-view))
    t)
  (:method ((view view))
    (not (member nil (view-valid view)))))


(defgeneric make-view-invalid (view)
  (:method ((view simple-view))
    view)
  (:method ((view view))
    (let ((valid (view-valid view)))
      (when (and valid (car valid))
        (setf (car valid) nil)))
    view))


(defgeneric make-view-valid (view &optional dont-inval-subviews)
  (:method ((view simple-view) &optional dont-inval-subviews)
    (declare (ignore dont-inval-subviews))
    view)
  (:method ((view view) &optional dont-inval-subviews)
    (let ((valid (view-valid view)))
      (unless (or (null valid) (car valid))
        (setf (car valid) t)
        (unless dont-inval-subviews
          (loop
            :for subview :across (view-subviews view)
            :do (make-view-invalid subview)))))
    view))


(defmethod view-font-codes ((view simple-view))
  (let ((codes (view-get view 'view-font-codes)))
    (if codes
        (values (car codes) (cdr codes))
        (let ((container (view-container view)))
          (and container (view-font-codes container))))))

(defmethod set-view-font-codes ((view simple-view) ff ms &optional ff-mask ms-mask)
  (let ((codes (view-get view 'view-font-codes)))
    (if codes
        (let* ((old-ff (car codes))
               (old-ms (cdr codes))
               (ff (if ff-mask
                       (logior (logand ff ff-mask) 
                               (logandc2 old-ff  ff-mask))
                       ff))
               (ms (if ms-mask
                       (logior (logand ms ms-mask) 
                               (logandc2 old-ms ms-mask))
                       ms)))
          (rplacd (rplaca codes ff) ms))
        (view-put view 'view-font-codes (cons ff ms)))
    (values ff ms)))



(defmethod view-font ((view simple-view))
  (multiple-value-bind (ff ms) (view-font-codes view)
    (font-spec ff ms)))

(defmethod set-view-font ((view simple-view) font-spec)
  (multiple-value-bind (ff ms) (view-font-codes view)
    (multiple-value-bind (ff ms) (font-codes font-spec ff ms)
      (set-view-font-codes view ff ms)))
  font-spec)


(defgeneric view-default-font (view)
  (:documentation "
DESCRIPTION:    If a :VIEW-FONT initialization argument is not
                specified when a view is created, the generic function
                VIEW-DEFAULT-FONT is called to determine its font.
                Every window has a font spec associated with it, even
                if the window never uses fonts.

RETURN:         The WINDOW method on VIEW-DEFAULT-FONT returns the
                value of *DEFAULT-FONT-SPEC*.

RETURN:         The SIMPLE-VIEW method returns NIL, meaning
                that the view inherits its font from its container.

WINDOW:         A window.

VIEW:           A simple view.
")
  (:method ((view simple-view))
    nil))



(defgeneric view-font-codes-info (view)
  (:method ((view simple-view))
    (multiple-value-call (function font-codes-info) (view-font-codes view))))

(defgeneric set-initial-view-font (view font-spec)
  (:method ((view simple-view) font-spec)
    (set-view-font view font-spec)))



(defgeneric view-cursor (view point)
  (:documentation "

The VIEW-CURSOR generic function determines the cursor shape whenever
the window containing the view is active and the cursor is over it.
The VIEW-CURSOR function is called by WINDOW-UPDATE-CURSOR.

VIEW:           A view or simple view.

POINT:          The position of the cursor, expressed as a point.

RETURN:         The cursor shape to display when the mouse is at
                point, a point in view. It is called by
                WINDOW-UPDATE-CURSOR as part of the default
                WINDOW-NULL-EVENT-HANDLER.  Specialize the view-cursor
                generic function to change your view’s cursor to one
                of the following predefined cursors or to a
                user-defined cursor.

                *ARROW-CURSOR* The standard north-northwest arrow
                cursor.

                *I-BEAM-CURSOR* The I-beam shape used when the cursor
                is over an area of editable text.

                *WATCH-CURSOR* The watch-face shape shown during
                time-consuming operations, when event processing is
                disabled.

")
  (:method ((view simple-view) point)
    (let ((container (view-container view)))
      (if container
          (view-cursor container (convert-coordinates point view container))
          *arrow-cursor*))))



(defun inset-corners (inset topleft bottomright)
  (values (add-points inset topleft) (subtract-points bottomright inset)))


;;;---------------------------------------------------------------------
;;; Internal functions.

(defun view-is-invalid-p (view visrgn cliprgn)
  (or (null visrgn)
      (null cliprgn)
      (multiple-value-bind (tl br) (view-corners view)
        (niy view-is-invalid-p view visrgn cliprgn tl br)
        ;; (without-interrupts
        ;;   ;; (let ((rgn *temp-rgn*)) ; so *temp-rgn* belongs to us
        ;;   ;;   (#_SetRectRgn rgn (point-h tl)(point-v tl) (point-h br)(point-v br))
        ;;   ;;   (#_SectRgn rgn visrgn rgn)
        ;;   ;;   (#_SectRgn rgn cliprgn rgn)                   
        ;;   ;;   (not (#_EmptyRgn rgn)))
        ;; t)
        t)))


(defgeneric frame-key-handler (view)
  (:method ((view simple-view))
    view))

(defgeneric invalidate-view-border (view &optional erase-p right-and-bottom-only))

(defmethod invalidate-view-border ((view simple-view) &optional erase-p right-and-bottom-only)
  (niy view erase-p right-and-bottom-only) #-(and)
  (when (wptr view)
    (let* ((container (or (view-container view) view))
           (ul (view-position view))
           (lr (add-points ul (view-size view)))
           (ul-h (point-h ul))
           (ul-v (point-v ul))
           (lr-h (point-h lr))
           (lr-v (point-v lr)))
      (multiple-value-bind (bul blr) (view-corners view)
        (unless (and (eql ul bul) (eql lr blr))
          (with-focused-view container
            (without-interrupts
              (let* ((rgn *temp-rgn*)
                     (rgn2 *temp-rgn-2*)
                     (bul-h (point-h bul))
                     (bul-v (point-v bul))
                     (blr-h (point-h blr))
                     (blr-v (point-v blr))
                     rgn-ul-h rgn-ul-v)
                (if right-and-bottom-only
                    (setq rgn-ul-h ul-h rgn-ul-v ul-v)
                    (setq rgn-ul-h bul-h rgn-ul-v bul-v))
                (#_SetRectRgn rgn rgn-ul-h rgn-ul-v blr-h blr-v)
                (#_SetRectRgn rgn2 ul-h ul-v lr-h lr-v)
                (#_DiffRgn rgn rgn2 rgn)
                #-carbon-compat
                (#_InvalRgn rgn)
                #+carbon-compat
                (inval-window-rgn (wptr view) rgn)
                (when erase-p
                  (let ((org (view-origin container))
                        (erase-rgn (window-erase-region (view-window container))))
                    (when erase-rgn
                      (unless (eql #@(0 0) org)
                        (#_OffsetRgn rgn (- (point-h org)) (- (point-v org))))
                      (#_UnionRgn rgn erase-rgn erase-rgn))))))))))))



;;; ------------------------------------------------
;;; instance drawing
;;; ------------------------------------------------


(defun focused-screenshot (view)
  (with-focused-view (view-window view)
    (with-view-handle (viewh view)
      (let ((image [[[NSImage alloc] initWithSize:(unwrap (size-to-nssize (view-size view)))] autorelease])
            bitmap)
        [viewh lockFocus]
        (unwind-protect
             ;; [viewh bounds]
             (setf bitmap [[[NSBitmapImageRep alloc] initWithFocusedViewRect:(unwrap (rect-to-nsrect (convert-rectangle (view-bounds view)
                                                                                                                        view
                                                                                                                        (view-window view))))] autorelease])
          [viewh unlockFocus])
        [image addRepresentation:bitmap]
        #+debug-views
        (format-trace 'focused-screenshot
                      :size (point-to-list  (view-size view))
                      :from-rect (rect-to-list (convert-rectangle (view-bounds view)
                                                                  view
                                                                  (view-window view)))
                      :ctm (get-ctm (view-window view))
                      :viewh viewh
                      :bitmap bitmap
                      :image image)
        #-cocoa-10.6 [image setFlipped:YES]
        image))))

(defmacro with-instance-drawing (view &body body)
  (let ((vview (gensym)))
    `(let ((,vview ,view))
       (push (focused-screenshot ,vview) (view-instance ,vview))
       (unwind-protect (progn ,@body)
         (pop (view-instance ,vview))))))


(defun new-instance (view)
  (when (view-instance view)
    (with-focused-view view
      (with-view-handle (viewh view)
        #-cocoa-10.6
        [(first (view-instance view))
         drawInRect:[viewh bounds]
         fromRect:(unwrap (make-nsrect :x 0 :y 0 :size (view-size view)))
         operation:#$NSCompositeCopy
         fraction:(cgfloat 1.0)]
        #+debug-views
        (format-trace 'new-instance :before
                      :frame (get-nsrect [viewh frame])
                      :bounds (get-nsrect [viewh bounds]))
        #+debug-views
        (format-trace 'new-instance
                      :instance (first (view-instance view)))
        #+cocoa-10.6
        [(first (view-instance view))
         drawInRect:[viewh bounds]
         fromRect:(unwrap (make-nsrect :x 0 :y 0 :size (view-size view)))
         operation:#$NSCompositeCopy
         fraction:(cgfloat 1.0)
         respectFlipped:yes
         hints: *null*]
        #+debug-views
        (format-trace 'new-instance :after_
                      :frame (get-nsrect [viewh frame])
                      :bounds (get-nsrect [viewh bounds]))))))


(defun example/instance-drawing ()
  (let ((view (front-window)))
    (with-instance-drawing view
      (loop for i from 20 to 200 by 10 do
        (sleep 0.1)
        (new-instance view)
        (with-focused-view view
          (draw-line 20 i 100 200)))
      (new-instance view))))

(defun example/instance-drawing/2 ()
  (let ((view (front-window)))
    (with-instance-drawing view
      (with-focused-view view
        (draw-line 20 20 200 100))
      (sleep 5)
      (new-instance view))))

;; (example/instance-drawing/2)
;; (example/instance-drawing)

(defun initialize/view ()
  (niy initialize/view))




;;;; THE END ;;;;
