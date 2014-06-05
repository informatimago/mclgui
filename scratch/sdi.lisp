(ql:quickload :mclgui)
(in-package :ui)
(initialize)


(defclass sdi-window (window)
  ((position-item :initform '() :accessor position-item)))

(defmethod set-view-position :after ((win sdi-window) h &optional v)
  (let ((pos  (make-point h v))
        (posi (position-item win)))
    (when posi
      (set-dialog-item-text posi (print (format nil "~S ~S"
                                                  (point-to-list pos)
                                                  (point-to-list (view-position win))))))))

(defvar *w* (make-instance 'sdi-window :window-title "Test"))


(defgeneric draw-view-bounds (view)
  (:method   ((view simple-view))
    #-(and) (progn (format t "~&view ~A~%" (view-nick-name view))
                   (format t "~&  frame  = ~S~%" (rect-to-list (view-frame view)))
                   (format t "~&  bounds = ~S~%" (rect-to-list (view-bounds view)))
                   (finish-output))
    (let* ((bounds (view-bounds view))
           (x (rect-left   bounds))
           (y (rect-top    bounds))
           (w (rect-width  bounds))
           (h (rect-height bounds)))
      (draw-rect* x y w h))))


(defgeneric draw-view-frame (view)
  (:method   ((view simple-view))
    #-(and) (progn (format t "~&frame  = ~S~%" (rect-to-list (view-frame view)))
                   (format t "~&bounds = ~S~%" (rect-to-list (view-bounds view)))
                   (finish-output))
    (let* ((frame (view-frame view))
           (x (rect-left   frame))
           (y (rect-top    frame))
           (w (rect-width  frame))
           (h (rect-height frame)))
      (draw-rect* x y w h))))


(defclass color-box (view)
  ((color :initarg :color :initform *black-color*  :accessor color)))

(defmethod view-draw-contents ((view color-box))
  (with-focused-view view
    (with-fore-color (color view)
      (let* ((bounds (view-bounds view))
             (x (rect-left   bounds))
             (y (rect-top    bounds))
             (w (rect-width  bounds))
             (h (rect-height bounds)))
        (fill-rect* x y w h)))
    (call-next-method)))

(defmethod view-draw-contents :after ((view color-box))
  (draw-view-bounds view))

(defun test-color-box ()
  (apply (function remove-subviews) *w* (coerce (view-subviews *w*) 'list))

  (let ((red (make-instance
              'color-box 
              :color *red-color*
              :view-position (make-point 20 10)
              :view-size     (make-point 100 20)
              :view-nick-name "red"))
        (blue (make-instance
               'color-box 
               :color *blue-color*
               :view-position (make-point 2 2)
               :view-size     (make-point 12 12)
               :view-nick-name "blue"))
        (green (make-instance
                'color-box 
                :color *green-color*
                :view-position (make-point 2 2)
                :view-size     (make-point 12 12)
                :view-nick-name "green")))
    (add-subviews red blue)
    (add-subviews *w* red)
    (add-subviews *w* green)))

#-(and) (progn

          (test-color-box)

          (with-focused-view (front-window)
            (draw-rect* 2 2 12 12)
            (draw-rect* 20 10 100 20))

          (let ((view (aref (view-subviews (front-window)) 0)))
           (view-draw-contents view))

          (let ((view (aref (view-subviews (front-window)) 0)))
            (with-focused-view view
              (view-draw-contents view)))

          (let ((v1 (aref (view-subviews (front-window)) 0))
                (v2 (aref (view-subviews (front-window)) 1)))
            (with-focused-view v1
              (view-draw-contents v1)
              (draw-rect* 0 0 100 20)
              (with-focused-view v2
                (view-draw-contents v2)
                (draw-rect* 0 0 12 12))))

          (let ((v (aref (view-subviews *w*) 0)))
            (list
             (rect-to-list (convert-rectangle (view-bounds v) v *w*))
             (point-to-list (set-view-position v 50 10))
             (rect-to-list (convert-rectangle (view-bounds v) v *w*))))


          (let* ((v1 (aref (view-subviews *w*) 0))
                 (v2 (aref (view-subviews v1) 0)))
            (loop for i from 10 to 50
                  do (set-view-position v1 (- 60 i) (+ 10 i))
                     (set-view-position v2 (+ 2 i) 2)))

          (let* ((v1 (aref (view-subviews *w*) 0))
                 (v2 (aref (view-subviews v1) 0)))
            (set-view-position v2 50 4))

          (point-to-list (view-scroll-position *w*))
          (set-view-scroll-position *w* -10 0)
          (view-draw-contents *w*)
          (mapcar (function point-to-list)
                  (list
                   (convert-coordinates #@(0 0)  (aref (view-subviews *w*) 0) *w*)
                   (convert-coordinates #@(0 0)  (aref (view-subviews *w*) 1) *w*)
                   (convert-coordinates #@(0 0)  (aref (view-subviews *w*) 0) (aref (view-subviews *w*) 1))))
          ((20 40) (2 2) (18 38))
          
          (pprint (dump *w*))

          

          (import '(com.informatimago.common-lisp.cesarum.utility:/apply
                    com.informatimago.common-lisp.cesarum.utility:compose))

          (map 'list (/apply (compose point-to-list view-origin)
                             (compose point-to-list view-position)
                             (compose point-to-list view-scroll-position))
            (view-subviews *w*))

          )


;;;--------------------------------------------------------------------

(defclass boxed-static-text-dialog-item (static-text-dialog-item)
  ())


(defmethod view-draw-contents :after ((item boxed-static-text-dialog-item))
  (with-focused-dialog-item (item)
    (draw-view-frame item)))


(defclass boxed-editable-text-dialog-item (editable-text-dialog-item)
  ())

(defmethod view-draw-contents :after ((item boxed-editable-text-dialog-item))
  (with-focused-dialog-item (item)
    (draw-view-frame item)))


(defun test-text-box ()
  (apply (function remove-subviews) *w* (coerce (view-subviews *w*) 'list))
  (add-subviews *w*

                (setf (position-item *w*) (make-instance
                                           'boxed-static-text-dialog-item
                                           :dialog-item-text "positions"
                                           :view-position (make-point 20 100)
                                           :view-size     (make-point 200 20)
                                           :view-nick-name "positions"))

                (make-instance
                 'color-box 
                 :color *blue-color*
                 :view-position (make-point 18 8)
                 :view-size     (make-point 104 24)
                 :view-nick-name "blue 1")

                (make-instance
                 'color-box 
                 :color *blue-color*
                 :view-position (make-point 18 28)
                 :view-size     (make-point 104 24)
                 :view-nick-name "blue 2")

                (make-instance
                 'color-box 
                 :color *blue-color*
                 :view-position (make-point 18 48)
                 :view-size     (make-point 104 24)
                 :view-nick-name "blue 3")

                
                (make-instance
                 'boxed-static-text-dialog-item
                 :dialog-item-text "STATIC TEXT"
                 :dialog-item-action (lambda (item)
                                       (format t "~&~S~%~S~2%" item (dialog-item-text item)))
                 :view-position (make-point 20 10)
                 :view-size     (make-point 100 20)
                 :view-nick-name "static text")

                (make-instance
                 'boxed-editable-text-dialog-item
                 :dialog-item-text "EDIT IT"
                 :dialog-item-action (lambda (item)
                                       (format t "~&~S~%~S~2%" item (dialog-item-text item)))
                 :view-position (make-point 20 30)
                 :view-size     (make-point 100 20)
                 ::view-nick-name "edit text")

                (make-instance
                 'boxed-editable-text-dialog-item
                 :dialog-item-text "Another edit"
                 :dialog-item-action (lambda (item)
                                       (format t "~&~S~%~S~2%" item (dialog-item-text item)))
                 :view-position (make-point 20 50)
                 :view-size     (make-point 100 20)
                 :view-nick-name "another edit text")


                )
  (invalidate-view *w*))


(defgeneric first-responder (w)
  (:method   ((w window))
    (with-handle (winh w)
      [winh firstResponder])))

#-(and)(progn
         (identify-streams)
          
         (invalidate-view *w*)
         (test-text-box)
         (position-item *w*)
         (set-view-position (front-window) 0 180)

         (map 'list (lambda (view) (list (point-to-list (convert-coordinates #@(0 0) view (view-window view)))
                                         (point-to-list (view-position view))))
           (view-subviews(first(windows))))
         

         (load "scratch/dump.lisp")
         (pprint (dump (front-window)))

                  
         (set-view-position (aref (view-subviews (front-window)) 0) 20 40)
         
         (point-to-list (view-position (aref (view-subviews (front-window)) 0)))
          (view-focus-and-draw-contents (front-window))

         (values (first-responder  (first (windows)))
                 (handle))


         (values (map 'list 'dialog-item-text
                   (view-subviews (first (windows))))
                 (map 'list (lambda (x) (with-handle (h x) (objcl:lisp-string [h stringValue])))
                   (view-subviews (first (windows)))))


         (map 'list (lambda (x) (list (class-name (class-of x)) (dialog-item-enabled-p x)))
           (view-subviews *w*))
         ((boxed-static-text-dialog-item nil) (boxed-editable-text-dialog-item t))

         (view-draw-contents (aref (view-subviews *w*) 0))
         (invalidate-view (aref (view-subviews *w*) 0))
         (remove-subviews *w* (aref (view-subviews *w*) 0))

         (view-font (front-window))

         (setf *descriptor-cache* (make-descriptor-cache))
         (set-view-font (front-window) '("Monaco"  9  :srcor))
         (with-font-focused-view (front-window)
           (erase-rect* 10 100 200 200)
           (draw-text 10 100 200 200 "Hao Wang, logicien americain.")
           (erase-rect* 10 100 200 200)
           (draw-text 10 100 200 200 "Hao Wang, logicien americain.

L'algorithme en  question  a  été  publié  en  1960  dans l'IBM Journal,
article intitule \"Toward  Mechanical Mathematics\", avec des variantes et
une  extension au calcul  des  prédicats.  Il  s'agit  ici  du  \"premier
programme\" de Wang, systeme \"P\".

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704
­ machine à lampes, 32 k  mots  de 36 bits, celle­là même qui vit naître
LISP à la même époque. Le programme  a  été écrit en assembleur (Fortran
existait, mais il ne s'était pas encore imposé)  et  l'auteur estime que
\"there is very little in the program that is not straightforward\".

Il observe que les preuves engendrées sont \"essentiellement des arbres\",
et  annonce  que  la  machine  a  démontre 220 théorèmes du  calcul  des
propositions  (tautologies)  en  3  minutes. Il en tire argument pour la
supériorité  d'une  approche  algorithmique  par  rapport à une approche
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et  Simon (à
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat
qui dure encore...

Cet  algorithme  a  été popularisé par J. McCarthy, comme exemple­fanion
d'application  de LISP. Il figure dans le manuel de la première  version
de  LISP  (LISP  1,  sur IBM 704 justement, le manuel est daté  de  Mars
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\"
publié en 1962 par MIT Press, un des maîtres­livres de l'Informatique.



"))
         
         (window-close *w*)
         )

#-(and) (progn
          (#/CGContextGetCTM)
          (#/CGAffineTransformInvert)
          (#/CGContextConcatCTM)

          )
