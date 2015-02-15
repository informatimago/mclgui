;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-text-edit.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests text-edit.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-15 <PJB> Extracted from text-edit.lisp.
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
(in-package :ui)

;;;---------------------------------------------------------------------
;;; Tests
;;;---------------------------------------------------------------------

(defclass te-test-window (window)
  ((te :initform nil :accessor test-window-te)))

(defmethod window-size-parts ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (let ((bounds    (view-bounds window))
            (dest-rect (te-dest-rect te))
            (top-char  (te-line-start  (te-top-line te) te)))
        #+debug-text (format-trace 'window-size-part :bounds (rect-to-list bounds))
        (setf (rect-width dest-rect) (rect-width bounds))
        (te-set-rects dest-rect bounds te)
        (te-set-top-line (te-line-at top-char te) te)
        (view-draw-contents window)))))

(defmethod window-null-event-handler ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-idle te))))

(defmethod view-activate-event-handler ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-activate te))))

(defmethod view-deactivate-event-handler ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-deactivate te))))


(defmethod view-draw-contents ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-update (te-%view-rect te) te))
    (call-next-method)))


(defmethod view-key-event-handler ((window te-test-window) char)
  (reporting-errors
    (let ((te (test-window-te window)))
      (when te
        (te-key char te)))))


(defmethod view-click-event-handler ((window te-test-window) where)
  (reporting-errors
    (let ((te (test-window-te window)))
      (when te
        (te-click where
                  (when *current-event*
                    (event-modifierp *current-event* shift-key))
                  te)))))


(defclass te-test-item-window (te-test-window)
  ())

(defmethod window-size-parts ((window te-test-item-window))
  (values))

(defmethod view-draw-contents ((window te-test-item-window))
  (let ((te (test-window-te window)))
    (when te
      (with-focused-view window
        (let ((border (inset-rect (te-view-rect te) -2 -2)))
          (draw-rect* (rect-left border) (rect-top border) (rect-width border) (rect-height border)))
        (te-update (te-%view-rect te) te)))))


;; TODO: call test/paragraph on terec in different states.
(defun test/paragraph (te)
  (doparagraphs (i para te)
    (assert (equal para (te-paragraph i te))))
  :success)

(defun test/te-set-text ()
  (let ((window (make-instance 'te-test-window
                               :window-title "test/te-set-text"
                               :view-size #@(200 400)
                               :view-font '("Times" 18 :plain :srcor))))
    (unwind-protect
         (let ((te (te-new (ui:make-rect 0 0 200 100) (ui:make-rect 0 0 200 100) window))
               (text "Hao Wang, logicien americain.

L'algorithme en  question  a  été  publié  en  1960  dans l'IBM Journal,
article intitule \"Toward  Mechanical Mathematics\", avec des variantes et
une  extension au calcul  des  prédicats.  Il  s'agit  ici  du  \"premier
programme\" de Wang, systeme \"P\".

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704
-- machine à lampes, 32 k  mots  de 36 bits, celle-là même qui vit naître
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

Cet  algorithme  a  été popularisé par J. McCarthy, comme exemple-fanion
d'application  de LISP. Il figure dans le manuel de la première  version
de  LISP  (LISP  1,  sur IBM 704 justement, le manuel est daté  de  Mars
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\"
publié en 1962 par MIT Press, un des maîtres-livres de l'Informatique.


"))
           (setf (test-window-te window) te)
           ;; ---
           (te-set-text text te)
           (window-size-parts window)
           (assert (string= text (te-get-text te)))
           (test/paragraph te)
           :success)
      ;; (window-close window)
      )))


(defun test/te-field ()
  (let* ((window (make-instance 'te-test-item-window
                                :window-title "test/field"
                                :view-size #@(200 100)
                                :view-font '("Times" 18 :plain :srcor)))
         (text   "hello world")
         (te     (te-new (make-rect 20 10 180 30) (make-rect 20 10 180 30) window)))
    (setf (test-window-te window) te)
    ;; ---
    (te-set-wrapping +te-no-wrap+ te)
    (te-set-just +te-just-left+ te)
    (te-set-text text te)
    (with-focused-view (te-in-port te)
      (let ((rect (te-%view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))
    (view-draw-contents window)
    te))


(defun test/te-wrap-paragraph ()
  (let* ((window (make-instance 'te-test-window
                                :window-title "test/te-wrap-paragraph"
                                :view-size #@(200 400)
                                :view-font '("Times" 18 :plain :srcor)))
         (text   (format nil "~
Hao Wang, logicien americain. 

L'algorithme en question a été publié en 1960 dans l'IBM Journal, ~
article intitule \"Toward Mechanical Mathematics\", avec des variantes et ~
une extension au calcul des prédicats. Il s'agit ici du \"premier ~
programme\" de Wang, systeme \"P\". 

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704 ~
-- machine à lampes, 32 k mots de 36 bits, celle-là même qui vit naître ~
LISP à la même époque. Le programme a été écrit en assembleur (Fortran ~
existait, mais il ne s'était pas encore imposé) et l'auteur estime que ~
\"there is very little in the program that is not straightforward\". 

Il observe que les preuves engendrées sont \"essentiellement des arbres\", ~
et annonce que la machine a démontré 220 théorèmes du calcul des ~
propositions (tautologies) en 3 minutes. Il en tire argument pour la ~
supériorité d'une approche algorithmique par rapport à une approche ~
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et Simon (à ~
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat ~
qui dure encore... 

Cet algorithme a été popularisé par J. McCarthy, comme exemple-fanion ~
d'application de LISP. Il figure dans le manuel de la première version ~
de LISP (LISP 1, sur IBM 704 justement, le manuel est daté de Mars ~
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\" ~
publié en 1962 par MIT Press, un des maîtres-livres de l'Informatique. 

"))
         (te     (te-new (make-rect 0 0 200 400) (make-rect 0 0 200 400) window)))
    (setf (test-window-te window) te)
    ;; ---
    (te-set-wrapping +te-word-wrap+ te)
    (te-set-just +te-just-left+ te)
    (te-set-text text te)
    (with-focused-view (te-in-port te)
      (let ((rect (te-%view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))
    (view-draw-contents window)
    (test/paragraph te)
    te))


(defun test/te-selection ()
  (let* ((text "first1 first2 first3 first4 first5
secon1 secon2 secon3 secon4 secon5
third1 third2 third3 third4 third5
fourt1 fourt2 foutr3 fourt4 fourt5
fifth1 fifth2 fifth3 fifth4 fifth5
")
         (rect   (make-rect 0 0 200 100))
         (te     (te-new rect rect nil))
         (third3 (search "third3" text)))

    (te-set-text text te)
    
    (macrolet ((check (expression s e c)
                 (let ((vs (gensym))
                       (ve (gensym))
                       (vc (gensym)))
                   `(let ((,vs ,s)
                          (,ve ,e)
                          (,vc ,c))
                      ,expression
                      (assert (= ,vs (te-sel-start   te)) () "after ~S, ~A should be ~A instead of ~A" ',expression 'start   ,vs (te-sel-start te))
                      (assert (= ,ve (te-sel-end     te)) () "after ~S, ~A should be ~A instead of ~A" ',expression 'end     ,ve (te-sel-end te)) 
                      (assert (= ,vc (te-sel-current te)) () "after ~S, ~A should be ~A instead of ~A" ',expression 'current ,vc (te-sel-current te))))))

      
      (check (te-set-select 0 6 te)
             0 6 6)

      ;; no extend:
      (check (te-set-select third3 third3 te)
             third3 third3 third3)

      (check (te-change-selection nil (+ third3 6) te)
             (+ third3 6) (+ third3 6) (+ third3 6))

      (check (te-change-selection nil (+ third3 36) te)
             (+ third3 36) (+ third3 36) (+ third3 36))

      (check (te-change-selection nil third3 te)
             third3 third3 third3)

      (check (te-change-selection nil (- third3 6) te)
             (- third3 6) (- third3 6) (- third3 6))

      ;; extend after:
      (check (te-set-select third3 third3 te)
             third3  third3  third3)

      (check (te-change-selection t (+ third3 6) te)
             third3 (+ third3 6) (+ third3 6))

      (check (te-change-selection t (+ third3 9) te)
             third3 (+ third3 9) (+ third3 9))

      (check (te-change-selection t (+ third3 3) te)
             third3 (+ third3 3) (+ third3 3))

      (check (te-change-selection t (+ third3 6) te)
             third3 (+ third3 6) (+ third3 6))

      ;; extend before:
      (check (te-change-selection t (- third3 6) te)
             (- third3 6) third3 (- third3 6))

      (check (te-change-selection t (- third3 9) te)
             (- third3 9) third3 (- third3 9))

      (check (te-change-selection t (- third3 3) te)
             (- third3 3) third3 (- third3 3))

      ;; expand after:

      (check (te-change-selection t (+ third3 6) te)
             third3 (+ third3 6) (+ third3 6))

      ;; expand back:
      
      (check (te-change-selection t third3 te)
             third3 third3 third3))
    :success))


(defun test/te-split ()
  (let* ((te (test-window-te (front-window)))
         (charpos 10)
         ;; (charpos (te-sel-current te))
         )
    (te-invariant te)
    (te-split-paragraph-at charpos te)
    (te-invariant te)
    (assert (aref (te-paragraphs te) (te-current-paragraph-index te)))
    (assert (or (= (te-current-paragraph-index te) (te-next-paragraph-index te))
                (null (aref (te-paragraphs te) (1+ (te-current-paragraph-index te))))))
    (assert (or (= (length (te-paragraphs te)) (te-next-paragraph-index te))
                (aref (te-paragraphs te) (te-next-paragraph-index te))))
    (assert (or (= (te-current-paragraph-index te) (te-next-paragraph-index te))
                (null (aref (te-paragraphs te) (1- (te-next-paragraph-index te))))))
    (values
     (te-paragraph-at charpos te)
     (te-current-paragraph-index te)
     (te-next-paragraph-index    te)
     (te-paragraphs te))))


(defun test/te-all ()
  (te-init)
  (test/te-set-text)
  (test/te-wrap-paragraph)
  (test/te-selection)
  (test/te-split))


(defparameter *test/text* (format nil "~
Hao Wang, logicien americain. 

L'algorithme en question a été publié en 1960 dans l'IBM Journal, ~
article intitule \"Toward Mechanical Mathematics\", avec des variantes et ~
une extension au calcul des prédicats. Il s'agit ici du \"premier ~
programme\" de Wang, systeme \"P\". 

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704 ~
-- machine à lampes, 32 k mots de 36 bits, celle-là même qui vit naître ~
LISP à la même époque. Le programme a été écrit en assembleur (Fortran ~
existait, mais il ne s'était pas encore imposé) et l'auteur estime que ~
\"there is very little in the program that is not straightforward\". 

Il observe que les preuves engendrées sont \"essentiellement des arbres\", ~
et annonce que la machine a démontré 220 théorèmes du calcul des ~
propositions (tautologies) en 3 minutes. Il en tire argument pour la ~
supériorité d'une approche algorithmique par rapport à une approche ~
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et Simon (à ~
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat ~
qui dure encore... 

Cet algorithme a été popularisé par J. McCarthy, comme exemple-fanion ~
d'application de LISP. Il figure dans le manuel de la première version ~
de LISP (LISP 1, sur IBM 704 justement, le manuel est daté de Mars ~
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\" ~
publié en 1962 par MIT Press, un des maîtres-livres de l'Informatique. 

"))



#-(and) (progn

          ;; Use one of those to create a text window:
          (test/te-set-text) 
          (test/te-wrap-paragraph)

          (defparameter *te* (test-window-te (front-window)))
          (te-set-text *text* *te*)
          (setf (te-click-stuff (test-window-te (front-window))) nil)
          
          (te-init)
          (test/te-wrap-paragraph)
          (te-set-wrapping +te-no-wrap+ *te*)
          (te-set-wrapping +te-word-wrap+ *te*)
          (te-update (view-bounds (te-in-port *te*)) *te*)
          
          (test/te-all)
          (test/te-set-text)

          (test/te-split)
          (test/te-selection)
          
          (te-set-select start end te)
          (te-change-selection extend new-point te)

          (inspect (test-window-te (front-window)))

          (te-cal-text (test-window-te (front-window)))
          
          (dotimes (i (te-nlines (test-window-te (front-window))))
            (write-line (te-line i (test-window-te (front-window)))))

          (let ((te  (test-window-te (front-window))))
            (rect-to-list (te-compute-caret-rect (te-sel-start te) te)))
          (:topleft (0 -1) :size (200 2))

          (map nil (lambda (w) (ignore-errors (te-cal-text (test-window-te w))))
            (remove 'te-test-window (windows) :key (lambda (x) (class-name (class-of x))) :test-not (function eql)))

          (let ((te (test-window-te (front-window))))
            (list (te-paragraph-at 0 te)
                  (te-paragraph-at 300 te)
                  (te-paragraph-at (te-length te) te)))

          (let ((te (test-window-te (front-window))))
            (setf (te-length te) 1475))

          (let ((te (test-window-te (front-window))))
            (values (te-length te)
                    (TE-line-starts te)))



          
          (let ((te (test-window-te (front-window))))
            (te-line 25 te))
          
          (let ((te (test-window-te (front-window))))
            (te-invariant te))
          
          (let ((te (test-window-te (front-window))))
            (te-paragraphs te))
          
          (let ((te (test-window-te (front-window))))
            (test/paragraph te))

          (let ((te (test-window-te (front-window))))
            (doparagraphs (i para te)
              (write-line (cdr para))))

          (let ((te (test-window-te (front-window))))
            (dotimes (i (te-nparagraphs te))
              (write-line (cdr (te-paragraph i te)))))

          (let ((te (test-window-te (front-window))))
            (dotimes (i (te-nparagraphs te))
              (write-line (cdr (te-paragraph i te)))))
          
          (let ((te (test-window-te (front-window))))
            (setf (te-high-hook te) (function te-default-high-hook)))

          (let ((te (test-window-te (front-window))))
            (te-set-default-bindings te))
          
          (let ((te (test-window-te (front-window))))
            (setf (te-just te) te-just-center)
            (te-cal-text te)
            (view-draw-contents  (front-window)))

          (let ((te (test-window-te (front-window))))
            (setf (te-just te) te-just-right)
            (te-cal-text te)
            (view-draw-contents  (front-window)))
          
          (te-set-select 0 10 (test-window-te (front-window)))
          (te-updates (test-window-te (front-window)))
          (view-draw-contents (test-window-te (front-window)))
          (let ((te    (test-window-te (front-window))))
            (te-cal-text te)
            (let ((rect  (te-compute-caret-rect (te-sel-start te) te)))
              (list (rect-left rect)
                    (rect-top rect)
                    (rect-right rect)
                    (rect-bottom rect))))
          

          (let* ((te  (test-window-te (front-window)))
                 (caret-position (te-sel-start te))
                 (lino (te-line-at caret-position te)))
            ;; (te-line-coordinates lino te)
            (rect-to-list (te-compute-caret-rect (te-sel-start te) te)))
          (:topleft (-1 0) :size (2 200))
          0
          608
          (rect-to-list #S(rect :topleft 594 :bottomright 13107812)) (:topleft (594 0) :size (18 200))
          ""
          
          (te-line-coordinates lino te)

          (defstruct e (v (make-array 2000 :element-type 'integer :initial-element 0)))
          (time (loop with v = (e-v (make-e)) 
                      for i below 2000
                      do (incf (aref v i))))

          (time (loop
                  for para in (let ((text (split-sequence #\Newline *text*)))
                                (loop repeat 2000
                                      with i = text
                                      collect (pop i)
                                      unless i
                                        do (setf i text)))
                  do (multiple-value-bind (ff ms) (font-codes '("Times" 18))
                       (font-codes-string-width para ff ms))))


          (te-set-text "bonjour le
monde!

" *te*)

          (defparameter *te* (test/te-field))
          (defparameter *te* (test/te-wrap-paragraph))
          )


;;;; THE END ;;;;
