;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               paragraph-style.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Builds NSParagraphStyle objects.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-23 <PJB> Created.
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
(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)


(defvar *text-alignments* '()
  "An a-list mapping keywords to Text Alignments.")

(defvar *writing-directions* '()
  "An a-list mapping keywords to Writing Directions.")

(defvar *line-break-mode* '()
  "An a-list mapping keywords to Line Break Modes.")


(defstruct (paragraph-style (:conc-name ps-))
  (alignment         :natural  :type (member :left :right :center :justified :natural))
  (writing-direction :natural  :type (member :left-to-right :right-to-left :natural))
  (line-break-mode   :clipping :type (member :word-wrapping :char-wrapping :clipping
                                                            :truncating-head :trucating-tail
                                                            :truncating-middle))
  (header-level             0    :type integer)
  (first-line-head-indent   0.0  :type cgfloat)
  (head-indent              0.0  :type cgfloat)
  (tail-indent              0.0  :type cgfloat)
  (hyphenation-factor       0.0  :type sfloat)
  (tightening-factor        0.05 :type sfloat)
  (line-height-multiple     0.0  :type cgfloat)
  (line-spacing             0.0  :type cgfloat)
  (maximum-line-height      0.0  :type cgfloat)
  (minimum-line-height      0.0  :type cgfloat)
  (paragraph-spacing        0.0  :type cgfloat)
  (paragraph-spacing-before 0.0  :type cgfloat)
  (default-tab-interval     8.0  :type cgfloat)
  (tab-stops #(28 56 84 112 140 168 196 224 252 280 308 336) :type vector))


(defmethod unwrap ((wps paragraph-style))
  (unwrapping wps
    (let ((ps [[NSMutableParagraphStyle alloc] init]))
      [ps setAlignment: (cdr (assoc (ps-alignment wps) *text-alignments*))]
      [ps setBaseWritingDirection: (cdr (assoc (ps-writing-direction wps) *writing-directions*))]
      [ps setLineBreakMode: (cdr (assoc (ps-line-break-mode wps) *line-break-mode*))]
      [ps setFirstLineHeadIndent:(cgfloat (ps-first-line-head-indent wps))]
      [ps setHeadIndent: (cgfloat (ps-head-indent wps))]
      [ps setTailIndent: (cgfloat (ps-tail-indent wps))]
      [ps setHyphenationFactor: (sfloat (ps-hyphenation-factor wps))]
      [ps setTighteningFactorForTruncation:(sfloat (ps-tightening-factor wps))]
      [ps setLineHeightMultiple: (cgfloat (ps-line-height-multiple wps))]
      [ps setLineSpacing: (cgfloat (ps-line-spacing wps))]
      [ps setMaximumLineHeight: (cgfloat (ps-maximum-line-height wps))]
      [ps setMinimumLineHeight: (cgfloat (ps-minimum-line-height wps))]
      [ps setParagraphSpacing: (cgfloat (ps-paragraph-spacing wps))]
      [ps setParagraphSpacingBefore: (cgfloat (ps-paragraph-spacing-before wps))]
      [ps setDefaultTabInterval: (cgfloat (ps-default-tab-interval wps))]
      [ps setTabStops: (unwrap (ps-tab-stops wps))]
      [ps setHeaderLevel: (ps-header-level wps)]
      ;; setTextBlocks:(NSArray *)array
      ;; setTextLists:(NSArray *)array
      ps)))


(defun initialize/paragraph-style ()
  (setf *text-alignments*    `((:left              . ,#$NSLeftTextAlignment)
                               (:right             . ,#$NSRightTextAlignment)
                               (:center            . ,#$NSCenterTextAlignment)
                               (:justified         . ,#$NSJustifiedTextAlignment)
                               (:natural           . ,#$NSNaturalTextAlignment))
        *writing-directions* `((:natural           . ,#$NSWritingDirectionNatural)
                               (:left-to-right     . ,#$NSWritingDirectionLeftToRight)
                               (:right-to-left     . ,#$NSWritingDirectionRightToLeft))
        *line-break-mode*    `((:word-wrapping     . ,#$NSLineBreakByWordWrapping)
                               (:char-wrapping     . ,#$NSLineBreakByCharWrapping)
                               (:clipping          . ,#$NSLineBreakByClipping)
                               (:truncating-head   . ,#$NSLineBreakByTruncatingHead)
                               (:truncating-tail   . ,#$NSLineBreakByTruncatingTail)
                               (:truncating-middle . ,#$NSLineBreakByTruncatingMiddle)))
  (values))


#-(and) (
         (initialize/paragraph-style)
         (unwrap
          (make-paragraph-style :alignment :left
                                :writing-direction :left-to-right
                                :line-break-mode :word-wrapping))

         )

;;;; THE END ;;;;
