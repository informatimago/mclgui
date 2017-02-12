;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               grafport.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements Quickdraw Grafport.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-03 <PJB> Created.
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
(in-package "MCLGUI")

(defclass bitmap ()
  ((baseaddr :reader bitmap-baseaddr)
   (rowbytes :reader bitmap-rowbytes)
   (bounds   :reader bitmap-bounds)))

(defclass grafport ()
  ((device     :reader grafport-device) ; integer device-specific info
   (portbits   :reader grafport-portbits); bitmap
   (portrect   :reader grafport-portrect)
   (visrgn     :reader grafport-visrgn)
   (cliprgn    :reader grafport-cliprgn)
   (bkpat      :reader grafport-bkpat)
   (fillpat    :reader grafport-fillpat)
   (pnloc      :reader grafport-pnloc)
   (pnsize     :reader grafport-pnsize)
   (pnmode     :reader grafport-pnmode)
   (pnpat      :reader grafport-pnpat)
   (pnvis      :reader grafport-pnvis)
   (txfont     :reader grafport-txfont)
   (txface     :reader grafport-txface)
   (txmode     :reader grafport-txmode)
   (txsize     :reader grafport-txsize)
   (spextra    :reader grafport-spextra)
   (fgcolor    :reader grafport-fgcolor)
   (bkcolor    :reader grafport-bkcolor)
   (colrbit    :reader grafport-colrbit)
   (patstretch :reader grafport-patstretch)
   (picsave    :reader grafport-picsave)
   (rgnsave    :reader grafport-rgnsave)
   (polysave   :reader grafport-polysave)
   (grafprocs  :reader grafport-grafprocs)))

(defvar *theport* nil)

#|


SetPort(p:^GrafPort);
----------------------------------------

This sets thePort, the current GrafPort.

On MacOSX, we use: ::

    (when [viewh lockFocusIfCanDraw]
      (unwind-protect
            …
        [viewh unlockFocus]))


SetOrigin(x:integer;y:integer);
----------------------------------------

This sets the coordinate of the topleft point of the GrafPort.
This applies a translation matrix from the old coordinates to the new one on the

|#
