;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    Defines the MCLGUI asdf system.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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

(asdf:defsystem :mclgui
  :name "mclgui"
  :description "Macintosh Common Lisp Graphical User Interface for OpenStep"
  :author "Pascal J. Bourguignon"
  :version "1.1.2"
  :license "GPL3"
  :depends-on ("closer-mop" ; window.lisp uses closer-mop:class-precedence-list, etc.
               "bordeaux-threads"
               "cffi"
               "trivial-gray-streams"
               "alexandria"
               "split-sequence"
               "com.informatimago.objcl"
               "com.informatimago.clext" ; closer-weak: wrapper.lisp uses weak lists
               ;; While developping:
               "com.informatimago.common-lisp.cesarum")
  :components (

               ;; (:file "cocoa"
               ;;  :depends-on ("packages"))

               #-ccl-1.10
               (:file "cg"
                :depends-on ())

               (:file "packages"
                :depends-on ())

               (:file "readtable"
                :depends-on ("packages" "point"))

               (:file "mutex"
                :depends-on ("packages"))

               (:file "debug"
                :depends-on ("packages" "mutex"))

               (:file "macros"
                :depends-on ("packages" "debug"))

               (:file "mailbox"
                :depends-on ("packages"))

               (:file "process"
                :depends-on ("packages"
                             "mailbox"
                             "debug"))

               (:file "variables"
                :depends-on ("packages" "point"))

               (:file "system"
                :depends-on ("packages" "readtable" "debug" "mac-event"))

               (:file "circular"
                :depends-on ("packages"))

               (:file "wrapper"
                :depends-on ("packages" "system" "circular"))

               (:file "notification"
                :depends-on ("packages"
                             "wrapper"))

               (:file "mac-event"
                :depends-on ("packages"
                             "macros" "variables" "point" "mutex"))

               (:file "objc-classes"
                :depends-on ("packages"
                             "readtable" "macros" "wrapper" "variables" "point" "rect"
                             "system" "mac-event" "debug"))

               (:file "graphics"
                :depends-on ("packages"
                             "macros" "variables" "point" "font"
                             "paragraph-style"))

               (:file "graphics-low"
                :depends-on ("packages"
                             "macros" "variables" "point"))

               (:file "paragraph-style"
                :depends-on ("packages"
                             "macros" "objc-classes"
                             "wrapper"))

               ;; Chapter 2:

               (:file "point"
                :depends-on ("packages"))

               (:file "font"
                :depends-on ("packages"
                             "readtable" "macros" "variables" "system"
                             "point" "objc-classes" "color"
                             "wrapper"))


               (:file "pattern"
                :depends-on ("packages"
                             "macros" "variables" "point"
                             "wrapper"))

               ;; Chapter 3: Menus

               (:file "menu"
                :depends-on ("packages"
                             "macros" "variables" "color"
                             "point" "font" "wrapper" "notification" "view-classes"))

               (:file "window-menu-item"
                :depends-on ("packages"
                             "menu" "window"))

               ;; Chapter 4: Views and Windows

               (:file "view-classes"
                :depends-on ("packages"
                             "macros" "variables" "color"
                             "wrapper"))

               (:file "view"
                :depends-on ("packages"
                             "process"
                             "macros" "variables" "color"
                             "point" "region" "font" "pen"
                             "wrapper" "view-classes" "objc-classes"
                             "region-view"
                             #-ccl-1.10 "cg"))

               (:file "view-stream"
                :depends-on ("packages"
                             "macros" "variables" "point" "font" "pen"
                             "view-classes" "view" "graphics"))

               (:file "window"
                :depends-on ("packages"
                             "process"
                             "macros" "variables" "color"
                             "point" "region" "font"
                             "objc-classes" "objc-region-path"
                             "view-classes"
                             "wrapper" "view" "notification"
                             "menu"
                             #-ccl-1.10 "cg"))

               (:file "fred-window"
                :depends-on ("packages"
                             "macros" "variables" "color"
                             "point" "region" "font"
                             "view-classes" "view" "window"))

               ;; Chapter 5: Dialog Items and Dialogs


               (:file "dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "region"))

               (:file "default-button-mixin"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event" "dialog"))

               (:file "control-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" ; "event"
                             "dialog-item"))

               (:file "button-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" ; "event"
                             "dialog-item" "control-dialog-item"
                             "default-button-mixin"))

               (:file "check-box-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "control-dialog-item"
                             "button-dialog-item"))

               (:file "radio-button-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" ; "event"
                             "dialog-item" "control-dialog-item"
                             "button-dialog-item"))

               (:file "static-text-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item"))

               (:file "focus-rect-mixin"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item"))

               (:file "key-handler-mixin"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window"))

               (:file "basic-editable-text-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "view-event" "key-handler-mixin"))

               (:file "text-edit-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "view-event" "key-handler-mixin"
                             "basic-editable-text-dialog-item"
                             "text-edit"))

               (:file "editable-text-dialog-item"
                :depends-on ("packages"
                             "text-edit-dialog-item"))

               (:file "table-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system" "region"
                             "view-classes" "view" "window" ; "event"
                             "region-view"
                             "dialog-item" "view-event" "key-handler-mixin"
                             "scroll-bar-dialog-item"))

               (:file "sequence-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "view-event" "key-handler-mixin"
                             "scroll-bar-dialog-item" "table-dialog-item"))

               (:file "scroll-bar-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" ; "event"
                             "dialog-item" "view-event"))

               (:file "pop-up-menu-dialog-item"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" ; "event"
                             "dialog-item" "view-event"))

               (:file "dialog"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window"  "view-event" "event"
                             "dialog-item" "key-handler-mixin"))

               (:file "y-or-n-dialog"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "key-handler-mixin"
                             "button-dialog-item"
                             "static-text-dialog-item" "editable-text-dialog-item"
                             "dialog"))

               (:file "select-dialog"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "key-handler-mixin"
                             "button-dialog-item"
                             "static-text-dialog-item" "editable-text-dialog-item"
                             "dialog"))

               (:file "message-dialog"
                :depends-on ("packages"
                             "macros" "variables" "point" "system" "pen"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "key-handler-mixin"
                             "button-dialog-item"
                             "static-text-dialog-item" "editable-text-dialog-item"
                             "dialog"))

               (:file "get-string-dialog"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "view-classes" "view" "window" "event"
                             "dialog-item" "key-handler-mixin"
                             "button-dialog-item"
                             "static-text-dialog-item" "editable-text-dialog-item"
                             "dialog"))

               (:file "scroller"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "graphics"
                             "view-classes" "view" "window" ; "event"
                             "dialog-item" "key-handler-mixin"
                             "scroll-bar-dialog-item"
                             "dialog"))

               ;; Chapter 6: Color

               (:file "color"
                :depends-on ("packages"
                             "wrapper" "graphics-low"))

               (:file "color-dialog"
                :depends-on ("packages"
                             "wrapper" "color" "dialog"))

               ;; Chapter 8: File System Interface

               (:file "file"
                :depends-on ("packages"))

               ;; We only implement the file chooser dialogs.
               ;; The rest is covered by CL.


               ;; Chapter 10: Events

               (:file "event"
                :depends-on ("packages"
                             "macros" "variables" "point" "system"
                             "objc-classes"
                             "view-classes" "view" "window"))

               (:file "view-event"
                :depends-on ("packages"
                             "macros" "variables" "point" "system" "view-classes" "view"
                             "event" "key-handler-mixin"))

               (:file "window-event"
                :depends-on ("packages"
                             "process"
                             "macros" "variables" "point" "system" "view-classes" "window"
                             "view-event" "event" "application"))


               (:file "cursor"
                :depends-on ("packages"
                             "macros" "variables" "point"
                             "wrapper" "pattern" "readtable"))

               (:file "scrap"
                :depends-on ("packages"
                             "macros" "variables" "point"))

               (:file "eval"
                :depends-on ("packages"
                             "macros" "variables" "application"))

               ;; Chapter 11: Apple Events

               (:file "application"
                :depends-on ("packages"
                             "process"
                             "macros" "variables" "wrapper"
                             "objc-classes" "event"))

               ;; Appendix D: Quickdraw Graphics:

               (:file "rect"
                :depends-on ("packages" "point"))

               (:file "region"
                :depends-on ("packages"
                             "macros" "variables" "point" "rect"))

               (:file "region-view"
                :depends-on ("packages"
                             "macros"
                             "region"
                             "graphics"
                             "view-classes"))

               (:file "region-path"
                :depends-on ("packages"
                             "macros" "variables" "point" "rect"
                             "graphics"
                             "region"))

               (:file "objc-region-path"
                :depends-on ("region-path"))

               (:file "pen"
                :depends-on ("packages"
                             "macros" "variables" "point"
                             "objc-classes" "view-classes"
                             "pattern"
                             "rect" "graphics"))

               (:file "quickdraw"
                :depends-on ("packages"
                             "macros" "variables" "point"
                             "objc-classes" "objc-region-path"
                             "view-classes" "view"
                             "pattern" "rect" "region" "pen"))

               ;; Managers:

               (:file "text-edit"
                :depends-on ("packages"
                             "mutex"
                             "window" "pen" "font" "point" "rect"
                             "region" "color" "scrap"
                             "ns-keys"))

               (:file "ns-keys"
                :depends-on ("packages"))

               #+has-appleevent
               (:file "apple-event"
                :depends-on ("packages" "application"))


               ;; ;; Extensions
               ;; (:file "layout"
               ;;  :depends-on ("packages" "view-classes"))
               ;;
               ;; (:file "debugger-dialog"
               ;;  :depends-on ("packages" "dialog" "layout"))


               ;; MCLGUI:

               (:file "mclgui"
                :depends-on ("packages"
                             "macros" "variables" "process" "wrapper"
                             "objc-classes"
                             "point" "font" "pen" "pattern" "cursor" "view-stream"
                             "menu" "view" "window" "dialog" "fred-window"
                             "dialog-item" "control-dialog-item"
                             "button-dialog-item" "check-box-dialog-item"
                             "radio-button-dialog-item" "static-text-dialog-item"
                             "focus-rect-mixin" "editable-text-dialog-item"
                             "table-dialog-item" "scroll-bar-dialog-item"
                             "pop-up-menu-dialog-item" "sequence-dialog-item"
                             "y-or-n-dialog" "message-dialog" "get-string-dialog"
                             "select-dialog"
                             "scroller" "region-path" "objc-region-path"
                             "event" "view-event" "window-event"
                             "cursor" "scrap" "eval" "application"
                             #+has-appleevent "apple-event")))
  :in-order-to ((asdf:test-op (asdf:test-op "mclgui-test"))))


;;;; THE END ;;;;
