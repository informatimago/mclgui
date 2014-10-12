;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ns-keys.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the encoding of NS function keys.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-08 <PJB> Created.
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

(defconstant +ns-up-arrow-function-code+      #xf700)
(defconstant +ns-down-arrow-function-code+    #xf701)
(defconstant +ns-left-arrow-function-code+    #xf702)
(defconstant +ns-right-arrow-function-code+   #xf703)
(defconstant +ns-f1-function-code+            #xf704)
(defconstant +ns-f2-function-code+            #xf705)
(defconstant +ns-f3-function-code+            #xf706)
(defconstant +ns-f4-function-code+            #xf707)
(defconstant +ns-f5-function-code+            #xf708)
(defconstant +ns-f6-function-code+            #xf709)
(defconstant +ns-f7-function-code+            #xf70a)
(defconstant +ns-f8-function-code+            #xf70b)
(defconstant +ns-f9-function-code+            #xf70c)
(defconstant +ns-f10-function-code+           #xf70d)
(defconstant +ns-f11-function-code+           #xf70e)
(defconstant +ns-f12-function-code+           #xf70f)
(defconstant +ns-f13-function-code+           #xf710)
(defconstant +ns-f14-function-code+           #xf711)
(defconstant +ns-f15-function-code+           #xf712)
(defconstant +ns-f16-function-code+           #xf713)
(defconstant +ns-f17-function-code+           #xf714)
(defconstant +ns-f18-function-code+           #xf715)
(defconstant +ns-f19-function-code+           #xf716)
(defconstant +ns-f20-function-code+           #xf717)
(defconstant +ns-f21-function-code+           #xf718)
(defconstant +ns-f22-function-code+           #xf719)
(defconstant +ns-f23-function-code+           #xf71a)
(defconstant +ns-f24-function-code+           #xf71b)
(defconstant +ns-f25-function-code+           #xf71c)
(defconstant +ns-f26-function-code+           #xf71d)
(defconstant +ns-f27-function-code+           #xf71e)
(defconstant +ns-f28-function-code+           #xf71f)
(defconstant +ns-f29-function-code+           #xf720)
(defconstant +ns-f30-function-code+           #xf721)
(defconstant +ns-f31-function-code+           #xf722)
(defconstant +ns-f32-function-code+           #xf723)
(defconstant +ns-f33-function-code+           #xf724)
(defconstant +ns-f34-function-code+           #xf725)
(defconstant +ns-f35-function-code+           #xf726)
(defconstant +ns-insert-function-code+        #xf727)
(defconstant +ns-delete-function-code+        #xf728)
(defconstant +ns-home-function-code+          #xf729)
(defconstant +ns-begin-function-code+         #xf72a)
(defconstant +ns-end-function-code+           #xf72b)
(defconstant +ns-page-up-function-code+       #xf72c)
(defconstant +ns-page-down-function-code+     #xf72d)
(defconstant +ns-print-screen-function-code+  #xf72e)
(defconstant +ns-scroll-lock-function-code+   #xf72f)
(defconstant +ns-pause-function-code+         #xf730)
(defconstant +ns-sys-req-function-code+       #xf731)
(defconstant +ns-break-function-code+         #xf732)
(defconstant +ns-reset-function-code+         #xf733)
(defconstant +ns-stop-function-code+          #xf734)
(defconstant +ns-menu-function-code+          #xf735)
(defconstant +ns-user-function-code+          #xf736)
(defconstant +ns-system-function-code+        #xf737)
(defconstant +ns-print-function-code+         #xf738)
(defconstant +ns-clear-line-function-code+    #xf739)
(defconstant +ns-clear-display-function-code+ #xf73a)
(defconstant +ns-insert-line-function-code+   #xf73b)
(defconstant +ns-delete-line-function-code+   #xf73c)
(defconstant +ns-insert-char-function-code+   #xf73d)
(defconstant +ns-delete-char-function-code+   #xf73e)
(defconstant +ns-prev-function-code+          #xf73f)
(defconstant +ns-next-function-code+          #xf740)
(defconstant +ns-select-function-code+        #xf741)
(defconstant +ns-execute-function-code+       #xf742)
(defconstant +ns-undo-function-code+          #xf743)
(defconstant +ns-redo-function-code+          #xf744)
(defconstant +ns-find-function-code+          #xf745)
(defconstant +ns-help-function-code+          #xf746)
(defconstant +ns-mode-switch-function-code+   #xf747)


(defconstant +ns-up-arrow-function-key+       (code-char +ns-up-arrow-function-code+))
(defconstant +ns-down-arrow-function-key+     (code-char +ns-down-arrow-function-code+))
(defconstant +ns-left-arrow-function-key+     (code-char +ns-left-arrow-function-code+))
(defconstant +ns-right-arrow-function-key+    (code-char +ns-right-arrow-function-code+))
(defconstant +ns-f1-function-key+             (code-char +ns-f1-function-code+))
(defconstant +ns-f2-function-key+             (code-char +ns-f2-function-code+))
(defconstant +ns-f3-function-key+             (code-char +ns-f3-function-code+))
(defconstant +ns-f4-function-key+             (code-char +ns-f4-function-code+))
(defconstant +ns-f5-function-key+             (code-char +ns-f5-function-code+))
(defconstant +ns-f6-function-key+             (code-char +ns-f6-function-code+))
(defconstant +ns-f7-function-key+             (code-char +ns-f7-function-code+))
(defconstant +ns-f8-function-key+             (code-char +ns-f8-function-code+))
(defconstant +ns-f9-function-key+             (code-char +ns-f9-function-code+))
(defconstant +ns-f10-function-key+            (code-char +ns-f10-function-code+))
(defconstant +ns-f11-function-key+            (code-char +ns-f11-function-code+))
(defconstant +ns-f12-function-key+            (code-char +ns-f12-function-code+))
(defconstant +ns-f13-function-key+            (code-char +ns-f13-function-code+))
(defconstant +ns-f14-function-key+            (code-char +ns-f14-function-code+))
(defconstant +ns-f15-function-key+            (code-char +ns-f15-function-code+))
(defconstant +ns-f16-function-key+            (code-char +ns-f16-function-code+))
(defconstant +ns-f17-function-key+            (code-char +ns-f17-function-code+))
(defconstant +ns-f18-function-key+            (code-char +ns-f18-function-code+))
(defconstant +ns-f19-function-key+            (code-char +ns-f19-function-code+))
(defconstant +ns-f20-function-key+            (code-char +ns-f20-function-code+))
(defconstant +ns-f21-function-key+            (code-char +ns-f21-function-code+))
(defconstant +ns-f22-function-key+            (code-char +ns-f22-function-code+))
(defconstant +ns-f23-function-key+            (code-char +ns-f23-function-code+))
(defconstant +ns-f24-function-key+            (code-char +ns-f24-function-code+))
(defconstant +ns-f25-function-key+            (code-char +ns-f25-function-code+))
(defconstant +ns-f26-function-key+            (code-char +ns-f26-function-code+))
(defconstant +ns-f27-function-key+            (code-char +ns-f27-function-code+))
(defconstant +ns-f28-function-key+            (code-char +ns-f28-function-code+))
(defconstant +ns-f29-function-key+            (code-char +ns-f29-function-code+))
(defconstant +ns-f30-function-key+            (code-char +ns-f30-function-code+))
(defconstant +ns-f31-function-key+            (code-char +ns-f31-function-code+))
(defconstant +ns-f32-function-key+            (code-char +ns-f32-function-code+))
(defconstant +ns-f33-function-key+            (code-char +ns-f33-function-code+))
(defconstant +ns-f34-function-key+            (code-char +ns-f34-function-code+))
(defconstant +ns-f35-function-key+            (code-char +ns-f35-function-code+))
(defconstant +ns-insert-function-key+         (code-char +ns-insert-function-code+))
(defconstant +ns-delete-function-key+         (code-char +ns-delete-function-code+))
(defconstant +ns-home-function-key+           (code-char +ns-home-function-code+))
(defconstant +ns-begin-function-key+          (code-char +ns-begin-function-code+))
(defconstant +ns-end-function-key+            (code-char +ns-end-function-code+))
(defconstant +ns-page-up-function-key+        (code-char +ns-page-up-function-code+))
(defconstant +ns-page-down-function-key+      (code-char +ns-page-down-function-code+))
(defconstant +ns-print-screen-function-key+   (code-char +ns-print-screen-function-code+))
(defconstant +ns-scroll-lock-function-key+    (code-char +ns-scroll-lock-function-code+))
(defconstant +ns-pause-function-key+          (code-char +ns-pause-function-code+))
(defconstant +ns-sys-req-function-key+        (code-char +ns-sys-req-function-code+))
(defconstant +ns-break-function-key+          (code-char +ns-break-function-code+))
(defconstant +ns-reset-function-key+          (code-char +ns-reset-function-code+))
(defconstant +ns-stop-function-key+           (code-char +ns-stop-function-code+))
(defconstant +ns-menu-function-key+           (code-char +ns-menu-function-code+))
(defconstant +ns-user-function-key+           (code-char +ns-user-function-code+))
(defconstant +ns-system-function-key+         (code-char +ns-system-function-code+))
(defconstant +ns-print-function-key+          (code-char +ns-print-function-code+))
(defconstant +ns-clear-line-function-key+     (code-char +ns-clear-line-function-code+))
(defconstant +ns-clear-display-function-key+  (code-char +ns-clear-display-function-code+))
(defconstant +ns-insert-line-function-key+    (code-char +ns-insert-line-function-code+))
(defconstant +ns-delete-line-function-key+    (code-char +ns-delete-line-function-code+))
(defconstant +ns-insert-char-function-key+    (code-char +ns-insert-char-function-code+))
(defconstant +ns-delete-char-function-key+    (code-char +ns-delete-char-function-code+))
(defconstant +ns-prev-function-key+           (code-char +ns-prev-function-code+))
(defconstant +ns-next-function-key+           (code-char +ns-next-function-code+))
(defconstant +ns-select-function-key+         (code-char +ns-select-function-code+))
(defconstant +ns-execute-function-key+        (code-char +ns-execute-function-code+))
(defconstant +ns-undo-function-key+           (code-char +ns-undo-function-code+))
(defconstant +ns-redo-function-key+           (code-char +ns-redo-function-code+))
(defconstant +ns-find-function-key+           (code-char +ns-find-function-code+))
(defconstant +ns-help-function-key+           (code-char +ns-help-function-code+))
(defconstant +ns-mode-switch-function-key+    (code-char +ns-mode-switch-function-code+))

#-(and) (:export
 "+NS-UP-ARROW-FUNCTION-KEY+" "+NS-DOWN-ARROW-FUNCTION-KEY+"
 "+NS-LEFT-ARROW-FUNCTION-KEY+" "+NS-RIGHT-ARROW-FUNCTION-KEY+"
 "+NS-F1-FUNCTION-KEY+" "+NS-F2-FUNCTION-KEY+" "+NS-F3-FUNCTION-KEY+"
 "+NS-F4-FUNCTION-KEY+" "+NS-F5-FUNCTION-KEY+" "+NS-F6-FUNCTION-KEY+"
 "+NS-F7-FUNCTION-KEY+" "+NS-F8-FUNCTION-KEY+" "+NS-F9-FUNCTION-KEY+"
 "+NS-F10-FUNCTION-KEY+" "+NS-F11-FUNCTION-KEY+"
 "+NS-F12-FUNCTION-KEY+" "+NS-F13-FUNCTION-KEY+"
 "+NS-F14-FUNCTION-KEY+" "+NS-F15-FUNCTION-KEY+"
 "+NS-F16-FUNCTION-KEY+" "+NS-F17-FUNCTION-KEY+"
 "+NS-F18-FUNCTION-KEY+" "+NS-F19-FUNCTION-KEY+"
 "+NS-F20-FUNCTION-KEY+" "+NS-F21-FUNCTION-KEY+"
 "+NS-F22-FUNCTION-KEY+" "+NS-F23-FUNCTION-KEY+"
 "+NS-F24-FUNCTION-KEY+" "+NS-F25-FUNCTION-KEY+"
 "+NS-F26-FUNCTION-KEY+" "+NS-F27-FUNCTION-KEY+"
 "+NS-F28-FUNCTION-KEY+" "+NS-F29-FUNCTION-KEY+"
 "+NS-F30-FUNCTION-KEY+" "+NS-F31-FUNCTION-KEY+"
 "+NS-F32-FUNCTION-KEY+" "+NS-F33-FUNCTION-KEY+"
 "+NS-F34-FUNCTION-KEY+" "+NS-F35-FUNCTION-KEY+"
 "+NS-INSERT-FUNCTION-KEY+" "+NS-DELETE-FUNCTION-KEY+"
 "+NS-HOME-FUNCTION-KEY+" "+NS-BEGIN-FUNCTION-KEY+"
 "+NS-END-FUNCTION-KEY+" "+NS-PAGE-UP-FUNCTION-KEY+"
 "+NS-PAGE-DOWN-FUNCTION-KEY+" "+NS-PRINT-SCREEN-FUNCTION-KEY+"
 "+NS-SCROLL-LOCK-FUNCTION-KEY+" "+NS-PAUSE-FUNCTION-KEY+"
 "+NS-SYS-REQ-FUNCTION-KEY+" "+NS-BREAK-FUNCTION-KEY+"
 "+NS-RESET-FUNCTION-KEY+" "+NS-STOP-FUNCTION-KEY+"
 "+NS-MENU-FUNCTION-KEY+" "+NS-USER-FUNCTION-KEY+"
 "+NS-SYSTEM-FUNCTION-KEY+" "+NS-PRINT-FUNCTION-KEY+"
 "+NS-CLEAR-LINE-FUNCTION-KEY+" "+NS-CLEAR-DISPLAY-FUNCTION-KEY+"
 "+NS-INSERT-LINE-FUNCTION-KEY+" "+NS-DELETE-LINE-FUNCTION-KEY+"
 "+NS-INSERT-CHAR-FUNCTION-KEY+" "+NS-DELETE-CHAR-FUNCTION-KEY+"
 "+NS-PREV-FUNCTION-KEY+" "+NS-NEXT-FUNCTION-KEY+"
 "+NS-SELECT-FUNCTION-KEY+" "+NS-EXECUTE-FUNCTION-KEY+"
 "+NS-UNDO-FUNCTION-KEY+" "+NS-REDO-FUNCTION-KEY+"
 "+NS-FIND-FUNCTION-KEY+" "+NS-HELP-FUNCTION-KEY+"
 "+NS-MODE-SWITCH-FUNCTION-KEY+" "+NS-UP-ARROW-FUNCTION-CODE+"
 "+NS-DOWN-ARROW-FUNCTION-CODE+" "+NS-LEFT-ARROW-FUNCTION-CODE+"
 "+NS-RIGHT-ARROW-FUNCTION-CODE+" "+NS-F1-FUNCTION-CODE+"
 "+NS-F2-FUNCTION-CODE+" "+NS-F3-FUNCTION-CODE+"
 "+NS-F4-FUNCTION-CODE+" "+NS-F5-FUNCTION-CODE+"
 "+NS-F6-FUNCTION-CODE+" "+NS-F7-FUNCTION-CODE+"
 "+NS-F8-FUNCTION-CODE+" "+NS-F9-FUNCTION-CODE+"
 "+NS-F10-FUNCTION-CODE+" "+NS-F11-FUNCTION-CODE+"
 "+NS-F12-FUNCTION-CODE+" "+NS-F13-FUNCTION-CODE+"
 "+NS-F14-FUNCTION-CODE+" "+NS-F15-FUNCTION-CODE+"
 "+NS-F16-FUNCTION-CODE+" "+NS-F17-FUNCTION-CODE+"
 "+NS-F18-FUNCTION-CODE+" "+NS-F19-FUNCTION-CODE+"
 "+NS-F20-FUNCTION-CODE+" "+NS-F21-FUNCTION-CODE+"
 "+NS-F22-FUNCTION-CODE+" "+NS-F23-FUNCTION-CODE+"
 "+NS-F24-FUNCTION-CODE+" "+NS-F25-FUNCTION-CODE+"
 "+NS-F26-FUNCTION-CODE+" "+NS-F27-FUNCTION-CODE+"
 "+NS-F28-FUNCTION-CODE+" "+NS-F29-FUNCTION-CODE+"
 "+NS-F30-FUNCTION-CODE+" "+NS-F31-FUNCTION-CODE+"
 "+NS-F32-FUNCTION-CODE+" "+NS-F33-FUNCTION-CODE+"
 "+NS-F34-FUNCTION-CODE+" "+NS-F35-FUNCTION-CODE+"
 "+NS-INSERT-FUNCTION-CODE+" "+NS-DELETE-FUNCTION-CODE+"
 "+NS-HOME-FUNCTION-CODE+" "+NS-BEGIN-FUNCTION-CODE+"
 "+Ns-END-FUNCTION-CODE+" "+NS-PAGE-UP-FUNCTION-CODE+"
 "+NS-PAGE-DOWN-FUNCTION-CODE+" "+NS-PRINT-SCREEN-FUNCTION-CODE+"
 "+NS-SCROLL-LOCK-FUNCTION-CODE+" "+NS-PAUSE-FUNCTION-CODE+"
 "+NS-SYS-REQ-FUNCTION-CODE+" "+NS-BREAK-FUNCTION-CODE+"
 "+NS-RESET-FUNCTION-CODE+" "+NS-STOP-FUNCTION-CODE+"
 "+NS-MENU-FUNCTION-CODE+" "+NS-USER-FUNCTION-CODE+"
 "+NS-SYSTEM-FUNCTION-CODE+" "+NS-PRINT-FUNCTION-CODE+"
 "+NS-CLEAR-LINE-FUNCTION-CODE+" "+NS-CLEAR-DISPLAY-FUNCTION-CODE+"
 "+NS-INSERT-LINE-FUNCTION-CODE+" "+NS-DELETE-LINE-FUNCTION-CODE+"
 "+NS-INSERT-CHAR-FUNCTION-CODE+" "+NS-DELETE-CHAR-FUNCTION-CODE+"
 "+NS-PREV-FUNCTION-CODE+" "+NS-NEXT-FUNCTION-CODE+"
 "+NS-SELECT-FUNCTION-CODE+" "+NS-EXECUTE-FUNCTION-CODE+"
 "+NS-UNDO-FUNCTION-CODE+" "+NS-REDO-FUNCTION-CODE+"
 "+NS-FIND-FUNCTION-CODE+" "+NS-HELP-FUNCTION-CODE+"
 "+NS-MODE-SWITCH-FUNCTION-CODE+")



(defparameter *ns-code-to-name*
  `((,+ns-up-arrow-function-code+ . ns-up-arrow-function)
    (,+ns-down-arrow-function-code+ . ns-down-arrow-function)
    (,+ns-left-arrow-function-code+ . ns-left-arrow-function)
    (,+ns-right-arrow-function-code+ . ns-right-arrow-function)
    (,+ns-f1-function-code+ . ns-f1-function)
    (,+ns-f2-function-code+ . ns-f2-function)
    (,+ns-f3-function-code+ . ns-f3-function)
    (,+ns-f4-function-code+ . ns-f4-function)
    (,+ns-f5-function-code+ . ns-f5-function)
    (,+ns-f6-function-code+ . ns-f6-function)
    (,+ns-f7-function-code+ . ns-f7-function)
    (,+ns-f8-function-code+ . ns-f8-function)
    (,+ns-f9-function-code+ . ns-f9-function)
    (,+ns-f10-function-code+ . ns-f10-function)
    (,+ns-f11-function-code+ . ns-f11-function)
    (,+ns-f12-function-code+ . ns-f12-function)
    (,+ns-f13-function-code+ . ns-f13-function)
    (,+ns-f14-function-code+ . ns-f14-function)
    (,+ns-f15-function-code+ . ns-f15-function)
    (,+ns-f16-function-code+ . ns-f16-function)
    (,+ns-f17-function-code+ . ns-f17-function)
    (,+ns-f18-function-code+ . ns-f18-function)
    (,+ns-f19-function-code+ . ns-f19-function)
    (,+ns-f20-function-code+ . ns-f20-function)
    (,+ns-f21-function-code+ . ns-f21-function)
    (,+ns-f22-function-code+ . ns-f22-function)
    (,+ns-f23-function-code+ . ns-f23-function)
    (,+ns-f24-function-code+ . ns-f24-function)
    (,+ns-f25-function-code+ . ns-f25-function)
    (,+ns-f26-function-code+ . ns-f26-function)
    (,+ns-f27-function-code+ . ns-f27-function)
    (,+ns-f28-function-code+ . ns-f28-function)
    (,+ns-f29-function-code+ . ns-f29-function)
    (,+ns-f30-function-code+ . ns-f30-function)
    (,+ns-f31-function-code+ . ns-f31-function)
    (,+ns-f32-function-code+ . ns-f32-function)
    (,+ns-f33-function-code+ . ns-f33-function)
    (,+ns-f34-function-code+ . ns-f34-function)
    (,+ns-f35-function-code+ . ns-f35-function)
    (,+ns-insert-function-code+ . ns-insert-function)
    (,+ns-delete-function-code+ . ns-delete-function)
    (,+ns-home-function-code+ . ns-home-function)
    (,+ns-begin-function-code+ . ns-begin-function)
    (,+ns-end-function-code+ . ns-end-function)
    (,+ns-page-up-function-code+ . ns-page-up-function)
    (,+ns-page-down-function-code+ . ns-page-down-function)
    (,+ns-print-screen-function-code+ . ns-print-screen-function)
    (,+ns-scroll-lock-function-code+ . ns-scroll-lock-function)
    (,+ns-pause-function-code+ . ns-pause-function)
    (,+ns-sys-req-function-code+ . ns-sys-req-function)
    (,+ns-break-function-code+ . ns-break-function)
    (,+ns-reset-function-code+ . ns-reset-function)
    (,+ns-stop-function-code+ . ns-stop-function)
    (,+ns-menu-function-code+ . ns-menu-function)
    (,+ns-user-function-code+ . ns-user-function)
    (,+ns-system-function-code+ . ns-system-function)
    (,+ns-print-function-code+ . ns-print-function)
    (,+ns-clear-line-function-code+ . ns-clear-line-function)
    (,+ns-clear-display-function-code+ . ns-clear-display-function)
    (,+ns-insert-line-function-code+ . ns-insert-line-function)
    (,+ns-delete-line-function-code+ . ns-delete-line-function)
    (,+ns-insert-char-function-code+ . ns-insert-char-function)
    (,+ns-delete-char-function-code+ . ns-delete-char-function)
    (,+ns-prev-function-code+ . ns-prev-function)
    (,+ns-next-function-code+ . ns-next-function)
    (,+ns-select-function-code+ . ns-select-function)
    (,+ns-execute-function-code+ . ns-execute-function)
    (,+ns-undo-function-code+ . ns-undo-function)
    (,+ns-redo-function-code+ . ns-redo-function)
    (,+ns-find-function-code+ . ns-find-function)
    (,+ns-help-function-code+ . ns-help-function)
    (,+ns-mode-switch-function-code+ . ns-mode-switch-function)))


;;;; THE END ;;;;
