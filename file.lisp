;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements the file dialogs.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-18 <PJB> Created.
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
(objcl:enable-objcl-reader-macros)


(defvar *default-directory* nil)

(defun panel-directory (panel)
  (pathname (concatenate 'string (objcl:lisp-string
                                  #-cocoa-10.6
                                  [panel directory]
                                  #+cocoa-10.6
                                  [[panel directoryURL] path]) "/")))

(defun panel-path (panel)
  (pathname (objcl:lisp-string
             #-cocoa-10.6
             [panel filename]
             #+cocoa-10.6
             [(objc:send panel "URL") path])))


(defun path-to-pathname (path)
  (let ((last/ (position #\/ path :from-end t))
        (last. (position #\. path :from-end t)))
    (when (and last. (if (null last/) (= 0 last.) (<= last. (1+ last/))))
      (setf last. nil))
    (flet ((escape-path (path)
             (with-output-to-string (*standard-output*)
               (loop
                 :for i :from 0
                 :for ch :across path
                 :do (cond
                       ((or (find ch "/-_")
                            (eql i last.)
                            (alphanumericp ch))
                        (write-char ch))
                       (t
                        (write-char #\\)
                        (write-char ch)))))))
      (pathname (escape-path path)))))

#-(and)(
        (path-to-pathname "/Users/pjb/Documents/Patchwork/PW-user-patches/B/éponge rand*dens x>")
        (with-open-file (in #P"/Users/pjb/Documents/Patchwork/PW-user-patches/B/éponge rand\\*dens x>")
          (list (read in) (read in)))

        (wild-pathname-p #P"/Users/pjb/Documents/Patchwork/PW-user-patches/B/éponge rand\\*dens x>" )
        (namestring  #P"/Users/pjb/Documents/Patchwork/PW-user-patches/B/éponge rand\\*dens x>")
        )


(defun choose-file-dialog (&key
                             (directory *default-directory*)
                             file-types
                             (button-string "Open")
                             (prompt "Open a file"))
  "
The CHOOSE-FILE-DIALOG function displays the standard Macintosh
SFGetFile dialog box, allowing you to select a file for reading.
Unless the dialog is canceled, this function returns a pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

FILE-TYPES:     An os-type parameter or list of os-type parameters.  If
                specified, only files with the given Macintosh file type are
                displayed in the dialog box.  Os-types are case sensitive.

BUTTON-STRING:  A string. Specifies the text that appears in the
                button that opens the chosen file. The default is
                Open.

PROMPT:         A string, displayed as title of the choose file dialog.
"
  (declare (ignore button-string))
  (let ((panel [NSOpenPanel openPanel]))
    [panel setCanChooseFiles:t]
    [panel setCanChooseDirectories:nil]
    [panel setResolvesAliases:t]
    [panel setAllowsMultipleSelection:nil]
    ;; ---
    ;; [panel setTitle:(objcl:objc-string title)]
    ;; [panel setMessage:(objcl:objc-string message)]
    ;; [panel setNameFieldLabel:(objcl:objc-string label)]
    [panel setPrompt:(objcl:objc-string prompt)]
    [panel setCanCreateDirectories:nil]
    [panel setCanSelectHiddenExtension:t]
    [panel setExtensionHidden:nil]
    [panel setTreatsFilePackagesAsDirectories:nil]
    [panel setAllowsOtherFileTypes:t]
    #+cocoa-10.6 [panel setShowsHiddenFiles:nil]
    (if file-types
        [panel setAllowedFileTypes:(list-to-nsarray (ensure-list file-types))]
        [panel setAllowedFileTypes:nil])
    ;; --
    #-cocoa-10.6
    [panel setDirectory:(objcl:objc-string (namestring directory))]
    #+cocoa-10.6
    (progn
      [panel setDirectoryURL:[NSURL fileURLWithPath:(objcl:objc-string (namestring directory)) isDirectory:t]]
      [panel setNameFieldStringValue:(objcl:objc-string "")])
    (when (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (setf *default-directory* (panel-directory panel))
      (let* ((urls  (objc:send panel "URLs"))
             (files '()))
        (dotimes (i [urls count])
          (push (path-to-pathname (objcl:lisp-string [[urls objectAtIndex:i] path])) files))
        (if (endp (rest files))
            (first files)
            files)))))



(defun choose-new-file-dialog (&key
                                 (directory *default-directory*)
                                 (prompt "Save a new file")
                                 (button-string "Save file"))
  "
The CHOOSE-NEW-FILE-dialog function displays the standard Macintosh
SFPutFile dialog box, allowing you to specify a destination file for
writing. An alert dialog box requests confirmation if an existing file
is chosen.  Unless canceled, it returns a pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

BUTTON-STRING:  A string. Specifies the text that appears in the
                button that opens the chosen file. The default is
                Save As.

PROMPT:         A string, displayed as title of the choose new file dialog.
"
  (declare (ignore button-string))
  (let ((panel [NSSavePanel savePanel]))
    ;; [panel setTitle:(objcl:objc-string title)]
    ;; [panel setMessage:(objcl:objc-string message)]
    ;; [panel setNameFieldLabel:(objcl:objc-string label)]
    [panel setPrompt:(objcl:objc-string prompt)]
    [panel setCanCreateDirectories:t]
    [panel setCanSelectHiddenExtension:t]
    [panel setExtensionHidden:nil]
    [panel setTreatsFilePackagesAsDirectories:nil]
    [panel setAllowsOtherFileTypes:t]
    #+cocoa-10.6 [panel setShowsHiddenFiles:nil]
    [panel setAllowedFileTypes:nil]
    ;; --
    #-cocoa-10.6
    [panel setDirectory:(objcl:objc-string (namestring directory))]
    #+cocoa-10.6
    (progn
      [panel setDirectoryURL:[NSURL fileURLWithPath:(objcl:objc-string (namestring directory)) isDirectory:t]]
      [panel setNameFieldStringValue:(objcl:objc-string "")])
    (when (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (setf *default-directory* (panel-directory panel))
      (panel-path panel))))



(defun choose-directory-dialog (&key
                                  (directory *default-directory*)
                                  (prompt "Select a directory"))
  "
The function CHOOSE-DIRECTORY-DIALOG displays a variation of the
standard Macintosh SfGetFile dialog box.  Unless canceled, it returns
a directory pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

PROMPT:         A string, displayed as title of the choose directory dialog.
"
  (let ((panel [NSOpenPanel openPanel]))
    [panel setCanChooseFiles:nil]
    [panel setCanChooseDirectories:t]
    [panel setResolvesAliases:t]
    [panel setAllowsMultipleSelection:nil]
    ;; ---
    ;; [panel setTitle:(objcl:objc-string title)]
    ;; [panel setMessage:(objcl:objc-string message)]
    ;; [panel setNameFieldLabel:(objcl:objc-string label)]
    [panel setPrompt:(objcl:objc-string prompt)]
    [panel setCanCreateDirectories:t]
    [panel setCanSelectHiddenExtension:t]
    [panel setExtensionHidden:nil]
    [panel setTreatsFilePackagesAsDirectories:nil]
    [panel setAllowsOtherFileTypes:t]
    #+cocoa-10.6 [panel setShowsHiddenFiles:nil]
    [panel setAllowedFileTypes:nil]
    ;; --
    #-cocoa-10.6
    [panel setDirectory:(objcl:objc-string (namestring directory))]
    #+cocoa-10.6
    (progn
      [panel setDirectoryURL:[NSURL fileURLWithPath:(objcl:objc-string (namestring directory)) isDirectory:t]]
      [panel setNameFieldStringValue:(objcl:objc-string "")])
    (when (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (setf *default-directory* (panel-directory panel))
      (let* ((urls  (objc:send panel "URLs"))
             (files '()))
        (dotimes (i [urls count])
          (push (pathname (concatenate 'string (objcl:lisp-string [[urls objectAtIndex:i] path]) "/")) files))
        (if (endp (rest files))
            (first files)
            files)))))


(defun choose-file-default-directory ()
  "
The function CHOOSE-FILE-DEFAULT-DIRECTORY returns the namestring of
the last directory selected by the CHOOSE-FILE-DIALOG,
CHOOSE-NEW-FILE-DIALOG, or CHOOSE-DIRECTORY-DIALOG dialog box.
Initially, this is the directory that is the translation of \"home:\".
"
  *default-directory*)


(defun set-choose-file-default-directory (pathname)
  "
The function SET-CHOOSE-FILE-DEFAULT-DIRECTORY sets the default
directory used by the CHOOSE-FILE-DIALOG, CHOOSE-NEW-FILE-DIALOG, or
CHOOSE-DIRECTORY-DIALOG dialog box to pathname.  It returns pathname.
"
  (setf *default-directory* (pathname pathname)))





(defparameter *ascii-characters*
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defparameter *macroman-characters*
  "ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄¤‹›ﬁﬂ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ˜¯˘˙˚¸˝˛ˇ")

(defun macroman-char (code)
  (cond
    ((< code   0) (error "Not a Mac Roman character code: ~d" code))
    ((< code  32) (code-char code))
    ((< code 127) (aref *ascii-characters* (- code 32)))
    ((= code 127) #\Rubout)
    ((< code 256) (aref *macroman-characters* (- code 128)))
    (t            (error "Not a Mac Roman character code: ~d" code))))

(defun macroman-code (ch)
  (let ((code (char-code ch)))
    (cond
      ((< code 32) code)
      ((eql #\Rubout ch) 127)
      ((let ((pos (position ch *ascii-characters*)))
         (when pos (+ pos 32))))
      ((let ((pos (position ch *macroman-characters*)))
         (when pos (+ pos 128))))
      (t
       (error "Not a Mac Roman character: '~C'" ch)))))


(defun mactype-from-string (type-string)
  (check-type type-string (string 4))
  (loop
    :for ch :across type-string
    :for code = (macroman-code ch)
    :for n = code :then (+ (* n 256) code)
    :finally (return n)))

(defun mactype-to-string (type-number)
  (check-type type-number (integer 0 #xffffffff))
  (loop
    :with result = (make-string 4)
    :for i :from 0
    :for pos :from 24 :downto 0 :by 8
    :do (setf (aref result i) (macroman-char (ldb (byte 8 pos) type-number)))
    :finally (return result)))


(defun mac-file-type (path)
  (mactype-to-string [[[[NSFileManager defaultManager]
                        attributesOfItemAtPath:(objcl:objc-string (namestring (truename path)))
                        error:oclo:*null*] objectForKey:#$NSFileHFSTypeCode] unsignedLongValue]))

(defun mac-file-creator (path)
  (mactype-to-string [[[[NSFileManager defaultManager]
                        attributesOfItemAtPath:(objcl:objc-string (namestring (truename path)))
                        error:oclo:*null*] objectForKey:#$NSFileHFSCreatorCode] unsignedLongValue]))

(defun set-mac-file-type (path type)
  [[NSFileManager defaultManager]
   setAttributes: [NSDictionary dictionaryWithObjects:[NSArray arrayWithObject:[NSNumber numberWithUnsignedLong:(mactype-from-string (string type))]]
                                forKeys:[NSArray arrayWithObject:#$NSFileHFSTypeCode]]
   ofItemAtPath:(objcl:objc-string (namestring (truename path)))
   error:oclo:*null*])

(defun set-mac-file-creator (path creator)
  [[NSFileManager defaultManager]
   setAttributes: [NSDictionary dictionaryWithObjects:[NSArray arrayWithObject:[NSNumber numberWithUnsignedLong:(mactype-from-string (string creator))]]
                                forKeys:[NSArray arrayWithObject:#$NSFileHFSCreatorCode]]
   ofItemAtPath:(objcl:objc-string (namestring (truename path)))
   error:oclo:*null*])


(defun full-pathname (path)
  (handler-case (truename path)
    (error () (translate-logical-pathname path))))


(defun initialize/file ()
  (setf *default-directory* (user-homedir-pathname)))




;;;; THE END ;;;;
