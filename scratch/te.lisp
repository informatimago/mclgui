(in-package :ui)

'(word-break-function
  click-loop-function
  te-caret-function
  te-selection-function
  te-just-left
  te-just-right
  te-just-center
  te-no-wrap
  te-word-wrap
  *caret-half-period*
  *selection-color*
  terec
  te-init
  te-default-word-break
  te-default-caret-hook
  te-default-high-hook
  te-set-rects
  te-set-text
  te-get-text
  te-cal-text



  (%te-adjust-starts charpos increment te)
  (%te-copy te)
  (%te-delete te)
  (%te-insert text te)
  (%te-move-gap-to parno te)
  (%te-split-paragraph parno te)
  (adjustable-string string size)
  (integer-compare a b)
  (set-click-loop click-proc te)
  (set-word-break word-proc te)
  (te-activate te)
  (te-adjust-gap increment te)
  (te-backward-char mods code char te)
  (te-beep mods code char te)
  (te-beginning-of-line mods code char te)
  (te-bind chord command te)
  (te-cal-text te)
  (te-calculate-text te)
  (te-call-caret-hook te)
  (te-caret-transition tick te)
  (te-change-selection extend new-point te)
  (te-clean-selection start end te)
  (te-click pt extend te)
  (te-column charpos te)
  (te-compute-caret-rect te)
  (te-compute-selection-rectangles start end te)
  (te-copy te)
  (te-current-paragraph te)
  (te-cut te)
  (te-deactivate te)
  (te-default-caret-hook rect lino te)
  (te-default-high-hook rects start-lino end-lino te)
  (te-default-word-break text charpos)
  (te-delete te)
  (te-delete-backward-char mods code char te)
  (te-delete-char mods code char te)
  (te-dispose te)
  (te-draw-line lino te)
  (te-end-of-line mods code char te)
  (te-enter mods code char te)
  (te-erase-caret te)
  (te-forward-char mods code char te)
  (te-from-scrap )
  (te-get-binding chord te)
  (te-get-scrap-len )
  (te-get-text te)
  (te-has-caret te)
  (te-idle te)
  (te-index-of-paragraph-at charpos te)
  (te-init )
  (te-insert text te)
  (te-invariant te)
  (te-is-active te)
  (te-key char te)
  (te-kill-line mods code char te)
  (te-kill-region mods code char te)
  (te-line-coordinates lino te)
  (te-line-at charpos te)
  (te-new dest-rect view-rect window)
  (te-newline mods code char te)
  (te-next-line mods code char te)
  (te-open-line mods code char te)
  (te-paragraph parno te)
  (te-paste te)
  (te-previous-line mods code char te)
  (te-recenter-top-bottom mods code char te)
  (te-redraw-line lino te)
  (te-scrap-handle )
  (te-scroll dh dv te)
  (te-scroll-up mods code char te)
  (te-self-insert mods code char te)
  (te-set-crap-len length)
  (te-set-default-bindings te)
  (te-set-just just te)
  (te-set-mark mods code char te)
  (te-set-rects dest-rect view-rect te)
  (te-set-select start end te)
  (te-set-text text te)
  (te-split-paragraph-at charpos te)
  (te-string-width string te &optional start end)
  (te-line lino te)
  (te-to-scrap )
  (te-transpose-chars mods code char te)
  (te-undo mods code char te)
  (te-update update-rect te)
  (te-update-font-info te)
  (te-update-view clip-rect te)
  (te-whitespacep char)
  (te-wrap-paragraph para te)
  (te-yank mods code char te)
  (text-box text box just)

  )
