(defun iaso2h:layerSetXline (savedEntLast / tmp vlaObj) 
  (vl-load-com)
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command "undo" "be")

  (if (not (tblsearch "layer" "xline")) 
    (command "-layer" "n" "xline" "p" "n" "xline" "d" "辅助线图层，不可打印！" "xline" "c" "41" 
             "xline" ""
    )
  )
  (if 
    (and (null savedEntLast) 
         (setq savedEntLast (entlast))
    )
    (progn 
      (setq vlaObj (vlax-ename->vla-object savedEntLast))
      (vla-put-color vlaObj 256)
      (vlax-put-property vlaObj 'Layer "xline")
    )
  )
  (if savedEntLast 
    (progn 
      (while (setq tmp (entnext savedEntLast)) 
        (setq savedEntLast tmp)
        (setq vlaObj (vlax-ename->vla-object savedEntLast))
        (vla-put-color vlaObj 256)
        (vlax-put-property vlaObj 'Layer "xline")
      )
    )
  )

  (command "undo" "be")
  (setvar 'cmdecho cmd)

  (princ)
)
(defun iaso2h:entlast (/ ent tmp) 
  (setq ent (entlast))
  (while (setq tmp (entnext ent)) (setq ent tmp))
  ent
)