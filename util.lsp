(defun iaso2h:layerSetXline (savedEntLast / vlaObj) 
  (vl-load-com)

  (if (not (tblsearch "layer" "xline")) 
    (command "-layer" "n" "xline" "p" "n" "xline" "d" "辅助线图层，不可打印！" "xline" "c" "41" 
             "xline" ""
    )
  )
  (while (setq savedEntLast (entnext savedEntLast)) 
    (progn 
      (command "._change" savedEntLast "" "p" "la" "xline" "")
    )
    (setq vlaObj (vlax-ename->vla-object savedEntLast))
    (vla-put-color vlaObj 256)
    (vlax-put-property vlaObj 'Layer "xline")
  )

  (princ)
)

(defun iaso2h:entlast (/ ent tmp) 
  (setq ent (entlast))
  (while (setq tmp (entnext ent)) (setq ent tmp))
  ent
)