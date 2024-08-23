(defun c:whatis (/ activeDoc sset ename vlObj eType vlType inspectatioinText) 
  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if (setq sset (ssget "_I")) 
    (setq ename (ssname sset 0))
    (setq ename (car (entsel "\n选择图元: ")))
  )
  (if ename 
    (progn 
      (setq vlObj (vlax-ename->vla-object ename))
      (setq eType (cdr (assoc 0 (entget ename))))
      (setq vlType (vla-get-ObjectName vlObj))
      (print eType)
      (print vlType)
      (setq inspectatioinText (getpoint "\n插入文字: "))
      ; (vla-sendcommand activeDoc
      (command "_text" "j" "mc" inspectatioinText 25 0 (strcat eType "\n" vlType))

      (load "LayerSetXline.lsp")
      (layerSetXline)
    )
  )

  (princ)
)