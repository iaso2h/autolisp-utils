(defun c:whatis (/ activeDoc sset ename vlObj eType vlType inspectatioinText) 
  (vl-load-com)
  (defun *error* (msg) 
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (if (setq sset (ssget "_I")) 
    (setq ename (ssname sset 0))
    (setq ename (car (entsel "\n选择图元：")))
  )
  (if ename 
    (progn 
      (if 
        (vl-catch-all-error-p 
          (setq vlObj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
        )
        (setq vlType "No Info")
        (setq vlType (vla-get-ObjectName vlObj))
      )

      (setq eType (cdr (assoc 0 (entget ename))))
      (princ (strcat eType "\n"))
      (princ (strcat vlType "\n"))
      (setq inspectatioinText (getpoint "\n插入文字: "))
      (command "_text" "j" "mc" inspectatioinText 25 0 (strcat eType "\n" vlType))

      (if *searchIncluded* 
        (progn 
          (load "LayerSetXline.lsp")
          (layerSetXline)
        )
      )
    )
  )

  (princ)
)