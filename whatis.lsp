(defun c:whatIs (/ activeDoc sset ent obj entType objType savedEntLast 
                 inspectatioinText
                ) 
  (vl-load-com)
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (setq sset (ssget "_I")) 
    (setq ent (ssname sset 0))
    (setq ent (car (entsel "\n选择图元：")))
  )
  (if ent 
    (progn 
      (if 
        (vl-catch-all-error-p 
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        )
        (setq objType "No Info")
        (setq objType (vla-get-ObjectName obj))
      )

      (setq entType (cdr (assoc 0 (entget ent))))
      (princ (strcat entType "\n"))
      (princ (strcat objType "\n"))
      (setq savedEntLast (entlast))
      (setq inspectatioinText (getpoint "\n插入文字: "))
      (command "_text" 
               "j"
               "mc"
               inspectatioinText
               25
               0
               (strcat entType "\n" objType)
      )

      (if *searchIncluded* 
        (progn 
          (load "util.lsp")
          (iaso2h:layerSetXline savedEntLast)
        )
      )
    )
  )

  (princ)
)