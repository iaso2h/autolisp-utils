(defun c:whatIsInside (/ sset eName vlaObj eType) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (if (setq sset (ssget "_I")) 
    (setq eName (ssname sset 0))
    (setq eName (car (entsel "\n选择图元：")))
  )
  (if eName 
    (progn 
      (setq eType (cdr (assoc 0 (entget eName))))
      (if 
        (not 
          (vl-catch-all-error-p 
            (setq vlaObj (vl-catch-all-apply 'vlax-ename->vla-object 
                                            (list eName)
                        )
            )
          )
        )
        ; )
        (progn 
          (vlax-dump-object vlaObj)
        )
      )
    )
  )

  (princ)
)