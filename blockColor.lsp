; Credit: http://www.mjtd.com/thread-74564-1-1.html
(defun c:blockColor () 
  (blockColorSelectionSet nil nil nil)
  (princ)
)

(defun blockColorSelectionSet (ss blkColor skipColorPrompt / blks i obj blkNames) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (if (null ss) 
    (setq ss (ssget "_:L" '((0 . "insert"))))
  )


  (if ss 
    (progn 
      ; Set default value to blkColor
      (if (null blkColor) 
        (setq blkColor 1)
      )
      (if (null skipColorPrompt) 
        (setq blkColor (acad_colordlg blkColor))
      )

      (setq blks (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))))
      (repeat (setq i (sslength ss)) 
        (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
        (chBlockColor blks obj blkColor)
      )
    )
  )

  (princ)
)

(defun chBlockColor (blks Obj color / blkName oName) 
  (if 
    (and (= (vla-get-ObjectName obj) "AcDbBlockReference") 
         (= (vla-get-HasAttributes obj) :vlax-true)
    )
    (foreach x (vlax-invoke obj 'getattributes) 
      (vla-put-color x color)
    )
  )
  (setq blkName (vla-get-name obj))
  (if (not (member blkName blkNames)) 
    (progn 
      (setq blkNames (cons blkName blkNames))
      (vlax-for x (vla-item blks blkName) 
        (setq oName (vla-get-ObjectName x))
        (cond 
          ((wcmatch oName "*Dimension,AcDbLeader,AcDbFcf")
           (vla-put-DimensionLineColor x color)
           (if (wcmatch oName "*Dimension") 
             (progn 
               ; TODO: doesn't has property extension line
               (vl-catch-all-apply 'vla-put-ExtensionLineColor (list x color))
               (if 
                 (setq blkName (assoc 2 
                                      (entget (vlax-vla-object->ename x))
                               )
                 )
                 (vlax-for x (vla-item blks (cdr blkName)) 
                   ; TODO: avoid locked layer
                   (vl-catch-all-apply 'vla-put-color (list x color))
                 )
               )
             )
             (if (wcmatch oName "*Dimension,AcDbFcf") 
               (vla-put-TextColor x color)
             )
           )
          )
          ((= oName "AcDbBlockReference")
           (chBlockColor blks x color)
          )
        )
        (vla-put-color x color)
      )
    )
  )
  (vla-UpDate obj)
  (princ)
)