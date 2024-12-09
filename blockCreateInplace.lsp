;; Quick Block
;; Creates a block instantly out of the objects that you select
;; Reference: http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Quick-block/td-p/3454228
;;
(defun c:blockCreateInplace (/ ss insertPoint cnt blockName timeStamp) 
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (if 
    (and 
      (setq ss (ssget "_:L"))
      (setq insertPoint (getpoint "请指定插入点：\n"))
    )
    (progn 
      (command "undo" "be")
      (insertBlock insertPoint ss)
    )
  )

  (command "undo" "e")
  (setvar 'cmdecho cmd)

  (princ)
)


(defun c:blockCreateInplaceByBlock (/ ssNew entLastSaved ent vla) 
  (vl-load-com)
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (if 
    (and 
      (setq ss (ssget "_:L"))
      (setq insertPoint (getpoint "请指定插入点：\n"))
    )
    (progn 
      (setq entLastSaved (entlast))
      (command "undo" "be")
      ; Explode 3 times in a row
      (repeat 3 
        ; Add unexplodable entities from old selection set
        (setq ssNew (ssadd))
        (command "_explode" ss)
        (setq i -1)
        (repeat (sslength ss) 
          (setq i (+ 1 i))
          (setq ent (ssname ss i))
          (if 
            (and ent 
                 (entget ent)
            )
            (ssadd ent ssNew)
          )
        )
        ; Add new entities from exploded elements
        ; TODO: convert attributes to text properly
        (while (setq ent (entnext entLastSaved)) 
          (setq entLastSaved ent)
          (ssadd entLastSaved ssNew)
        )

        ; Entering next loop
        (setq ss ssNew)
      )


      ; Change entity to byblock
      (setq i -1)
      (repeat (sslength ssNew) 
        (setq i (+ 1 i))
        (setq ent (ssname ssNew i))
        (setq vla (vlax-ename->vla-object ent))
        (vl-catch-all-apply 'vla-put-color (list vla 0))
      )

      (insertBlock insertPoint ssNew)
    )
  )

  (command "undo" "e")
  (setvar 'cmdecho cmd)

  (princ)
)


(defun insertBlock (insertPoint ss / timeStamp cnt blockName) 
  (setq timeStamp (rtos (getvar "CDATE") 2 6))
  (setq cnt       1
        blockName (strcat timeStamp (itoa cnt))
  )
  (while (tblsearch "BLOCK" blockName) 
    (setq blockName (strcat timeStamp (itoa (setq cnt (1+ cnt)))))
  )
  (command "_.-Block" blockName insertPoint ss "")
  (command "_.-insert" blockName insertPoint "" "" "")

  (princ)
)