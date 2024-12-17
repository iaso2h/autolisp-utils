;; Quick Block
;; Creates a block instantly out of the objects that you select
;; Reference: http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Quick-block/td-p/3454228
;;
(defun c:blockCreateInplace (/ ss i insertPoint savedEntLast ent) 
  (load "attr2Text.lsp")
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
      (setq savedEntLast (entlast))
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq eName (cdr (assoc 0 (entget ent))))
        (cond 
          ((eq eName "ATTDEF")
           (ssadd (ssname (attr2TextConvert (ssadd ent (ssadd)) t) 0) ss)
          )
          ((eq eName "INSERT")
           (progn 
             (command "_explode" ent)
             (while (setq ent (entnext savedEntLast)) 
               (setq savedEntLast ent)
               (ssadd savedEntLast ss)
             )
           )
          )
        )

        (setq i (1+ i))
      )

      (insertBlock insertPoint ss)
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
  (command "_.-block" blockName insertPoint ss "")
  (command "_.-insert" blockName insertPoint "" "" "")

  (princ)
)

(defun c:blockCreateInplaceByBlock (/ cmd ss blockColor blockEnt blockObj) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)

  (c:blockCreateInplace)
  (setq blockEnt (entlast))
  (setq blockObj (vlax-ename->vla-object blockEnt))
  (setq blockColor (acad_colordlg 1))
  (if (null blockColor) 
    (setq blockColor 1)
  )
  (vla-put-color blockObj blockColor)
  (vla-put-layer blockObj "0")
  
  (setq ss (ssadd))
  (ssadd blockEnt ss)
  (load "blockColor.lsp")
  (blockColorSelectionSet ss 0 t)
  (command "undo" "e")
  (setvar 'cmdecho cmd)

  ; (princ)
)
