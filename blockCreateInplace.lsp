;; Quick Block
;; Creates a block instantly out of the objects that you select
;; Reference: http://forums.autodesk.com/t5/Visual-LISP-AutoLISP-and-General/Quick-block/td-p/3454228
;;
(defun c:blockCreateInplace (/ ss insertPoint cnt blockName timeStamp) 
  (princ "\n")
  (defun *error* (msg) 
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq timeStamp (rtos (getvar "CDATE") 2 6))
  (if 
    (and 
      (list (setq ss (ssget "_:L")) 
            (setq insertPoint (getpoint "请指定插入点：\n"))
      )
    )
    (progn 
      (setq cnt       1
            blockName (strcat timeStamp (itoa cnt))
      )
      (while (tblsearch "BLOCK" blockName) 
        (setq blockName (strcat timeStamp (itoa (setq cnt (1+ cnt)))))
      )
      (command "_.-Block" blockName insertPoint ss "")
      (command "_.-insert" blockName insertPoint "" "" "")
    )
  )

  (princ)
)
