(defun c:dimRemoveFontOverride (/ ss i ent obj dimTextOverride cnt) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (if *searchIncluded* 
    (progn 
      (load "util.lsp")
    )
  )

  (if (setq ss (ssget "_X" '((0 . "*DIMENSION")))) 
    (progn 
      (command "undo" "be")
      (setq i 0)
      (setq cnt 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq dimTextOverride (vla-get-TextOverride obj))
        (if (wcmatch dimTextOverride "*\*;*") 
          (vla-put-TextOverride obj (LM:UnFormat dimTextOverride nil))
          (setq cnt (1+ cnt))
        )

        (setq i (1+ i))
      )
      (command "undo" "e")
      (if (> cnt 0) (princ (strcat (rtos cnt 2 0) "\n个标准尺寸的覆盖文字已经被清除\n\n")))
    )
  )

  (princ)
)
