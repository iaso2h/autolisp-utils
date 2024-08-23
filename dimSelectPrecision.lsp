(defun c:dimSelectPrecision (/ ss ss0 ss1 ss2 i ename vlObj vlType textPrecision ans tmp) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "函数已取消" "quit / exit abort"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq ss (ssget "_X" '((0 . "*dimension"))))
  (setq i 0)
  (repeat (sslength ss) 
    (setq ename (ssname ss i))
    (setq vlObj (vlax-ename->vla-object ename))
    (setq vlType (vla-get-ObjectName vlObj))
    (if (not (eq vlType "AcDb2LineAngularDimension")) 
      (progn 
        (setq textPrecision (vla-get-primaryunitsprecision vlObj))
        (cond 
          ((eq textPrecision 0)
           (progn 
             (if (null ss0) 
               (setq ss0 (ssadd))
             )
             (ssadd ename ss0)
           )
          )
          ((eq textPrecision 1)
           (progn 
             (if (null ss1) 
               (setq ss1 (ssadd))
             )
             (ssadd ename ss1)
           )
          )
          (t
           (progn 
             (if (null ss2) 
               (setq ss2 (ssadd))
             )
             (ssadd ename ss2)
           )
          )
        )
      )
    )

    (setq i (+ 1 i))
  )

  (while t 
    (if (null ans) 
      (setq ans "0位小数")
    )
    (initget "0位小数 1位小数 2位以及2位以上小数")
    (if (setq tmp (getkword (strcat "\n选择标注精确度[0位小数/1位小数/2位以及2位以上小数] <" ans ">: "))) 
      (setq ans tmp)
    )
    (cond 
      ((eq ans "0位小数")
       (if ss0
         (sssetfirst nil ss0)
         (prompt "\n提示：没有精确度为0位小数的标注")
       )
      )
      ((eq ans "1位小数")
       (if ss1
         (sssetfirst nil ss1)
         (prompt "\n提示：没有精确度为1位小数的标注")
       )
      )
      (t
       (if ss2
         (sssetfirst nil ss2)
         (prompt "\n提示：没有精确度为2位以及2位小数以上的标注")
       )
      )
    )

    (princ)
  )
)