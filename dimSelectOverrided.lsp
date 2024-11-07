(defun c:dimSelectOverrided (/ ss ssNew i ename vlaObj textOverrided textMeasurement) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "函数已取消" "quit / exit abort"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (vl-load-com)
  (princ "\n")

  (setq ss (ssget "_X" '((0 . "*dimension"))))
  (setq i 0)
  (repeat (sslength ss) 
    (if 
      (and (setq ename (ssname ss i)) 
           (setq vlaObj (vlax-ename->vla-object ename))
           (not 
             (vl-catch-all-error-p 
               (setq textOverrided (vl-catch-all-apply 'vla-get-textoverride 
                                                       (list vlaObj)
                                   )
               )
             )
           )
           (not 
             (vl-catch-all-error-p 
               (setq textMeasurement (vl-catch-all-apply 'vla-get-measurement 
                                                         (list vlaObj)
                                     )
               )
             )
           )
      )
      ;; debug
      ; (print (strcat "textmeasurement: " (rtos textMeasurement 2 2)))
      ; (print (strcat "textoverride: " textOverrided))
      (if 
        (not 
          (or (wcmatch textOverrided "*<>*") 
              (= textOverrided "")
              (= (atof textOverrided) textMeasurement)
          )
        )
        (progn 
          (if (not ssNew) 
            (setq ssNew (ssadd))
          )
          (ssadd ename ssNew)
        )
      )

      (setq i (+ 1 i))
    )
  )

  (if ssNew 
    (sssetfirst nil ssNew)
    (princ "没有虚标数值的标注\n")
  )

  (princ)
)