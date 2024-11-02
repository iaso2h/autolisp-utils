(defun c:dimSelectOverrided (/ ss ssNew i ename vlaObj textOverrided textMeasurement) 
  (vl-load-com)
  (setq ss (ssget "_X" '((0 . "*dimension"))))
  (setq i 0)
  (repeat (sslength ss) 
    (setq ename (ssname ss i))
    (setq vlaObj (vlax-ename->vla-object ename))
    (setq textOverrided (vla-get-textoverride vlaObj))
    (setq textMeasurement (vla-get-measurement vlaObj))
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

  (sssetfirst nil ssNew)

  (princ)
)
