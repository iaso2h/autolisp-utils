(defun c:dimSelectOverrided (/ ss ssNew i ename vlObj textOverrided)
  (setq ss (ssget "_X" '((0 . "*dimension"))))
  (setq i 0)
  (repeat (sslength ss)
    (setq ename (ssname ss i))
    (setq vlObj (vlax-ename->vla-object ename))
    (setq textOverrided (vla-get-textoverride vlObj))
    (if
      (not
        (or (wcmatch textOverrided "*<>*")
            (= textOverrided "")
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
