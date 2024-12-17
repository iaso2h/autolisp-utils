; Credit: https://www.cadtutor.net/forum/topic/62651-convert-quotattributes-valuequot-to-text/
; Converts attributes (attr. definitions, tags) to plain texts
(defun attr2TextConvert (ss silentChk / acDoc ssNew ssl i eNameSrc eNameNew entNew 
                         grp grplst addg
                        ) 
  (if ss 
    (progn 
      (vl-load-com)
      (princ "\n")
      (defun *error* (msg) 
        (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
          (princ (strcat "Error: " msg "\n"))
        )
        (princ)
      )
      (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-startundomark acDoc)

      (setq ssl (sslength ss)
            i   0
      )
      (setq grplst (list 7 8 10 11 39 40 41 50 51 62 71 72 73))
      (setq ssNew (ssadd))

      (while (< i ssl) 
        (setq eNameSrc (ssname ss i))
        (setq entSrc (entget eNameSrc))
        (setq entNew '((0 . "TEXT")))
        (setq entNew (append entNew (list (cons 1 (cdr (assoc 2 entSrc))))))

        (foreach grp grplst 
          (setq addg (assoc grp entSrc))
          (if (/= addg nil) 
            (setq entNew (append entNew (list (assoc grp entSrc))))
          )
        )

        (if (entmake entNew) 
          (progn 
            (setq eNameNew (entlast))
            (ssadd eNameNew ssNew)

            (vla-put-truecolor 
              (vlax-ename->vla-object eNameNew)
              (vla-get-truecolor 
                (vlax-ename->vla-object eNameSrc)
              )
            )
            (entdel eNameSrc)
          )
        )

        (setq i (1+ i))
      )

      (vla-endundomark acDoc)

      (if (null silentChk) 
        (if (> ssl 1) 
          (progn (princ (strcat (rtos ssl 2 0) "个属性定义已被转换\n")) 
                 (sssetfirst nil ssNew)
          )
          (princ "无属性定义可以转换\n")
        )
      )

    )
  )

  ssNew
)

(defun C:attr2Text () 
  (attr2TextConvert 
    (setq ss (ssget "_:L" '((0 . "ATTDEF"))))
    nil
  )

  (princ)
)
(defun C:attr2TextAll () 
  (attr2TextConvert 
    (setq ss (ssget "_X" '((0 . "ATTDEF"))))
    nil
  )

  (princ)
)