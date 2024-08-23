(defun C:fontGBSwap (/ st simfangPath simkaiPath) 
  (setq simfangPath "C:\\Windows\\Fonts\\simfang.ttf")
  (setq simkaiPath "C:\\Windows\\Fonts\\simkai.ttf")
  (vlax-for f (vla-get-TextStyles (vla-get-activedocument (vlax-get-acad-object))) 
    (vla-GetFont f 'typeFace 'Bold 'Italic 'charSet 'PitchandFamily)
    (if (wcmatch (strcase typeFace) "*FANGSONG_GB2312*") 
      (if (findfile simfangPath) 
        (progn 
          (vla-put-fontfile f simfangPath)
          (princ 
            (strcat 
              "\n"
              typeFace
              " has replaced by 仿宋.ttf"
            )
          )
        )
      )
      (if (wcmatch (strcase typeFace) "*仿宋_GB2312*") 
        (if (findfile simfangPath) 
          (progn 
            (vla-put-fontfile f simfangPath)
            (princ 
              (strcat 
                "\n"
                typeFace
                " has replaced by 仿宋.ttf"
              )
            )
          )
        )
        (if (wcmatch (strcase typeFace) "*KAITI_GB2312*") 
          (if (findfile simfangPath) 
            (progn 
              (vla-put-fontfile f simkaiPath)
              (princ 
                (strcat 
                  "\n"
                  typeFace
                  " has replaced by 楷体.ttf"
                )
              )
            )
          )
        )
      )
    )
  )
  (command "_.regen")
  (princ)
)