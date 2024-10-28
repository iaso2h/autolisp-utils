(defun c:addSelectedPlus (/ debugMode debugTextInsertPoint ss sset sfirst typ) 
  (defun *error* (msg) 
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq debugMode nil)
  (princ "\n")

  (if (not (setq sset (ssget "_I"))) 
    (progn 
      (setq sset (entsel "\n请选择图元: "))
      (setq sfirst nil)

      (setq ss (car sset))
      (setq typ (cdr (assoc 0 (entget ss))))
      (cond 
        ;; Tangent {{{
        ;; 线图案
        ((= typ "TCH_PATH_ARRAY") (command "._TLinePattern"))
        ;; 用地红线（显示有错误）
        ((= typ "TCH_AREAREDLINE") (command "._DrawRedLine"))
        ;; 坐标标注
        ((= typ "TCH_COORD") (command "._TCOORD"))
        ;; 图名标注
        ((= typ "TCH_DRAWINGNAME") (command "._TGMAPNAME"))
        ;; 云线
        ((= typ "TCH_MODI") (command "._TGREVCLOUD"))
        ;; 连续标注
        ((= typ "TCH_DIMENSION") (command "._TDimMP"))
        ((= typ "TCH_DIMENSION2") (command "._TDimMP"))
        ;; 标高标注
        ((= typ "TCH_ELEVATION") (command "._TMELEV"))
        ;; 天正单行文字
        ((= typ "TCH_TEXT") (command "._TTEXT"))
        ;; 天正多行文字
        ((= typ "TCH_MTEXT") (command "._TMTEXT"))
        ;; 引出标注
        ((= typ "TCH_MULTILEADER") (command "._TGLEADER"))
        ;; 门窗
        ((= typ "TCH_OPENING") (command "._TOPENING"))
        ;; 箭头引注
        ((= typ "TCH_ARROW") (command "._TGARROW"))
        ;; 半径标注
        ((= typ "TCH_RADIUSDIM") (command "._TDIMRAD")) ; TODO:
        ;; ((= typ "TCH_RADIUSDIM") (command "._TDIMDIA"))
        ;; 柱
        ((= typ "TCH_COLUMN") (command "._TGCOLUMN"))
        ;; 墙
        ((= typ "TCH_WALL") (command "._TGWALL"))
        ;; 玻璃幕墙
        ((= typ "TCH_CURTAIN_WALL") (command "._TGWALL"))
        ;; 转角窗
        ((= typ "TCH_CORNER_WINDOW") (command "._TCORNERWIN"))
        ;; 房间
        ((= typ "TCH_SPACE") (command "._TUPDSPACE"))
        ;; 防火分区
        ((= typ "TCH_FIREZONE") (command "._TFIREZONECREATE"))
        ;; 疏散路径
        ((= typ "TCH_EVACPATH") (command "._TSPACEEVACUATEPATH"))
        ;; 楼层框
        ((= typ "TCH_FLOORRECT") (command "._TFLOOR")) ;; TODO:
        ;; 任意坡顶
        ((= typ "TCH_SLOPEROOF") (command "._TSLOPEROOF"))
        ;; 老虎窗
        ((= typ "TCH_DORMER") (command "._TDORMER"))
        ;; 对称轴
        ((= typ "TCH_SYMMETRY") (command "._TGSYMMETRICAL"))
        ;; 做法标注
        ((= typ "TCH_COMPOSING") (command "._TGCOMPOSING"))
        ;; 轴网标注
        ((= typ "TCH_AXIS_LABEL") (command "._TSINGLEAXISDIM")) ; TODO:
        ;; 索引图名
        ((= typ "TCH_DRAWINGINDEX") (command "._TGINDEXDIM"))
        ;; 指向索引
        ((= typ "TCH_INDEXPOINTER") (command "._TINDEXPTR")) ; TODO:
        ;; 剖切符号
        ;; ((= typ "TCH_INDEXPOINTER") (command "._TINDEXPTR"))
        ;; 指北针
        ((= typ "TCH_NORTHTHUMB") (command "._TNORTHTHUMB"))
        ;; 内视符号
        ((= typ "TCH_TDBINSIGHT") (command "._TGINSIGHT"))
        ;; 切割线
        ((= typ "TCH_CUT") (command "._TGSYMBCUT"))
        ;; 剖面剖切
        ((= typ "TCH_SYMB_SECTION") (command "._TGSECTION"))
        ;; 天正表格
        ((= typ "TCH_SHEET") (command "._TNEWSHEET"))
        ;; 台阶
        ((= typ "TCH_STEP") (command "._TSTEP"))
        ;; 直线楼梯
        ((= typ "TCH_LINESTAIR") (command "._TLSTAIR"))
        ;; 双跑楼梯
        ((= typ "TCH_RECTSTAIR") (command "._TRSTAIR"))
        ;; 圆弧楼梯
        ((= typ "TCH_ARCSTAIR") (command "._TASTAIR"))
        ;; 任意楼梯
        ((= typ "TCH_CURVESTAIR") (command "._TCSTAIR"))
        ;; 多跑楼梯
        ((= typ "TCH_MULTISTAIR") (command "._TMULTISTAIR"))
        ;; 坡道
        ((= typ "TCH_ASCENT") (command "._TASCENT"))
        ;; 阳台
        ((= typ "TCH_BALCONY") (command "._TBALCONY"))
        ;; 平板
        ((= typ "TCH_SLAB") (command "._TSLAB"))
        ;; 图块
        ((= typ "TCH_TCH_BLOCK_INSERT") (command "._TKW"))
        ; }}}
        (debugMode
         (progn 
           (setq debugTextInsertPoint (getpoint 
                                        "选择调试文字的位置: \n"
                                      )
           )
           (command "._text" debugTextInsertPoint 300 0 typ)
         )
        )
        (t
         (command "._addselected" sset)
        )
      )
    )
    (progn 
      (sssetfirst nil nil)
      (command "._addselected" "_P")
    )
  )


  (princ)
)
