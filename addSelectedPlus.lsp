(defun c:addSelectedPlus (/ debugMode debugTextInsertPoint ss ename ent eType) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq debugMode nil)

  (if 
    (not 
      (setq ss (ssget "_I"))
    )
    (vl-catch-all-error-p 
      (setq ss (vl-catch-all-apply 'ssget (list "_:S+.")))
    )
  )
  (if ss 
    (progn 
      (setq ename (ssname ss 0))
      (setq eType (cdr (assoc 0 (entget ename))))
      (cond 
        ;; Tangent {{{
        ;; 线图案
        ((= eType "TCH_PATH_ARRAY") (command "._TLinePattern"))
        ;; 用地红线（显示有错误）
        ((= eType "TCH_AREAREDLINE") (command "._DrawRedLine"))
        ;; 坐标标注
        ((= eType "TCH_COORD") (command "._TCOORD"))
        ;; 图名标注
        ((= eType "TCH_DRAWINGNAME") (command "._TGMAPNAME"))
        ;; 云线
        ((= eType "TCH_MODI") (command "._TGREVCLOUD"))
        ;; 连续标注
        ((= eType "TCH_DIMENSION") (command "._TDimMP"))
        ((= eType "TCH_DIMENSION2") (command "._TDimMP"))
        ;; 标高标注
        ((= eType "TCH_ELEVATION") (command "._TMELEV"))
        ;; 天正单行文字
        ((= eType "TCH_TEXT") (command "._TTEXT"))
        ;; 天正多行文字
        ((= eType "TCH_MTEXT") (command "._TMTEXT"))
        ;; 引出标注
        ((= eType "TCH_MULTILEADER") (command "._TGLEADER"))
        ;; 门窗
        ((= eType "TCH_OPENING") (command "._TOPENING"))
        ;; 门窗装饰套
        ((= eType "TCH_OPENINGSLOT") (command "._TOpeningSlot"))
        ;; 箭头引注
        ((= eType "TCH_ARROW") (command "._TGARROW"))
        ;; 半径标注
        ((= eType "TCH_RADIUSDIM") (command "._TDIMRAD")) ; TODO:
        ;; ((= eType "TCH_RADIUSDIM") (command "._TDIMDIA"))
        ;; 柱
        ((= eType "TCH_COLUMN") (command "._TGCOLUMN"))
        ;; 墙
        ((= eType "TCH_WALL") (command "._TGWALL"))
        ;; 玻璃幕墙
        ((= eType "TCH_CURTAIN_WALL") (command "._TGWALL")) ;; TODO:
        ;; 墙体切割
        ((= eType "TCH_KATANA") (command "._TCH_KATANA"))
        ;; 墙体造型
        ((= eType "TCH_WALL_PATCH") (command "._TADDPATCH"))
        ;; 转角窗
        ((= eType "TCH_CORNER_WINDOW") (command "._TCORNERWIN"))
        ;; 房间
        ((= eType "TCH_SPACE") (command "._TUPDSPACE"))
        ;; 防火分区
        ((= eType "TCH_FIREZONE") (command "._TFIREZONECREATE"))
        ;; 疏散路径
        ((= eType "TCH_EVACPATH") (command "._TSPACEEVACUATEPATH"))
        ;; 楼层框
        ((= eType "TCH_FLOORRECT") (command "._TFLOOR")) ;; TODO:
        ;; 任意坡顶
        ((= eType "TCH_SLOPEROOF") (command "._TSLOPEROOF"))
        ;; 老虎窗
        ((= eType "TCH_DORMER") (command "._TDORMER"))
        ;; 对称轴
        ((= eType "TCH_SYMMETRY") (command "._TGSYMMETRICAL"))
        ;; 做法标注
        ((= eType "TCH_COMPOSING") (command "._TGCOMPOSING"))
        ;; 轴网标注
        ((= eType "TCH_AXIS_LABEL") (command "._TSINGLEAXISDIM")) ; TODO:
        ;; 索引图名
        ((= eType "TCH_DRAWINGINDEX") (command "._TGINDEXDIM"))
        ;; 指向索引
        ((= eType "TCH_INDEXPOINTER") (command "._TINDEXPTR")) ; TODO:
        ;; 剖切符号
        ;; ((= eType "TCH_INDEXPOINTER") (command "._TINDEXPTR"))
        ;; 指北针
        ((= eType "TCH_NORTHTHUMB") (command "._TNORTHTHUMB"))
        ;; 内视符号
        ((= eType "TCH_TDBINSIGHT") (command "._TGINSIGHT"))
        ;; 切割线
        ((= eType "TCH_CUT") (command "._TGSYMBCUT"))
        ;; 剖面剖切
        ((= eType "TCH_SYMB_SECTION") (command "._TGSECTION"))
        ;; 天正表格
        ((= eType "TCH_SHEET") (command "._TNEWSHEET"))
        ;; 台阶
        ((= eType "TCH_STEP") (command "._TSTEP"))
        ;; 直线楼梯
        ((= eType "TCH_LINESTAIR") (command "._TLSTAIR"))
        ;; 双跑楼梯
        ((= eType "TCH_RECTSTAIR") (command "._TRSTAIR"))
        ;; 圆弧楼梯
        ((= eType "TCH_ARCSTAIR") (command "._TASTAIR"))
        ;; 任意楼梯
        ((= eType "TCH_CURVESTAIR") (command "._TCSTAIR"))
        ;; 多跑楼梯
        ((= eType "TCH_MULTISTAIR") (command "._TMULTISTAIR"))
        ;; 扶手
        ((= eType "TCH_HANDRAIL") (command "._THANDRAIL"))
        ;; 坡道
        ((= eType "TCH_ASCENT") (command "._TASCENT"))
        ;; 阳台
        ((= eType "TCH_BALCONY") (command "._TBALCONY"))
        ;; 平板
        ((= eType "TCH_SLAB") (command "._TSLAB"))
        ;; 图块
        ((= eType "TCH_TCH_BLOCK_INSERT") (command "._TKW"))
        ; }}}
        (debugMode
         (progn 
           (setq debugTextInsertPoint (getpoint 
                                        "选择调试文字的位置: \n"
                                      )
           )
           (command "._text" debugTextInsertPoint 300 0 eType)
         )
        )
        (t
         (progn 
           (sssetfirst nil nil)
           (command "._addselected" ss)
         )
        )
      )
    )
  )

  (princ)
)

(defun iaso2h:entlast ( / ent tmp )
    (setq ent (entlast))
    (while (setq tmp (entnext ent)) (setq ent tmp))
    ent
)