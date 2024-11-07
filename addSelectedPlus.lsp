; File: addSeelctedPlus.lsp
; Author: iaso2h
; Description: 增强版添加选择, 支持T20V10
; Version: 0.1.1
; Last Modified: 2024-11-04

(defun c:addSelectedPlus (/ ss eName eType vlaObj savedLastEnt) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if osm (setvar 'osmode osm))
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

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
      (setq eName (ssname ss 0))
      (setq eType (cdr (assoc 0 (entget eName))))
      (setq vlaObj (vlax-ename->vla-object eName))
      (cond 
        ;; Tangent {{{
        ;; 线图案
        ((= eType "TCH_PATH_ARRAY")
         (iterCopyProperty "._TLinePattern" vlaObj (list))
        )
        ;; 用地红线
        ((= eType "TCH_AREAREDLINE")
         (iterCopyProperty "._DrawRedLine" vlaObj (list))
        )
        ;; 坐标标注
        ((= eType "TCH_COORD")
         (iterCopyProperty 
           "._TCOORD"
           vlaObj
           (append 
             (list 'TextHeight 'TextStyle 'TextColor 'Precision 'Text2BaseRatio 
                   'CoordShowMode
             )
             (list 'Style)
           )
         )
        )
        ;; 图名标注
        ((= eType "TCH_DRAWINGNAME")
         (iterCopyProperty 
           "._TGMAPNAME"
           vlaObj
           (list 'NameStyle 'NameHeight 'NameHeight 
                 ;  'ScaleText
                 'SpaceCoef 'DimStyle 'TextColor 'ShowScale
           )
         )
        )
        ;; 云线
        ((= eType "TCH_MODI")
         (iterCopyProperty 
           "._TGREVCLOUD"
           vlaObj
           (list 'EditText 
                 'TextStyle
                 'TextHeight
                 'TextColor
           )
         )
        )
        ;; 连续标注
        ((= eType "TCH_DIMENSION") (command "._TDimMP"))
        ((= eType "TCH_DIMENSION2")
         (iterCopyProperty 
           "._TDimMP"
           vlaObj
           (list 
             ; 'DimStyle
             'Associate
             'ScaleFactors
           )
         )
        )
        ;; 标高标注
        ((= eType "TCH_ELEVATION")
         (iterCopyProperty 
           "._TMELEV"
           vlaObj
           (list 
             'TextColor
             'Text2BaseRatio
             'FileMode
           )
         )
        )
        ;; 天正单行文字
        ((= eType "TCH_TEXT")
         (iterCopyProperty 
           "._TTEXT"
           vlaObj
           (list 
                 ; 'Height
                 ; 'Alignment
                 ; 'Rotation
                 ; 'TextStyle
                 'XScale 'ObliqueAngle 
                 ; 'MaskFlag
           )
         )
        )
        ;; 天正多行文字
        ((= eType "TCH_MTEXT")
         (iterCopyProperty 
           "._TTEXT"
           vlaObj
           (list 
                 ; 'Height
                 ; 'Alignment
                 ; 'Rotation
                 ; 'TextStyle
                 ; 'PageWidth
                 ; 'LineSpace
           )
         )
        )
        ;; 引出标注
        ((= eType "TCH_MULTILEADER")
         (iterCopyProperty 
           "._TGLEADER"
           vlaObj
           (list 'Height 'FontStyle 'TextColor 'Text2BaseRatio 'AlignType 'UpTextType 
                 'DownTextType 'ArrowStyle 'ArrowSize 'MaskFlag
           )
         )
        )
        ;; 门窗
        ((= eType "TCH_OPENING") (command "._TOPENING"))
        ;; 门窗装饰套
        ((= eType "TCH_OPENINGSLOT") (command "._TOpeningSlot"))
        ;; 箭头引注
        ((= eType "TCH_ARROW")
         (iterCopyProperty 
           "._TGLEADER"
           vlaObj
           (list 'Height 'FontStyle 'TextColor 'Text2BaseRatio 'Alignment 'ArrowSize 
                 'ArrowStyle 'MaskFlag
           )
         )
        )
        ;; 半径标注、直径标注
        ((= eType "TCH_RADIUSDIM")
         (progn 
           (if (= (vlax-get-property vlaObj 'RadiusType) "半径") 
             (command "._TDIMRAD")
             (command "._TDIMDIA")
           )
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'ScaleFactors (entlast)) ; Buggy
         )
        )
        ;; 柱
        ((= eType "TCH_COLUMN")
         (progn 
           (if (= (vlax-get-property vlaObj 'StruSectionShapeText) "矩形") 
             (progn 
               (command "._TGCOLUMN")
               (while (= 1 (getvar "cmdactive")) 
                 (command pause)
               )
               (while (setq savedLastEnt (entnext (entlast))) 
                 (progn 
                   (copyPropertyGeneric vlaObj savedLastEnt)

                   (copyProperty vlaObj 'Elevation savedLastEnt)

                   (copyProperty vlaObj 'BottomFace savedLastEnt)
                   (copyProperty vlaObj 'TopFace savedLastEnt)

                   (copyProperty vlaObj 'Insulate savedLastEnt)

                   (copyProperty vlaObj 'InsulateLayer savedLastEnt)
                 )
               )
             )
             (progn 
               (if (= (vlax-get-property vlaObj 'Type) "构造柱") 
                 (command "._TFORTICOLU")
                 (command "._TPOLYCOLU")
               )
               (while (= 1 (getvar "cmdactive")) 
                 (command pause)
               )
               (while (setq savedLastEnt (entnext (entlast))) 
                 (progn 
                   (copyPropertyGeneric vlaObj savedLastEnt)

                   (copyProperty vlaObj 'Elevation savedLastEnt)
                   (copyProperty vlaObj 'Height savedLastEnt)

                   (copyProperty vlaObj 'BottomFace savedLastEnt)
                   (copyProperty vlaObj 'TopFace savedLastEnt)

                   (copyProperty vlaObj 'HatchLayer savedLastEnt)
                   (copyProperty vlaObj 'Insulate savedLastEnt)

                   (copyProperty vlaObj 'InsulateLayer savedLastEnt)
                   (copyProperty vlaObj 'Style savedLastEnt)
                 )
               )
             )
           )
         )
        )
        ;; 墙
        ((= eType "TCH_WALL")
         (progn 
           (iterCopyProperty 
             "._TGWALL"
             vlaObj
             (append 
               (list 
                 ;  'Elevation
                 ;  'LeftWidth
                 ;  'RightWidth
                 ;  'Height
               )
               (list 'BottomFace 
                     'TopFace
                     'Enclose
               )
               (list 
                 ;  'Usage
                 ;  'Style
                 'Insulate
               )
               (list 
                 'LeftLayer
                 'RightLayer
                 'HatchLayer
                 'InsulateLayer
               )
             )
           )
         )
        )
        ;; 玻璃幕墙
        ((= eType "TCH_CURTAIN_WALL") (command "._TConvertCurtain"))
        ;; 墙体切割
        ((= eType "TCH_KATANA") (command "._TKATANA"))
        ;; 墙体造型
        ((= eType "TCH_WALL_PATCH")
         (progn 
           (iterCopyProperty 
             "._TADDPATCH"
             vlaObj
             (append 
               (list 
                 'Elevation
                 'Height
               )
               (list 'LayerHatch 
                     'SurfLayer
               )
               (list 
                 'Insulate
               )
             )
           )
         )
        )
        ;; 转角窗
        ((= eType "TCH_CORNER_WINDOW")
         (progn 
           (iterCopyProperty 
             "._TCORNERWIN"
             vlaObj
             (append 
               (list 
                 'Elevation
                 'FrameHeight
                 'FrameThickness
               )
               (list 'IsProtrudeWin)
               (list 
                 'TextAngle
                 'TextHeight
                 'TextStyle
                 'Visible
               )
               (list 
                 'GlassLayer
                 'FrameLayer
                 'SlabLayer
                 'TextLayer
               )
               (list 
                 'IsHole
               )
             )
           )
         )
        )
        ;; 房间
        ((= eType "TCH_SPACE")
         (progn 
           (iterCopyProperty 
             "._TUPDSPACE"
             vlaObj
             (append 
               (list 'NameType 'TextHeight 'TextStyle 'TextHeightArea 'TextStyleArea)
               (list 
                     ;  'FloorThickness
                     ;  'StuccoThickness
                     'SpaceClearHeight 'AreaCoefficient 
                     ;  'SpaceType
               )
               (list 
                 'HatchLayer
               )
               (list 
                 'MaskFlag
                 'Floor3D
                 ;  'ShowOutLine
                 'ShowHatch
               )
             )
           )
         )
        )

        ;; 防火分区
        ((= eType "TCH_FIREZONE")
         (progn 
           (iterCopyProperty 
             "._TFIREZONECREATE"
             vlaObj
             (append 
               (list 'TextHeight 'TextStyle 'TextColor)
               (list 'IsDimFireZoneMask 'IsDimFireZoneName 'IsDimFireZoneUint 
                     'IsDimFunction 'IsDimSubNameAndArea 'IsDimSumArea 'AreaAccuracy 
                     'IsDimTextFrame
               )
               (list 
                 'HaveSubFireZone
               )
               (list 
                 'HatchLayer
               )
               (list 
                 'OutLineWidth
               )
             )
           )
         )
        )
        ;; 疏散路径
        ((= eType "TCH_EVACPATH")
         (progn 
           (iterCopyProperty 
             "._TSPACEEVACUATEPATH"
             vlaObj
             (list 
               'TextHeight
               'FontStyle
               'TextColor
             )
           )
         )
        )
        ;; 楼层框
        ((= eType "TCH_FLOORRECT") (command "._TFLOOR")) ;; TODO:
        ;; 任意坡顶
        ((= eType "TCH_SLOPEROOF")
         (progn 
           (command "._TSLOPEROOF")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj savedLastEnt)
           (copyProperty vlaObj 'Elevation savedLastEnt)
         )
         (command "._TSLOPEROOF")
        )
        ;; 老虎窗
        ((= eType "TCH_DORMER") (command "._TDORMER")) ;; Buggy
        ;; 对称轴
        ((= eType "TCH_SYMMETRY")
         (progn 
           (command "._TGSYMMETRICAL")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'Elevation savedLastEnt)
         )
        )
        ;; 做法标注
        ((= eType "TCH_COMPOSING")
         (progn 
           (command "._TGCOMPOSING")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'Elevation savedLastEnt)
         )
        )
        ;; 轴网标注
        ((= eType "TCH_AXIS_LABEL") ; TODO: Distinguish multiple axes and single axis
         (progn 
           (iterCopyProperty 
             "._TSINGLEAXISDIM"
             vlaObj
             (append 
               (list 'TextStyle 'Radius 'TextRatio)
               (list 'TextLayer)
             )
           )
         )
        ) ; TODO:
        ;; 索引图名
        ((= eType "TCH_DRAWINGINDEX")
         (progn 
           (iterCopyProperty 
             "._TGINDEXDIM"
             vlaObj
             (append 
               (list 'FontStyle 'ShowName 'TextColor)
               (list 'Diameter 'OldStyle 'TextRatio 'MaskFlag)
             )
           )
         )
        )
        ;; 指向索引
        ((= eType "TCH_INDEXPOINTER")
         (progn 
           (iterCopyProperty 
             "._TSINGLEAXISDIM"
             vlaObj
             (append 
               (list 'UpTextType 'DownTextType 'TextColor)
               (list 'OldStyle 'Diameter 'TextRatio 'Text2BaseRatio 'FrameLineStyle 
                     'FrameLineWidth
               )
             )
           )
         )
         (command "._TINDEXPTR")
        ) ; TODO: Distinguish multiple axes and single
        ;; 剖切索引
        ;; ((= eType "TCH_INDEXPOINTER") (command "._TINDEXPTR"))
        ;; 指北针
        ((= eType "TCH_NORTHTHUMB")
         (progn 
           (command "._TNORTHTHUMB")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'Elevation savedLastEnt)
         )
        )
        ;; 内视符号
        ((= eType "TCH_TDBINSIGHT")
         (progn 
           (iterCopyProperty 
             "._TGINSIGHT"
             vlaObj
             (append 
               (list 'FontStyle 'TextColor)
               (list 'Angle 'Diameter 'CircleText 'TextRatio 'MaskFlag)
             )
           )
         )
        )
        ;; 切割线
        ((= eType "TCH_CUT")
         (progn 
           (command "._TGSYMBCUT")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'Elevation savedLastEnt)
         )
        )
        ;; 剖面剖切
        ((= eType "TCH_SYMB_SECTION")
         (progn 
           (iterCopyProperty 
             "._TGSECTION"
             vlaObj
             (list 'Height 'FontStyle 'ConnerNoteNum 'IsShowIndex 'DrawHeight 
                   'DrawStyle 'DrawLocal 'DrawDirect 'DrawModulus 'TextColor 'SectionType 
                   'SectionStyle 'ShowSectionLine 'SectionLineLineTypePro 'SectionLineColor
             )
           )
         )
        )
        ;; 天正表格
        ((= eType "TCH_SHEET")
         (progn 
           (iterCopyProperty 
             "._TNEWSHEET"
             vlaObj
             (append 
               (list 'CellTextStyle 'CellTextSize 'CellTextColor 'CellSpaceFactor 
                     'CellHAlign 'CellVAlign 'CellLineWrap
               )
               (list 'HasHLine 'HLineColor 'HLineStyle 'HLineWidth 'RowHeightProperty)
               (list 
                 'HasVLine
                 'VLineColor
                 'VLineStyle
                 'VLineWidth
               )
               (list 
                 'HasBorder
                 'BorderColor
                 'BorderStyle
                 'BorderWidth
               )
               (list 'Ttile 'TitleTextStyle 'TitleTextSize 'TitleTextColor 
                     'TitleSpaceFactor 'TitleHAlign 'TitleVAlign 'TitleBaseLineColor 
                     'TitleBaseLineStyle 'TitleBaseLineWidth 'TitleOutside 'TitleHide
               )
               (list 'AllTextStyle 'AllTextSize 'AllTextColor 'AllSpaceFactor 
                     'AllHAlign 'AllVAlign 'CellLinePropertyInherit 'CellLineWrap
               )
               (list 'RowHeight 'RowHeightProperty 'RowInheritSheet 'RowHasBaseLine 
                     'RowHLineColor 'RowHLineStyle 'RowHLineWidth
               )
               (list 'ColWidth 'ColTextStyle 'ColTextSize 'ColTextColor 
                     'ColSpaceFactor 'ColHAlign 'ColInheritSheet 'ColLineWrap 'ColAllowDrag 
                     'ColHasRightLine 'ColRLineColor 'ColRLineStyle 'ColRLineWidth
               )
             )
           )
         )
        )
        ;; 台阶
        ((= eType "TCH_STEP")
         (progn 
           (command "._TSTEP")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'Elevation savedLastEnt)
         )
        )
        ;; 直线楼梯
        ((= eType "TCH_LINESTAIR")
         (progn 
           (iterCopyProperty 
             "._TLSTAIR"
             vlaObj
             (list 'AlongWall)
           )
         )
        )
        ;; 双跑楼梯
        ((= eType "TCH_RECTSTAIR")
         (progn 
           (iterCopyProperty 
             "._TRSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'EvacuateDiaColor
               )
             )
           )
         )
        )
        ;; 圆弧楼梯
        ((= eType "TCH_ARCSTAIR")
         (progn 
           (iterCopyProperty 
             "._TASTAIR"
             vlaObj
             (append 
               (list 'AlongWall)
               (list 
                 'BeamLayer
                 'StepLayer
               )
             )
           )
         )
        )
        ;; 多跑楼梯
        ((= eType "TCH_MULTISTAIR")
         (progn 
           (iterCopyProperty 
             "._TMULTISTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'AlongWall
               )
             )
           )
         )
        )
        ;; 双分平行楼梯
        ((= eType "TCH_PARALLELSTAIR")
         (progn 
           (iterCopyProperty 
             "._TDRAWPARALLELSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'EvacuateDiaColor
               )
             )
           )
         )
        )
        ;; 双分转角楼梯
        ((= eType "TCH_CORNERSTAIR")
         (progn 
           (iterCopyProperty 
             ".TDRAWCORNERSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
             )
           )
         )
        )
        ;; 双分三跑楼梯
        ((= eType "TCH_DOUBLEMULSTAIR")
         (progn 
           (iterCopyProperty 
             ".TDRAWDOUBLEMULSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
             )
           )
         )
        )
        ;; 交叉楼梯
        ((= eType "TCH_SCISSORSSTAIR")
         (progn 
           (iterCopyProperty 
             ".TDRAWSCISSORSSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'EvacuateDiaColor
               )
             )
           )
         )
        )
        ;; 剪刀梯
        ((= eType "TCH_CROSSSTAIR")
         (progn 
           (iterCopyProperty 
             ".TDRAWCROSSSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'EvacuateDiaColor
               )
             )
           )
         )
        )
        ;; 三角楼梯
        ((= eType "TCH_TRIANGLESTAIR")
         (progn 
           (iterCopyProperty 
             ".TDRAWTRIANGLESTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'EvacuateDiaColor
               )
             )
           )
         )
        )
        ;; 转角楼梯
        ((= eType "TCH_RECTCORNERSTAIR")
         (progn 
           (iterCopyProperty 
             ".TDRAWRECTCORNERSTAIR"
             vlaObj
             (append 
               (list 'TextHeight 'StruTextHeight 'StruFontStyle 'TextUp 'TextDown)
               (list 
                 'SlabThick
               )
               (list 
                 'StepLayer
                 'RailLayer
                 'HandRailLayer
                 'ArrowLayer
               )
               (list 
                 'EvacuateDiaColor
               )
             )
           )
         )
        )
        ;; 扶手
        ((= eType "TCH_HANDRAIL")
         (progn 
           (command "._THANDRAIL")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
           (copyProperty vlaObj 'Shape savedLastEnt)
         )
        )
        ;; 坡道
        ((= eType "TCH_ASCENT")
         (progn 
           (command "._TASCENT")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
         )
        )
        ;; 阳台
        ((= eType "TCH_BALCONY")
         (progn 
           (iterCopyProperty 
             "._TBALCONY"
             vlaObj
             (append 
               (list 'SlabThick)
               (list 'GroundLayer)
             )
           )
         )
        )
        ;; 平板
        ((= eType "TCH_SLAB")
         (progn 
           (command "._TSLAB")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
         )
        )
        ;; 散水
        ((= eType "TCH_APRON")
         (progn 
           (command "._TOUTLNA")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
         )
        )
        ;; 三维网架
        ((= eType "TCH_NETSHELF")
         (progn 
           (command ".TNETSHELF")
           (while (= 1 (getvar "cmdactive")) 
             (command pause)
           )
           (setq savedLastEnt (entlast))
           (copyPropertyGeneric vlaObj (entlast))
         )
        )
        ;; 图块
        ((= eType "TCH_TCH_BLOCK_INSERT")
         (progn 
           (iterCopyProperty 
             "._TKW"
             vlaObj
             (list)
           )
         )
        )
        ; }}}

        ;; Default
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
(defun iterCopyProperty (cmdName vlaObj propertyList / savedLastEnt) 
  (command cmdName)

  (while (= 1 (getvar "cmdactive")) 
    (command pause)
  )

  (while (setq savedLastEnt (entnext (entlast))) 
    (progn 
      (copyPropertyGeneric vlaObj savedLastEnt)
      (mapcar 
        '(lambda (property) 
           (copyProperty vlaObj property savedLastEnt)
         )
        propertyList
      )
    )
  )
)
(defun copyProperty (vlaObj symbol entTgt) 
  (if (vlax-property-available-p vlaObj symbol) 
    (vlax-put-property (vlax-ename->vla-object entTgt) 
                       symbol
                       (vlax-get-property vlaObj symbol)
    )
  )
)
(defun copyPropertyGeneric (vlaObjSrc entTgt) 
  (if (= (getvar "pstylemode") 0) 
    (copyProperty vlaObjSrc 'PlotStyleName entTgt)
  )
  (vla-put-truecolor 
    (vlax-ename->vla-object entTgt)
    (vla-get-truecolor vlaObjSrc)
  )
  ; (copyProperty vlaObj 'Color ent)
  (copyProperty vlaObjSrc 'Layer entTgt)
  (copyProperty vlaObjSrc 'Linetype entTgt)
  ; (copyProperty vlaObj 'LinetypeScale ent)
  ; (copyProperty vlaObj 'Lineweight ent)
  (copyProperty vlaObjSrc 'EntityTransparency entTgt)
  (copyProperty vlaObjSrc 'LayoutRotation entTgt)
  (copyProperty vlaObjSrc 'ObjectControl entTgt)
  (copyProperty vlaObjSrc 'Material entTgt)
  (copyProperty vlaObjSrc 'ShadowType entTgt)
)
