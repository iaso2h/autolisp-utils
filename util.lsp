(defun iaso2h:layerSetXline (savedEntLast / tmp vlaObj) 
  (vl-load-com)
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (command "undo" "be")

  (if (not (tblsearch "layer" "xline")) 
    (command "-layer" "n" "xline" "p" "n" "xline" "d" "辅助线图层，不可打印！" "xline" "c" "41" 
             "xline" ""
    )
  )
  (if 
    (and (null savedEntLast) 
         (setq savedEntLast (entlast))
    )
    (progn 
      (setq vlaObj (vlax-ename->vla-object savedEntLast))
      (vla-put-color vlaObj 256)
      (vlax-put-property vlaObj 'Layer "xline")
    )
  )
  (if savedEntLast 
    (progn 
      (while (setq tmp (entnext savedEntLast)) 
        (setq savedEntLast tmp)
        (setq vlaObj (vlax-ename->vla-object savedEntLast))
        (vla-put-color vlaObj 256)
        (vlax-put-property vlaObj 'Layer "xline")
      )
    )
  )

  (command "undo" "be")
  (setvar 'cmdecho cmd)

  (princ)
)
(defun iaso2h:entlast (/ ent tmp) 
  (setq ent (entlast))
  (while (setq tmp (entnext ent)) (setq ent tmp))
  ent
)

;;-------------------=={ UnFormat String }==------------------;;
;;                                                            ;;
;;  Returns a string with all MText formatting codes removed. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  str - String to Process                                   ;;
;;  mtx - MText Flag (T if string is for use in MText)        ;;
;;------------------------------------------------------------;;
;;  Returns:  String with formatting codes removed            ;;
;;------------------------------------------------------------;;

(defun LM:UnFormat ( str mtx / _replace rx )

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq str
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                            (foreach pair
                               '(
                                    ("\032"    . "\\\\\\\\")
                                    (" "       . "\\\\P|\\n|\\t")
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq str (_replace (car pair) (cdr pair) str))
                            )
                            (if mtx
                                (_replace "\\\\" "\032" (_replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" str))
                                (_replace "\\"   "\032" str)
                            )
                        )
                    )
                )
            )
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p str))
                str
            )
        )
    )
)
(vl-load-com)