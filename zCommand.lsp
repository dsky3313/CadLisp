;;--------------------------------------------------
(defun c:bb()
    (command "-boundary")
    (princ )
)
;;--------------------------------------------------

;;--------------------------------------------------
(defun c:d()
    (command "DIST")
    (princ )
)
;;--------------------------------------------------
(defun c:dd()
    (command "DIMLINEAR")
    (princ )
)
;;--------------------------------------------------
(defun c:dc()
    (command "DIMCONTINUE")
    (princ )
)
;;--------------------------------------------------
;;--------------------------------------------------
(defun c:ee()
    (command "EXTRUDE")
    (princ )
)
;;--------------------------------------------------
(defun c:eee()
    (command "solidedit" "f" "e")
    (princ )
)
;;--------------------------------------------------
(defun c:F()
  (command "FILLET" "u")
  (princ )
)
;;--------------------------------------------------
(defun c:ft()
  (command "flatten")
  (princ )
)
;;--------------------------------------------------
(defun c:li()
  (command "layiso")
  (princ )
)
;;--------------------------------------------------
(defun c:lk()
  (command "LAYLCK")
  (princ )
)
;;--------------------------------------------------
(defun c:ll()
  (command "LAYMCUR")
  (princ )
)
;;--------------------------------------------------
(defun C:LO()
  (command "layuniso")
  (princ )
)
;;--------------------------------------------------
(defun C:Lon()
  (command "layon")
  (princ )
)
;;--------------------------------------------------
(defun C:Loo()
  (command "layoff")
  (princ )
)
;;--------------------------------------------------
(defun c:luk()
(command "layulk")
(princ )
)
;;--------------------------------------------------
(defun c:ok()
  (command "overkill")
  (princ )
)
;;--------------------------------------------------
(defun c:pd()
  (command "pdfimport" "")
  (princ )
)
;;--------------------------------------------------
(defun c:qs()
  (command "qselect")
  (princ )
)
;;--------------------------------------------------
(defun c:TE()
  ;;(C:TXTEXP)
  (command "TXTEXP")
  (princ )
)
;;--------------------------------------------------

(defun c:uu()
  (command "UNION")
  (princ )
)
;;--------------------------------------------------
(defun c:vv()
  (command "-view" "top")
  (princ "\n View    TOP (평면도)")
  (princ )
)
;;--------------------------------------------------
(defun c:v`()
  (command "-view" "_seiso")
  (setvar "PERSPECTIVE" 0)
  (princ "\n View    SE ISO")
  (princ )
)
;;--------------------------------------------------
(defun c:v1()
  (command "-view" "front")
  (setvar "PERSPECTIVE" 0)
  (princ "\n View    FRONT (정면도)")
  (princ )
)
;;--------------------------------------------------
(defun c:v2()
  (command "-view" "left")
  (setvar "PERSPECTIVE" 0)
  (princ "\n View    LEFT (좌측면도)")
  (princ )
)
;;--------------------------------------------------
(defun c:v3()
  (command "-view" "right")
  (setvar "PERSPECTIVE" 0)
  (princ "\n View    RIGHT (우측면도)")
  (princ )
)
;;--------------------------------------------------
(defun c:wq()
  (command "_shademode" "o")
  (princ "\n Visual Style    3D")
  (princ )
)
;;--------------------------------------------------
(defun c:ww()
  ;(command "VSCURRENT" "2")
  (command "_shademode" "2")
  (princ "\n Visual Style    2D")
  (princ )
)
;;--------------------------------------------------
(defun c:`()
  (command "UCS" "3")
  (princ "\n UCS 3 point")
  (princ )
)
;;--------------------------------------------------
(defun c:``()
  (command "plan" "c")
  (princ "\n PLAN    Current UCS")
  (princ )
)
;;--------------------------------------------------
(defun c:2()
  (command "move")
  (princ )
)
;;--------------------------------------------------
(defun c:22()
  (command "move" "m2p")
  (princ )
)
;;--------------------------------------------------
(defun c:3()
  (command "OFFSET")
  (princ )
)

