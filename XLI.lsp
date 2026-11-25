;;; ==============================
;;; XLI.LSP
;;;  - 엑셀에서 선택한 셀 범위의 좌표를 점/폴리선/문자로 가져온다
;;;
;;; 원본파일: ZDREAM ZD_Civil.zrx 의 CIM (Excel 화면) 동작을 참고해 새로 작성
;;;
;;; 명령어: XLI
;;;
;;; 사용법
;;;  1) 좌표가 든 엑셀 파일을 열어 둔다
;;;  2) XLI 실행 후 열 형식과 삽입 객체(오른쪽)를 설정한다
;;;  3) 확인 -> 엑셀에 2칸 이상 선택해 둔 범위를 우선 쓰고, 없으면 범위 지정 창이 뜬다
;;;     ('엑셀 최소화'를 켜면 범위를 읽은 뒤 엑셀 창을 최소화)
;;;
;;; 가정 / 주의
;;;  - Excel 설치 필요(COM), LT 불가. 좌표는 현재 UCS 기준
;;;  - 숫자가 아닌 행(제목줄 등)은 건너뜀. Text 파일 가져오기는 없음
;;;
;;; 동작방식
;;;  - 대화상자(DCL)는 임시 파일로 만들어 쓰고 지운다
;;;  - Excel Selection.Value2 를 읽어 UCS->WCS 로 entmake, Undo 한 번에 되돌림
;;; ==============================

(vl-load-com)

;;------------------------------
;;  설정 (기본값. 로드할 때마다 다시 적용되고, 대화상자에서 바꾼 값은 다음 로드 전까지 유지)
;;------------------------------
(setq xli:fmt 4)  ; 열 형식 1~5
(setq xli:swap t)  ; X,Y 바꿔서 가져오기
(setq xli:mini t)  ; 범위를 읽은 뒤 엑셀 최소화
(setq xli:point t)  ; 점 만들기
(setq xli:psize 0.5)  ; 점 크기 (절대면 도면 단위, 상대면 화면 %)
(setq xli:prel nil)  ; T 면 화면에 상대적인 크기, nil 이면 절대 단위
(setq xli:pdmode 35)  ; 점 스타일 (원 + X)
(setq xli:pline t)  ; 폴리선 만들기
(setq xli:3d t)  ; 폴리선을 3D 로
(setq xli:text t)  ; 문자(번호) 쓰기
(setq xli:ztext t)  ; Z값 문자 쓰기
(setq xli:th 0.5)  ; 문자 높이
(setq xli:zth 0.5)  ; Z문자 높이
(setq xli:dx 1.0)  ; 문자 X 오프셋
(setq xli:dy 0.0)  ; 문자 Y 오프셋
(setq xli:zdx 1.0)  ; Z문자 X 오프셋
(setq xli:zdy -0.75)  ; Z문자 Y 오프셋 (높이 0.5 의 1.5배, 점 아래)
(setq xli:prec 3)  ; Z값 소수 자릿수
(setq xli:lay-pt "!POINT")
(setq xli:lay-ln "!LINE")
(setq xli:lay-tx "!TEXT")
(setq xli:lay-z "!Z")

;;------------------------------
;;  셀 값 변환
;;------------------------------
;; variant 면 값을 꺼내고 아니면 그대로
(defun xli:val (x)
  (if (= (type x) 'VARIANT)
    (vlax-variant-value x)
    x
  )
)

;; 셀 값 -> 실수 (숫자가 아니면 nil)
(defun xli:num (x / s)
  (setq x (xli:val x))
  (cond
    ((numberp x) (float x))
    ((= (type x) 'STR)
     (setq s (vl-string-trim " \t" (vl-string-subst "" "," x)))
     (if (/= s "") (distof s 2))
    )
    (t nil)
  )
)

;; 셀 값 -> 문자열 (비었으면 "")
(defun xli:str (x)
  (setq x (xli:val x))
  (cond
    ((= (type x) 'STR) x)
    ((numberp x) (rtos x 2 8))
    (t "")
  )
)

;;------------------------------
;;  엑셀 읽기
;;------------------------------
;; 실행 중인 엑셀을 우선 쓰고, 없으면 새로 띄운다
(defun xli:app (/ app)
  (setq app (vl-catch-all-apply 'vlax-get-object '("Excel.Application")))
  (if (or (null app) (vl-catch-all-error-p app))
    (setq app (vl-catch-all-apply 'vlax-get-or-create-object '("Excel.Application")))
  )
  (if (or (null app) (vl-catch-all-error-p app))
    nil
    app
  )
)

;; COM 속성 읽기. 실패하거나 객체가 없으면 기본값
(defun xli:prop (obj name default / v)
  (if obj
    (setq v (vl-catch-all-apply 'vlax-get-property (list obj name)))
  )
  (if (or (null v) (vl-catch-all-error-p v))
    default
    v
  )
)

;; 엑셀을 보이게 하고(최소화돼 있으면 복원) 포커스를 옮긴다
;; AppActivate 는 창 제목 앞부분 또는 끝부분이 맞아야 하므로 "문서명 - Excel" 두 형태를 다 시도한다
(defun xli:focus-excel (app / wsh state hwnd path file)
  (vl-catch-all-apply 'vlax-put-property (list app 'Visible :vlax-true))
  (setq state (xli:prop app 'WindowState 0))
  (if (and (numberp state) (= state -4140))
    (vl-catch-all-apply 'vlax-put-property (list app 'WindowState -4143))
  )
  (setq hwnd (xli:prop app 'Hwnd 0))
  (setq wsh (vl-catch-all-apply 'vlax-create-object '("WScript.Shell")))
  (if (and (numberp hwnd) (> hwnd 0) (not (vl-catch-all-error-p wsh)))
    (progn
      ;; 다른 프로그램 창은 그냥 앞으로 못 오므로(작업표시줄만 깜빡임)
      ;; Alt 키 입력을 흉내낸 뒤 SetForegroundWindow 를 호출한다
      (setq path (vl-filename-mktemp "xlifocus.ps1"))
      (setq file (open path "w"))
      (foreach line
        (list
          "Add-Type -Name W -Namespace X -MemberDefinition '[DllImport(\"user32.dll\")] public static extern bool SetForegroundWindow(IntPtr h); [DllImport(\"user32.dll\")] public static extern void keybd_event(byte b, byte s, uint f, UIntPtr e);'"
          "[X.W]::keybd_event(18, 0, 0, [UIntPtr]::Zero)"
          (strcat "【void】[X.W]::SetForegroundWindow([IntPtr]" (itoa hwnd) ")")
          "[X.W]::keybd_event(18, 0, 2, [UIntPtr]::Zero)"
        )
        (write-line line file)
      )
      (close file)
      (vl-catch-all-apply
        'vlax-invoke-method
        (list
          wsh
          'Run
          (strcat "powershell -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File \"" path "\"")
          0
          :vlax-true
        )
      )
      (vl-file-delete path)
    )
  )
  (if (not (vl-catch-all-error-p wsh))
    (vlax-release-object wsh)
  )
)

;; 엑셀에서 이미 지정(2칸 이상 선택)해 둔 범위를 우선 쓰고, 없으면 지정 창을 띄운다
;; (엑셀은 항상 한 칸이 선택돼 있으므로 한 칸은 "지정 안 됨"으로 본다)
(defun xli:pick (app / sel)
  (setq sel (xli:prop app 'Selection nil))
  (if (and (= (type sel) 'VLA-OBJECT) (> (xli:prop sel 'Count 0) 1))
    sel
    (xli:ask-range app)
  )
)

;; 엑셀을 앞으로 가져와 사용자가 셀 범위를 지정하게 한다 (InputBox Type 8)
;; 취소하면 nil. InputBox 호출이 실패하면 미리 선택해 둔 Selection 을 쓴다
(defun xli:ask-range (app / addr rng)
  ;; 생략 인자를 LISP 에서 못 넘기므로 모든 인자를 값으로 채운다
  ;; (기본값=현재 선택 주소, 위치=엑셀 창 근처, 도움말 없음, Type 8=범위)
  (setq addr (xli:prop (xli:prop app 'Selection nil) 'Address ""))
  (setq rng
    (vl-catch-all-apply
      'vlax-invoke-method
      (list
        app 'InputBox "셀 범위를 선택하세요." "영역 선택" addr
        (+ (xli:prop app 'Left 0.0) 100.0) (+ (xli:prop app 'Top 0.0) 100.0)
        "" 0 8
      )
    )
  )
  (setq rng (xli:val rng))  ; variant 로 감싸져 올 수 있다
  (cond
    ((vl-catch-all-error-p rng)
     (princ
       (strcat
         "\n【XLI】 영역 선택 창을 열지 못해 현재 선택된 범위를 사용합니다. ("
         (vl-catch-all-error-message rng) ")"
       )
     )
     (xli:prop app 'Selection nil)
    )
    ((= (type rng) 'VLA-OBJECT) rng)
    ;; 주소 문자열로 돌아오면 활성 시트의 범위로 바꾼다
    ((= (type rng) 'STR) (xli:range app rng))
    ((= rng :vlax-false) nil)
    (t
     (princ (strcat "\n【XLI】 영역 선택 결과를 알 수 없습니다: " (vl-princ-to-string (type rng))))
     nil
    )
  )
)

;; 주소 문자열 -> 범위 객체 (실패하면 nil)
(defun xli:range (app addr / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method (list app 'Range addr)))
  (if (vl-catch-all-error-p r)
    nil
    r
  )
)

;; 범위 -> 행 목록 (각 행은 셀 값 리스트)
(defun xli:rows (rng / v val rows)
  (setq v (vl-catch-all-apply 'vlax-get-property (list rng 'Value2)))
  (cond
    ((or (null v) (vl-catch-all-error-p v)) nil)
    (t
     (setq val (xli:val v))
     ;; 셀 하나만 선택하면 배열이 아니라 단일 값이 온다
     (if (= (type val) 'SAFEARRAY)
       (setq rows (vlax-safearray->list val))
       (setq rows (list (list val)))
     )
     ;; 한 줄만 선택하면 1차원 배열이라 한 행으로 감싼다
     (if (and rows (not (listp (car rows))))
       (list rows)
       rows
     )
    )
  )
)

;; 엑셀 창을 최소화해 CAD 화면이 보이게 한다 (xlMinimized = -4140)
(defun xli:minimize (app)
  (vl-catch-all-apply 'vlax-put-property (list app 'WindowState -4140))
)

;; 행 -> (문자 x y z 문자2). 좌표가 숫자가 아니면 nil
(defun xli:parse (row fmt swap / lbl lbl2 x y z)
  (cond
    ((= fmt 1)
     (setq x (nth 0 row))
     (setq y (nth 1 row))
    )
    ((= fmt 2)
     (setq lbl (nth 0 row))
     (setq x (nth 1 row))
     (setq y (nth 2 row))
    )
    ((= fmt 3)
     (setq x (nth 0 row))
     (setq y (nth 1 row))
     (setq z (nth 2 row))
    )
    ((= fmt 4)
     (setq lbl (nth 0 row))
     (setq x (nth 1 row))
     (setq y (nth 2 row))
     (setq z (nth 3 row))
    )
    ((= fmt 5)
     (setq lbl (nth 0 row))
     (setq x (nth 1 row))
     (setq y (nth 2 row))
     (setq z (nth 3 row))
     (setq lbl2 (nth 4 row))
    )
  )
  (setq x (xli:num x))
  (setq y (xli:num y))
  (setq z (xli:num z))
  (if (and x y)
    (list
      (if lbl (xli:str lbl) "")
      (if swap y x)
      (if swap x y)
      (if z z 0.0)  ; AutoLISP 의 or 는 값이 아니라 T 를 돌려준다
      (if lbl2 (xli:str lbl2) "")
    )
  )
)

;;------------------------------
;;  객체 만들기
;;------------------------------
;; UCS 좌표 -> WCS
(defun xli:wcs (p)
  (trans p 1 0)
)

;; 레이어가 없으면 만든다
(defun xli:layer (name)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        '(70 . 0)
        '(62 . 7)
        '(6 . "Continuous")
      )
    )
  )
)

(defun xli:mk-point (p lay)
  (xli:layer lay)
  (entmake (list '(0 . "POINT") (cons 8 lay) (cons 10 (xli:wcs p))))
)

(defun xli:mk-text (p str h lay / ang)
  (if (/= str "")
    (progn
      (xli:layer lay)
      ;; 현재 UCS 의 X축 방향으로 문자를 눕힌다
      (setq ang (angle (trans '(0.0 0.0 0.0) 1 0) (trans '(1.0 0.0 0.0) 1 0)))
      (entmake
        (list
          '(0 . "TEXT")
          (cons 8 lay)
          (cons 10 (xli:wcs p))
          (cons 40 h)
          (cons 1 str)
          (cons 50 ang)
        )
      )
    )
  )
)

(defun xli:mk-pline3d (pts lay)
  (entmake (list '(0 . "POLYLINE") (cons 8 lay) '(66 . 1) '(70 . 8) '(10 0.0 0.0 0.0)))
  (foreach p pts
    (entmake (list '(0 . "VERTEX") (cons 8 lay) (cons 10 (xli:wcs p)) '(70 . 32)))
  )
  (entmake (list '(0 . "SEQEND") (cons 8 lay)))
)

;; 2D: WCS 의 XY 로 투영하고 고도는 첫 점의 Z
(defun xli:mk-pline2d (pts lay)
  (entmake
    (append
      (list
        '(0 . "LWPOLYLINE")
        '(100 . "AcDbEntity")
        (cons 8 lay)
        '(100 . "AcDbPolyline")
        (cons 90 (length pts))
        '(70 . 0)
        (cons 38 (caddr (xli:wcs (car pts))))
      )
      (mapcar '(lambda (p) (list 10 (car (xli:wcs p)) (cadr (xli:wcs p)))) pts)
    )
  )
)

(defun xli:mk-pline (pts lay is-3d)
  (cond
    ((< (length pts) 2) nil)
    (is-3d
     (xli:layer lay)
     (xli:mk-pline3d pts lay)
    )
    (t
     (xli:layer lay)
     (xli:mk-pline2d pts lay)
    )
  )
)

;; 한 행의 점/문자를 만들고 UCS 점을 돌려준다
(defun xli:emit (r / p)
  (setq p (list (nth 1 r) (nth 2 r) (nth 3 r)))
  (if xli:point
    (xli:mk-point p xli:lay-pt)
  )
  (if (and xli:text (member xli:fmt '(2 4 5)))
    (xli:mk-text
      (list (+ (car p) xli:dx) (+ (cadr p) xli:dy) (caddr p))
      (nth 0 r) xli:th xli:lay-tx
    )
  )
  (if (and xli:text (= xli:fmt 5))
    (xli:mk-text
      (list (+ (car p) xli:dx) (- (+ (cadr p) xli:dy) (* xli:th 1.4)) (caddr p))
      (nth 4 r) xli:th xli:lay-tx
    )
  )
  (if (and xli:ztext (member xli:fmt '(3 4 5)))
    (xli:mk-text
      (list (+ (car p) xli:zdx) (+ (cadr p) xli:zdy) (caddr p))
      (rtos (caddr p) 2 xli:prec) xli:zth xli:lay-z
    )
  )
  p
)

;; 모든 행을 만들고 가져온 개수를 돌려준다
(defun xli:import (rows / pts n r)
  (setq n 0)
  (if xli:point
    (progn
      (setvar "PDMODE" xli:pdmode)
      (setvar "PDSIZE" (if xli:prel (- xli:psize) xli:psize))
    )
  )
  (foreach row rows
    (setq r (xli:parse row xli:fmt xli:swap))
    (if r
      (progn
        (setq pts (cons (xli:emit r) pts))
        (setq n (1+ n))
      )
    )
  )
  (if xli:pline
    (xli:mk-pline (reverse pts) xli:lay-ln xli:3d)
  )
  n
)

;;------------------------------
;;  대화상자
;;------------------------------
;; 임시 DCL 로 쓸 텍스트 (파일을 따로 두지 않는다)
;; 왼쪽=가져오기, 가운데=점/선, 오른쪽=문자/Z문자. 열마다 높이를 고정해야 행 높이가 맞는다
(defun xli:dcl-text ()
  (list
    "xli_main : dialog {"
    "  label = \"좌표 가져오기\";"
    "  : row {"
    "    : column {"
    "      alignment = top;"
    "      fixed_height = true;"
    "      : boxed_column {"
    "        label = \"좌표 가져오기 설정\";"
    "        : radio_column {"
    "          : radio_button { key = \"fmt1\"; label = \"2열 - X, Y\"; }"
    "          : radio_button { key = \"fmt2\"; label = \"3열 - 문자, X, Y\"; }"
    "          : radio_button { key = \"fmt3\"; label = \"3열 - X, Y, Z\"; }"
    "          : radio_button { key = \"fmt4\"; label = \"4열 - 문자, X, Y, Z\"; }"
    "          : radio_button { key = \"fmt5\"; label = \"5열 - 문자, X, Y, Z, 문자2\"; }"
    "        }"
    "        : toggle { key = \"swap\"; label = \"X, Y 좌표를 바꿔서 가져오기\"; }"
    "      }"
    "      : boxed_column {"
    "        label = \"Excel 가져오기 설정\";"
    "        : toggle { key = \"mini\"; label = \"엑셀 셀 지정 후 엑셀 최소화\"; }"
    "      }"
    "    }"
    "    : column {"
    "      alignment = top;"
    "      fixed_height = true;"
    "      : boxed_column {"
    "        label = \"점\";"
    "        : toggle { key = \"point\"; label = \"점 만들기\"; }"
    "        : edit_box { key = \"lay_pt\"; label = \"도면층\"; edit_width = 16; }"
    "        : edit_box { key = \"psize\"; label = \"크기\"; edit_width = 8; }"
    "        : radio_row {"
    "          : radio_button { key = \"pabs\"; label = \"절대 단위\"; }"
    "          : radio_button { key = \"prel\"; label = \"화면 상대(%)\"; }"
    "        }"
    "      }"
    "      : boxed_column {"
    "        label = \"선\";"
    "        : toggle { key = \"pline\"; label = \"선 만들기\"; }"
    "        : radio_row {"
    "          : radio_button { key = \"ln2d\"; label = \"폴리선\"; }"
    "          : radio_button { key = \"ln3d\"; label = \"3D 폴리선\"; }"
    "        }"
    "        : edit_box { key = \"lay_ln\"; label = \"도면층\"; edit_width = 16; }"
    "      }"
    "    }"
    "    : column {"
    "      alignment = top;"
    "      fixed_height = true;"
    "      : boxed_column {"
    "        label = \"문자\";"
    "        : toggle { key = \"text\"; label = \"문자(번호) 쓰기\"; }"
    "        : edit_box { key = \"lay_tx\"; label = \"도면층\"; edit_width = 16; }"
    "        : row {"
    "          : column {"
    "            : edit_box { key = \"dx\"; label = \"X 오프셋\"; edit_width = 6; }"
    "            : edit_box { key = \"th\"; label = \"높이\"; edit_width = 6; }"
    "          }"
    "          : column {"
    "            alignment = top;"
    "            : edit_box { key = \"dy\"; label = \"Y 오프셋\"; edit_width = 6; }"
    "          }"
    "        }"
    "      }"
    "      : boxed_column {"
    "        label = \"Z 문자\";"
    "        : toggle { key = \"ztext\"; label = \"Z값 문자 쓰기\"; }"
    "        : edit_box { key = \"lay_z\"; label = \"도면층\"; edit_width = 16; }"
    "        : row {"
    "          : column {"
    "            : edit_box { key = \"zdx\"; label = \"X 오프셋\"; edit_width = 6; }"
    "            : edit_box { key = \"zth\"; label = \"높이\"; edit_width = 6; }"
    "          }"
    "          : column {"
    "            : edit_box { key = \"zdy\"; label = \"Y 오프셋\"; edit_width = 6; }"
    "            : edit_box { key = \"prec\"; label = \"소수점\"; edit_width = 6; }"
    "          }"
    "        }"
    "      }"
    "    }"
    "  }"
    "  : text { label = \"엑셀에서 선택해 둔 범위를 쓰고, 없으면 지정합니다.\"; }"
    "  : row {"
    "    alignment = centered;"
    "    fixed_width = true;"
    "    : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 14; }"
    "    : button { key = \"cancel\"; label = \"종료\"; is_cancel = true; width = 14; }"
    "  }"
    "}"
  )
)

(defun xli:tile-real (key cur / v)
  (setq v (distof (get_tile key) 2))
  (if v v cur)
)

(defun xli:tile-str (key cur / s)
  (setq s (vl-string-trim " " (get_tile key)))
  (if (= s "") cur s)
)

;; 0 이하이면 이전 값을 유지한다 (높이용)
(defun xli:tile-pos (key cur / v)
  (setq v (xli:tile-real key cur))
  (if (> v 0) v cur)
)

(defun xli:read-main (/ i)
  (setq i 1)
  (while (<= i 5)
    (if (= (get_tile (strcat "fmt" (itoa i))) "1")
      (setq xli:fmt i)
    )
    (setq i (1+ i))
  )
  (setq xli:swap (= (get_tile "swap") "1"))
  (setq xli:mini (= (get_tile "mini") "1"))
)

(defun xli:read-obj ()
  (setq xli:point (= (get_tile "point") "1"))
  (setq xli:pline (= (get_tile "pline") "1"))
  (setq xli:3d (= (get_tile "ln3d") "1"))
  (setq xli:text (= (get_tile "text") "1"))
  (setq xli:ztext (= (get_tile "ztext") "1"))
  (setq xli:psize (xli:tile-pos "psize" xli:psize))
  (setq xli:prel (= (get_tile "prel") "1"))
  (setq xli:th (xli:tile-pos "th" xli:th))
  (setq xli:zth (xli:tile-pos "zth" xli:zth))
  (setq xli:dx (xli:tile-real "dx" xli:dx))
  (setq xli:dy (xli:tile-real "dy" xli:dy))
  (setq xli:zdx (xli:tile-real "zdx" xli:zdx))
  (setq xli:zdy (xli:tile-real "zdy" xli:zdy))
  (setq xli:prec (max 0 (min 8 (fix (xli:tile-real "prec" xli:prec)))))
  (setq xli:lay-pt (xli:tile-str "lay_pt" xli:lay-pt))
  (setq xli:lay-ln (xli:tile-str "lay_ln" xli:lay-ln))
  (setq xli:lay-tx (xli:tile-str "lay_tx" xli:lay-tx))
  (setq xli:lay-z (xli:tile-str "lay_z" xli:lay-z))
)

(defun xli:enable (keys on)
  (foreach key keys
    (mode_tile key (if on 0 1))
  )
)

;; 객체 체크를 끄면 그 객체의 하위 항목을 비활성으로 만든다
(defun xli:sync ()
  (xli:enable '("lay_pt" "psize" "pabs" "prel") (= (get_tile "point") "1"))
  (xli:enable '("ln2d" "ln3d" "lay_ln") (= (get_tile "pline") "1"))
  (xli:enable '("lay_tx" "th" "dx" "dy") (= (get_tile "text") "1"))
  (xli:enable '("lay_z" "zth" "prec" "zdx" "zdy") (= (get_tile "ztext") "1"))
)

;; 현재 설정을 대화상자에 채운다
(defun xli:set-tiles ()
  (set_tile (strcat "fmt" (itoa xli:fmt)) "1")
  (set_tile "swap" (if xli:swap "1" "0"))
  (set_tile "mini" (if xli:mini "1" "0"))
  (set_tile "point" (if xli:point "1" "0"))
  (set_tile "pline" (if xli:pline "1" "0"))
  (set_tile (if xli:3d "ln3d" "ln2d") "1")
  (set_tile "text" (if xli:text "1" "0"))
  (set_tile "ztext" (if xli:ztext "1" "0"))
  (set_tile "psize" (rtos xli:psize 2 4))
  (set_tile (if xli:prel "prel" "pabs") "1")
  (set_tile "th" (rtos xli:th 2 4))
  (set_tile "zth" (rtos xli:zth 2 4))
  (set_tile "dx" (rtos xli:dx 2 4))
  (set_tile "dy" (rtos xli:dy 2 4))
  (set_tile "zdx" (rtos xli:zdx 2 4))
  (set_tile "zdy" (rtos xli:zdy 2 4))
  (set_tile "prec" (itoa xli:prec))
  (set_tile "lay_pt" xli:lay-pt)
  (set_tile "lay_ln" xli:lay-ln)
  (set_tile "lay_tx" xli:lay-tx)
  (set_tile "lay_z" xli:lay-z)
  (xli:sync)
)

;; 확인이면 T, 종료면 nil
(defun xli:dialog (/ dcl-path dcl-id file code)
  (setq dcl-path (vl-filename-mktemp "xli.dcl"))
  (setq file (open dcl-path "w"))
  (foreach line (xli:dcl-text)
    (write-line line file)
  )
  (close file)
  (setq dcl-id (load_dialog dcl-path))
  (setq code 0)
  (if (new_dialog "xli_main" dcl-id)
    (progn
      (xli:set-tiles)
      (action_tile "point" "(xli:sync)")
      (action_tile "pline" "(xli:sync)")
      (action_tile "text" "(xli:sync)")
      (action_tile "ztext" "(xli:sync)")
      (action_tile "accept" "(xli:read-main)(xli:read-obj)(done_dialog 1)")
      (setq code (start_dialog))
    )
  )
  (unload_dialog dcl-id)
  (vl-file-delete dcl-path)
  (= code 1)
)

;;------------------------------
;;  명령
;;------------------------------
(defun c:XLI (/ *error* doc app rng rows count start marked ok)
  (defun *error* (msg)
    (if marked
      (vl-catch-all-apply 'vla-endundomark (list doc))
    )
    (if (not (wcmatch (strcase (vl-princ-to-string msg) t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【XLI】 오류: " (vl-princ-to-string msg)))
    )
    (princ)
  )
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq ok (xli:dialog))
  (if ok
    (setq app (xli:app))
  )
  (if app
    (xli:focus-excel app)
  )
  (if app
    (setq rng (xli:pick app))
  )
  (if rng
    (setq rows (xli:rows rng))
  )
  (if (and rng xli:mini)
    (xli:minimize app)
  )
  (setq start (getvar "MILLISECS"))
  (cond
    ((not ok) nil)
    ((null app) (princ "\n【XLI】 오류: Excel 을 찾을 수 없습니다."))
    ((null rng) (princ "\n【XLI】 셀 범위를 지정하지 않아 취소했습니다."))
    ((null rows) (princ "\n【XLI】 오류: 지정한 범위를 읽을 수 없습니다."))
    (t
     (vla-startundomark doc)
     (setq marked t)
     (setq count (xli:import rows))
     (vla-endundomark doc)
     (setq marked nil)
     (princ
       (strcat
         "\n【XLI】 " (itoa count) "개 좌표를 가져왔습니다. Time: "
         (itoa (- (getvar "MILLISECS") start)) " ms"
       )
     )
    )
  )
  (princ)
)

(princ)
