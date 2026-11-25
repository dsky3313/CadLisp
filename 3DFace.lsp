;;; ===========================================================================
;;; 3Dface.LSP : 3DPOINT -> 3DFACE -> 검사 -> 3DSOLID
;;;
;;; 명령어
;;;   3DF : 번호 선택 (1.면생성 / 2.면검사 / 3.솔리드화), 번호 클릭 또는 입력, Enter = 1(면 생성), 0 = 종료
;;;
;;; 옵션
;;;   면생성   : 선택한 3DPOINT 3개(삼각형) 또는 4개(삼각형 2개)로 3DFACE 생성
;;;   면검사   : 3DFACE 껍데기가 닫혀 있는지 검사하고 문제 변을 3DFaceCheck 도면층에 표시
;;;   솔리드화 : 윗면 3DFACE 선택 -> 밑면/옆면을 닫은 3DSOLID 생성 (SAT 작성 + ACISIN)
;;;
;;; 가정
;;;   - 옵션은 번호(1, 2, 3)를 클릭하거나 입력해서 선택

;;; 참고 : 명령행의 클릭 가능한 항목
;;;   - AutoCAD 용어 : 옵션(Option) 또는 키워드(Keyword). [자리맞추기(J)/스타일(S)] 의 (J)(S) 는 입력용 단축 글자
;;;   - 리습 구현 : initget 으로 키워드 등록 -> getkword 로 입력 (NumberCheck.lsp 와 같은 방식)
;;;   - 한글 키워드는 "옵션 키워드가 유효하지 않다" 오류 -> 숫자/영문만 등록, 한글 이름은 프롬프트 문자열에만 표시
;;; ===========================================================================

;;----------------------------------------------------------------------;;
;;  면생성
;;----------------------------------------------------------------------;;

(defun F3D:tri ( a b c )
  (entmakex
    (list
     '(0 . "3DFACE")
     '(100 . "AcDbEntity")
     '(100 . "AcDbFace")
      (cons 10 a)
      (cons 11 b)
      (cons 12 c)
      (cons 13 c)
    )
  )
)

(defun p2s:face ( / ss i pts n dA dB dC cnt )

  (setq cnt 0)

  (while (setq ss (progn (princ "\n【3DFace】 ") (ssget '((0 . "POINT")))))
    (setq pts nil)
    (setq i 0)
    (while (< i (sslength ss))
      (setq pts (cons (cdr (assoc 10 (entget (ssname ss i)))) pts))
      (setq i (1+ i))
    )
    (setq n (length pts))
    (cond
      ((= n 3)
        (F3D:tri (nth 0 pts) (nth 1 pts) (nth 2 pts))
        (setq cnt (1+ cnt))
      )
      ((= n 4)
        (setq dA (+ (distance (nth 0 pts) (nth 1 pts)) (distance (nth 2 pts) (nth 3 pts))))
        (setq dB (+ (distance (nth 0 pts) (nth 2 pts)) (distance (nth 1 pts) (nth 3 pts))))
        (setq dC (+ (distance (nth 0 pts) (nth 3 pts)) (distance (nth 1 pts) (nth 2 pts))))
        (cond
          ((and (>= dB dA) (>= dB dC))
            (setq pts (list (nth 0 pts) (nth 1 pts) (nth 2 pts) (nth 3 pts)))
          )
          ((>= dA dC)
            (setq pts (list (nth 0 pts) (nth 2 pts) (nth 1 pts) (nth 3 pts)))
          )
          (T
            (setq pts (list (nth 0 pts) (nth 1 pts) (nth 3 pts) (nth 2 pts)))
          )
        )
        (F3D:tri (nth 0 pts) (nth 1 pts) (nth 2 pts))
        (F3D:tri (nth 0 pts) (nth 2 pts) (nth 3 pts))
        (setq cnt (+ cnt 2))
      )
      (T
        (princ (strcat "\n【3DFace】 점 " (itoa n) "개 선택됨 : 3개 또는 4개만 가능합니다."))
      )
    )
  )

  (princ (strcat "\n【3DFace】 3DFACE " (itoa cnt) "개 생성"))
  (princ)
)

;;----------------------------------------------------------------------;;
;;  면검사
;;----------------------------------------------------------------------;;

(defun ck:key ( p )
  (strcat
    (rtos (car p) 2 6) ","
    (rtos (cadr p) 2 6) ","
    (rtos (caddr p) 2 6)
  )
)

(defun ck:edge ( p q / k1 k2 k r )
  (setq k1 (ck:key p))
  (setq k2 (ck:key q))
  (if (/= k1 k2)
    (progn
      (setq k (if (< k1 k2) (strcat k1 "|" k2) (strcat k2 "|" k1)))
      (setq r (assoc k ck:tab))
      (if r
        (setq ck:tab (subst (list k (1+ (nth 1 r)) (nth 2 r) (nth 3 r)) r ck:tab))
        (setq ck:tab (cons (list k 1 p q) ck:tab))
      )
    )
  )
)

(defun ck:line ( p q col )
  (entmakex
    (list
     '(0 . "LINE")
     '(100 . "AcDbEntity")
     '(8 . "3DFaceCheck")
      (cons 62 col)
     '(370 . 60)
     '(100 . "AcDbLine")
      (cons 10 p)
      (cons 11 q)
    )
  )
)

(defun p2s:check ( / ss i en p1 p2 p3 p4 e open over )

  (setq ck:tab nil)
  (setq ss (progn (princ "\n【3DFace】 ") (ssget '((0 . "3DFACE")))))

  (if ss
    (progn
      (if (not (tblsearch "LAYER" "3DFaceCheck"))
        (entmake
          (list
           '(0 . "LAYER")
           '(100 . "AcDbSymbolTableRecord")
           '(100 . "AcDbLayerTableRecord")
           '(2 . "3DFaceCheck")
           '(70 . 0)
           '(62 . 1)
           '(6 . "Continuous")
          )
        )
      )

      (setq i 0)
      (while (< i (sslength ss))
        (setq en (entget (ssname ss i)))
        (setq p1 (cdr (assoc 10 en)))
        (setq p2 (cdr (assoc 11 en)))
        (setq p3 (cdr (assoc 12 en)))
        (setq p4 (cdr (assoc 13 en)))
        (ck:edge p1 p2)
        (ck:edge p2 p3)
        (if (equal p3 p4 1e-9)
          (ck:edge p3 p1)
          (progn
            (ck:edge p3 p4)
            (ck:edge p4 p1)
          )
        )
        (setq i (1+ i))
      )

      (setq open 0)
      (setq over 0)
      (foreach e ck:tab
        (cond
          ((= 1 (nth 1 e))
            (ck:line (nth 2 e) (nth 3 e) 1)
            (setq open (1+ open))
            (if (<= open 10)
              (princ (strcat "\n【3DFace】 구멍 변 : " (ck:key (nth 2 e)) "  ~  " (ck:key (nth 3 e))))
            )
          )
          ((> (nth 1 e) 2)
            (ck:line (nth 2 e) (nth 3 e) 2)
            (setq over (1+ over))
          )
        )
      )

      (princ
        (strcat
          "\n【3DFace】 검사 완료 : 면 " (itoa (sslength ss)) "개 / 구멍 변(빨강) " (itoa open)
          "개 / 겹침 변(노랑) " (itoa over) "개"
        )
      )
      (if (and (= open 0) (= over 0))
        (princ "\n【3DFace】 모든 변이 정확히 2개 면에 공유됩니다. (닫힘)")
        (princ "\n【3DFace】 3DFaceCheck 도면층에 표시했습니다. 다른 도면층을 끄고 확인하세요.")
      )
    )
    (princ "\n【3DFace】 선택된 3DFACE 가 없습니다.")
  )

  (setq ck:tab nil)
  (princ)
)

;;----------------------------------------------------------------------;;
;;  솔리드화
;;----------------------------------------------------------------------;;

;; 실수 -> 문자열 (아주 작은 값은 0)
(defun f2s:rt ( x )
  (rtos (if (< (abs x) 1e-12) 0.0 x) 2 12)
)

;; 포인터 문자열
(defun f2s:p ( n )
  (strcat "$" (itoa n))
)

;; 단위 벡터
(defun f2s:unit ( v / l )
  (setq l (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)) (* (caddr v) (caddr v)))))
  (mapcar (function (lambda ( a ) (/ a l))) v)
)

;; 다각형 법선 (Newell)
(defun f2s:normal ( pts / a b nx ny nz i m )
  (setq nx 0.0)
  (setq ny 0.0)
  (setq nz 0.0)
  (setq m (length pts))
  (setq i 0)
  (repeat m
    (setq a (nth i pts))
    (setq b (nth (rem (1+ i) m) pts))
    (setq nx (+ nx (* (- (cadr a) (cadr b)) (+ (caddr a) (caddr b)))))
    (setq ny (+ ny (* (- (caddr a) (caddr b)) (+ (car a) (car b)))))
    (setq nz (+ nz (* (- (car a) (car b)) (+ (cadr a) (cadr b)))))
    (setq i (1+ i))
  )
  (f2s:unit (list nx ny nz))
)

;; 닫힌 다면체 -> SAT 문자열 리스트
;;   W     : 중심 이동된 꼭짓점 좌표 리스트
;;   faces : 면 리스트 (꼭짓점 번호 리스트, 바깥에서 볼 때 반시계)
;;   cen   : 이동한 중심 좌표
(defun f2s:sat ( W faces cen / nv nf nc ne ce cm em fi f m i u v key ci ei e
                 fstart st B0 TR LU SH F0 L0 P0 C0 E0 S0 X0 Q0 out
                 pts nrm ud nxt prv part sense ve d dx a b c p )

  (setq nv (length W))
  (setq nf (length faces))

  ;; 반변(coedge) 목록 : (면번호 시작점 끝점)
  (setq ce nil)
  (setq fstart nil)
  (setq st 0)
  (setq fi 0)
  (foreach f faces
    (setq m (length f))
    (setq fstart (cons st fstart))
    (setq i 0)
    (repeat m
      (setq ce (cons (list fi (nth i f) (nth (rem (1+ i) m) f)) ce))
      (setq i (1+ i))
    )
    (setq st (+ st m))
    (setq fi (1+ fi))
  )
  (setq ce (reverse ce))
  (setq fstart (reverse fstart))
  (setq nc (length ce))

  ;; (시작,끝) -> 반변 번호,  변 목록
  (setq cm nil)
  (setq em nil)
  (setq ne 0)
  (setq ci 0)
  (foreach c ce
    (setq u (cadr c))
    (setq v (caddr c))
    (setq cm (cons (cons (strcat (itoa u) "," (itoa v)) ci) cm))
    (setq key (strcat (itoa (min u v)) "," (itoa (max u v))))
    (if (null (assoc key em))
      (progn
        (setq em (cons (list key ne (min u v) (max u v) ci) em))
        (setq ne (1+ ne))
      )
    )
    (setq ci (1+ ci))
  )
  (setq em (reverse em))

  ;; 번호 기준점
  (setq B0 0)
  (setq TR 1)
  (setq LU 2)
  (setq SH 3)
  (setq F0 4)
  (setq L0 (+ F0 nf))
  (setq P0 (+ L0 nf))
  (setq C0 (+ P0 nf))
  (setq E0 (+ C0 nc))
  (setq S0 (+ E0 ne))
  (setq X0 (+ S0 ne))
  (setq Q0 (+ X0 nv))

  (setq out nil)
  (setq out (cons "700 0 1 0 " out))
  (setq out (cons "@4 dodo @12 ACIS 32.0 NT @24 Wed Nov  4 09:12:22 2026 " out))
  (setq out (cons "1 9.9999999999999995e-007 1e-010 " out))
  (setq out (cons (strcat "body $-1 -1 $-1 " (f2s:p LU) " $-1 " (f2s:p TR) " #") out))
  (setq out
    (cons
      (strcat "transform $-1 -1 1 0 0 0 1 0 0 0 1 "
        (f2s:rt (car cen)) " " (f2s:rt (cadr cen)) " " (f2s:rt (caddr cen))
        " 1 no_rotate no_reflect no_shear #")
      out
    )
  )
  (setq out (cons (strcat "lump $-1 -1 $-1 $-1 " (f2s:p SH) " " (f2s:p B0) " #") out))
  (setq out (cons (strcat "shell $-1 -1 $-1 $-1 $-1 " (f2s:p F0) " $-1 " (f2s:p LU) " #") out))

  ;; 면
  (setq fi 0)
  (repeat nf
    (setq nxt (if (< fi (1- nf)) (f2s:p (+ F0 fi 1)) "$-1"))
    (setq out
      (cons
        (strcat "face $-1 -1 $-1 " nxt " " (f2s:p (+ L0 fi)) " " (f2s:p SH)
          " $-1 " (f2s:p (+ P0 fi)) " forward single #")
        out
      )
    )
    (setq fi (1+ fi))
  )

  ;; 루프
  (setq fi 0)
  (repeat nf
    (setq out
      (cons
        (strcat "loop $-1 -1 $-1 $-1 " (f2s:p (+ C0 (nth fi fstart))) " " (f2s:p (+ F0 fi)) " #")
        out
      )
    )
    (setq fi (1+ fi))
  )

  ;; 평면
  (foreach f faces
    (setq pts (mapcar (function (lambda ( k ) (nth k W))) f))
    (setq nrm (f2s:normal pts))
    (setq ud (f2s:unit (mapcar '- (cadr pts) (car pts))))
    (setq out
      (cons
        (strcat "plane-surface $-1 -1 $-1 "
          (f2s:rt (car (car pts))) " " (f2s:rt (cadr (car pts))) " " (f2s:rt (caddr (car pts))) " "
          (f2s:rt (car nrm)) " " (f2s:rt (cadr nrm)) " " (f2s:rt (caddr nrm)) " "
          (f2s:rt (car ud)) " " (f2s:rt (cadr ud)) " " (f2s:rt (caddr ud))
          " forward_v I I I I #")
        out
      )
    )
  )

  ;; 반변
  (setq ci 0)
  (setq fi 0)
  (foreach f faces
    (setq m (length f))
    (setq i 0)
    (repeat m
      (setq u (cadr (nth (+ (nth fi fstart) i) ce)))
      (setq v (caddr (nth (+ (nth fi fstart) i) ce)))
      (setq nxt (+ (nth fi fstart) (rem (1+ i) m)))
      (setq prv (+ (nth fi fstart) (rem (+ i m -1) m)))
      (setq part (cdr (assoc (strcat (itoa v) "," (itoa u)) cm)))
      (setq sense (if (< u v) "forward" "reversed"))
      (setq e (cadr (assoc (strcat (itoa (min u v)) "," (itoa (max u v))) em)))
      (setq out
        (cons
          (strcat "coedge $-1 -1 $-1 " (f2s:p (+ C0 nxt)) " " (f2s:p (+ C0 prv)) " "
            (f2s:p (+ C0 part)) " " (f2s:p (+ E0 e)) " " sense " " (f2s:p (+ L0 fi)) " $-1 #")
          out
        )
      )
      (setq i (1+ i))
    )
    (setq fi (1+ fi))
  )

  ;; 변
  (foreach e em
    (setq a (nth (nth 2 e) W))
    (setq b (nth (nth 3 e) W))
    (setq d (distance a b))
    (setq out
      (cons
        (strcat "edge $-1 -1 $-1 " (f2s:p (+ X0 (nth 2 e))) " 0 " (f2s:p (+ X0 (nth 3 e))) " "
          (f2s:rt d) " " (f2s:p (+ C0 (nth 4 e))) " " (f2s:p (+ S0 (nth 1 e))) " forward @7 unknown #")
        out
      )
    )
  )

  ;; 직선 곡선
  (foreach e em
    (setq a (nth (nth 2 e) W))
    (setq b (nth (nth 3 e) W))
    (setq d (distance a b))
    (setq dx (mapcar (function (lambda ( p q ) (/ (- q p) d))) a b))
    (setq out
      (cons
        (strcat "straight-curve $-1 -1 $-1 "
          (f2s:rt (car a)) " " (f2s:rt (cadr a)) " " (f2s:rt (caddr a)) " "
          (f2s:rt (car dx)) " " (f2s:rt (cadr dx)) " " (f2s:rt (caddr dx)) " I I #")
        out
      )
    )
  )

  ;; 꼭짓점 (각 꼭짓점이 속한 첫 번째 변)
  (setq i 0)
  (repeat nv
    (setq ve nil)
    (foreach e em
      (if (and (null ve) (or (= i (nth 2 e)) (= i (nth 3 e))))
        (setq ve (nth 1 e))
      )
    )
    (setq out
      (cons
        (strcat "vertex $-1 -1 $-1 " (f2s:p (+ E0 ve)) " " (f2s:p (+ Q0 i)) " #")
        out
      )
    )
    (setq i (1+ i))
  )

  ;; 점
  (foreach p W
    (setq out
      (cons
        (strcat "point $-1 -1 $-1 " (f2s:rt (car p)) " " (f2s:rt (cadr p)) " " (f2s:rt (caddr p)) " #")
        out
      )
    )
  )

  (setq out (cons "End-of-ACIS-data" out))
  (reverse out)
)

;;----------------------------------------------------------------------;;
;;  p2s:solid
;;----------------------------------------------------------------------;;

;; 꼭짓점 번호 (같은 위치면 기존 번호), 호출 함수의 vt vk vn 사용
(defun f2s:vi ( p / k r )
  (setq k (strcat (rtos (car p) 2 6) "," (rtos (cadr p) 2 6) "," (rtos (caddr p) 2 6)))
  (setq r (cdr (assoc k vk)))
  (if (null r)
    (progn
      (setq r vn)
      (setq vk (cons (cons k vn) vk))
      (setq vt (cons p vt))
      (setq vn (1+ vn))
    )
  )
  r
)

(defun p2s:solid ( / *error* ss i en p1 p2 p3 p4 tri tris vt vk vn th bz n a b c V2 des faces
                 cen W lines f path fd fdia t1 e s cmdecho-orig )

  (defun *error* ( m )
    (if fdia (setvar "FILEDIA" fdia))
    (if cmdecho-orig (setvar "CMDECHO" cmdecho-orig))
    (if (not (wcmatch (strcase m t) "*cancel*,*exit*"))
      (princ (strcat "\n【3DFace】 오류: " m))
    )
    (princ)
  )

  (setq ss (progn (princ "\n【3DFace】 ") (ssget '((0 . "3DFACE")))))

  (if ss
    (progn
      (setq vt nil)
      (setq vk nil)
      (setq vn 0)
      (setq tris nil)
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (entget (ssname ss i)))
        (setq p1 (cdr (assoc 10 en)))
        (setq p2 (cdr (assoc 11 en)))
        (setq p3 (cdr (assoc 12 en)))
        (setq p4 (cdr (assoc 13 en)))
        (setq tri (list (list p1 p2 p3)))
        (if (not (equal p3 p4 1e-9))
          (setq tri (cons (list p1 p3 p4) tri))
        )
        (foreach t1 tri
          (setq a (f2s:vi (car t1)))
          (setq b (f2s:vi (cadr t1)))
          (setq c (f2s:vi (caddr t1)))
          (if (and (/= a b) (/= b c) (/= a c))
            (progn
              ;; 위에서 볼 때 반시계가 되도록 정렬
              (if
                (< (- (* (- (car (cadr t1)) (car (car t1))) (- (cadr (caddr t1)) (cadr (car t1))))
                      (* (- (car (caddr t1)) (car (car t1))) (- (cadr (cadr t1)) (cadr (car t1)))))
                   0.0)
                (setq tris (cons (list a c b) tris))
                (setq tris (cons (list a b c) tris))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (setq vt (reverse vt))
      (setq n vn)

      (setq th (getreal "\n【3DFace】 최저점 아래 두께 <4.5>: "))
      (if (null th) (setq th 4.5))
      (setq bz (- (apply 'min (mapcar 'caddr vt)) th))

      ;; 꼭짓점 : 윗면 n개 + 밑면 n개
      (setq V2 (append vt (mapcar (function (lambda ( p ) (list (car p) (cadr p) bz))) vt)))

      ;; 면 : 윗면, 밑면, 옆면(테두리 변)
      (setq des nil)
      (foreach f tris
        (setq des (cons (strcat (itoa (car f)) "," (itoa (cadr f))) des))
        (setq des (cons (strcat (itoa (cadr f)) "," (itoa (caddr f))) des))
        (setq des (cons (strcat (itoa (caddr f)) "," (itoa (car f))) des))
      )
      (setq faces nil)
      (foreach f tris
        (setq faces (cons (list (car f) (cadr f) (caddr f)) faces))
        (setq faces (cons (list (+ (car f) n) (+ (caddr f) n) (+ (cadr f) n)) faces))
        (foreach e (list (list (car f) (cadr f)) (list (cadr f) (caddr f)) (list (caddr f) (car f)))
          (if (not (member (strcat (itoa (cadr e)) "," (itoa (car e))) des))
            (setq faces (cons (list (car e) (+ (car e) n) (+ (cadr e) n) (cadr e)) faces))
          )
        )
      )
      (setq faces (reverse faces))

      ;; 중심 이동
      (setq cen
        (list
          (/ (+ (apply 'min (mapcar 'car V2)) (apply 'max (mapcar 'car V2))) 2.0)
          (/ (+ (apply 'min (mapcar 'cadr V2)) (apply 'max (mapcar 'cadr V2))) 2.0)
          (/ (+ (apply 'min (mapcar 'caddr V2)) (apply 'max (mapcar 'caddr V2))) 2.0)
        )
      )
      (setq W (mapcar (function (lambda ( p ) (mapcar '- p cen))) V2))

      (setq lines (f2s:sat W faces cen))

      (setq path (strcat (getvar "TEMPPREFIX") "f2s_tmp.sat"))
      (setq fd (open path "w"))
      (foreach s lines (write-line s fd))
      (close fd)

      (setq fdia (getvar "FILEDIA"))
      (setvar "FILEDIA" 0)
      (setq cmdecho-orig (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.ACISIN" path)
      (setvar "CMDECHO" cmdecho-orig)
      (setq cmdecho-orig nil)
      (setvar "FILEDIA" fdia)
      (setq fdia nil)
      (vl-file-delete path)

      (princ
        (strcat
          "\n【3DFace】 3DSOLID 생성 시도 : 윗면 " (itoa (length tris)) "개, 꼭짓점 " (itoa (* 2 n))
          "개, 면 " (itoa (length faces)) "개 (밑면 표고 " (rtos bz 2 3) ")"
        )
      )
    )
    (princ "\n【3DFace】 선택된 3DFACE 가 없습니다.")
  )
  (princ)
)

;;----------------------------------------------------------------------;;
;;  c:3DF
;;----------------------------------------------------------------------;;

(defun c:3DF ( / opt )

  (setq opt "1")
  (while (/= opt "0")
    (initget "0 1 2 3")
    (setq opt (getkword "\n【3DFace】 [면 생성(1)/면 검사(2)/솔리드화(3)/종료(0)] <1>: "))
    (if (null opt) (setq opt "1"))
    (cond
      ((= opt "1") (p2s:face))
      ((= opt "2") (p2s:check))
      ((= opt "3") (p2s:solid))
    )
  )
  (princ)
)

(princ)