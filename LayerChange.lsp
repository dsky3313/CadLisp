;;; ==============================
;;; Layer_Change.LSP
;;;  - 명령 실행 시 자동 레이어 변경
;;;  - 추가: cd, ct 명령
;;;
;;; 원본파일: LayerDirectorV2-1.lsp (Lee Mac)
;;;
;;; 명령어: c`, c``, c1~c8, cd, ct
;;;
;;; 사용법
;;;  1) c`/c``/c1~c8 실행 후 객체 선택 → 선택 객체를 해당 레이어로 이동
;;;  2) cd/ct 실행 후 객체 선택 → 선택 객체를 !DIM / !TEXT 레이어로 이동
;;;  3) TEXT/DIM/LEADER 명령 자동 감지 후 레이어 변경
;;;
;;; 가정 / 주의
;;;  - 레이어: !0~!8, !TEXT, !DIM
;;;  - 패턴 매칭: [DM]TEXT,TEXT / DIM*,*LEADER / dd,dc
;;;
;;; 동작방식
;;;  - 명령 시작 시 패턴 매칭 + 레이어 생성/변경
;;;  - 레이어 생성은 entmake (command 아님)
;;;  - 명령 끝 후 이전 레이어 복구
;;; ==============================

(vl-load-com)

;;------------------------------
;;  설정
;;------------------------------

;; 명령 패턴 → 레이어 매핑 (명령 선종 색상)
(setq lc:cmdlayer
  '(
    ("【DM】TEXT,TEXT" "!TEXT" "continuous" 2)
    ("DIM*,*LEADER" "!DIM" "continuous" 7)
    ("dd,dc" "!DIM" "continuous" 7)
  )
)

;;------------------------------
;;  보조 함수
;;------------------------------

(defun lc:layer-exists (lay-name)
  (tblsearch "LAYER" lay-name)
)

(defun lc:create-layer (lay-name lt-name col-num / def ent)
  (if (not (lc:layer-exists lay-name))
    (progn
      (setq def
        (list
          (cons 0 "LAYER")
          (cons 100 "AcDbSymbolTableRecord")
          (cons 100 "AcDbLayerTableRecord")
          (cons 70 0)
          (cons 2 lay-name)
          (cons 62 (if (and (>= col-num 1) (< col-num 256)) col-num 7))
          (cons 6 (if (tblsearch "ltype" lt-name) lt-name "Continuous"))
          (cons 290 1)
        )
      )
      (entmake def)
    )
  )
)

(defun lc:move (lay-name lt-name col-num / ss)
  (lc:create-layer lay-name lt-name col-num)
  (setq ss (progn (princ "\n【LayerChange】 ") (ssget)))
  (if ss
    (command "chprop" ss "" "la" lay-name "c" "BYLAYER" "lt" "BYLAYER" "")
  )
  (princ)
)

(defun lc:find-layer (cmd / lst)
  (setq lst (cdar (vl-member-if
    (function (lambda (x) (wcmatch (strcase cmd) (strcase (car x)))))
    lc:cmdlayer
  )))
  lst
)

(defun lc:reactor-set (obj arg / lst)
  (if (and (setq arg (car arg))
           (setq lst (lc:find-layer (strcase arg)))
      )
    (progn
      (lc:create-layer (car lst) (cadr lst) (caddr lst))
      (setq lc:old-layer (cons (getvar 'clayer) lc:old-layer))
      (setvar 'clayer (car lst))
    )
  )
  (princ)
)

(defun lc:reactor-reset (obj arg / lay)
  (if (and (= 'list (type lc:old-layer))
           (setq lay (car lc:old-layer))
      )
    (progn
      (if (lc:layer-exists lay)
        (setvar 'clayer lay)
      )
      (setq lc:old-layer (cdr lc:old-layer))
    )
  )
  (princ)
)

;;------------------------------
;;  리액터 초기화
;;------------------------------

(setq lc:old-layer '())

(vlr-command-reactor "LC:CommandReactor"
  '(
    (:vlr-commandwillstart . lc:reactor-set)
    (:vlr-commandended     . lc:reactor-reset)
    (:vlr-commandcancelled . lc:reactor-reset)
    (:vlr-commandfailed    . lc:reactor-reset)
  )
)

;;------------------------------
;;  명령 (직접 호출용)
;;------------------------------

;; !0  흰색(7)   !1  빨강(1)   !2  노랑(2)   !3  녹(3)
;; !4  하늘(4)   !5  파랑(5)   !6  선홍(6)   !7  253   !8  30
(defun c:c`  () (lc:move "!0" "continuous" 7))
(defun c:c`` () (lc:move "!7" "continuous" 253))
(defun c:c1  () (lc:move "!1" "continuous" 1))
(defun c:c2  () (lc:move "!2" "continuous" 2))
(defun c:c3  () (lc:move "!3" "continuous" 3))
(defun c:c4  () (lc:move "!4" "continuous" 4))
(defun c:c5  () (lc:move "!5" "continuous" 5))
(defun c:c6  () (lc:move "!6" "continuous" 6))
(defun c:c7  () (lc:move "!7" "continuous" 253))
(defun c:c8  () (lc:move "!8" "continuous" 30))

;; 추가 명령
(defun c:cd () (lc:move "!DIM" "continuous" 7))
(defun c:ct () (lc:move "!TEXT" "continuous" 2))

(princ)