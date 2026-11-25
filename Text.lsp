;;; ==============================
;;; TM.LSP
;;;  - 두 점의 중간에 중간중심(MC) 정렬 문자 생성
;;;
;;; 원본파일:
;;;
;;; 명령어: TM
;;;
;;; 사용법
;;;  1) TM 입력
;;;  2) 첫 번째 점, 두 번째 점 지정
;;;  3) 문자 입력 (공백 포함 가능)
;;;
;;; 가정 / 주의
;;;  - 문자 높이는 TEXTSIZE, 스타일은 현재 TEXTSTYLE, 회전은 0
;;;  - 현재 UCS 기준으로 점을 받아 WCS로 변환해 생성
;;;
;;; 동작방식
;;;  - 두 점의 중점을 구해 entmake로 TEXT 생성 (72=1, 73=2 : 중간중심)
;;; ==============================

;;------------------------------
;;  보조 함수
;;------------------------------
(defun tm:midpoint (pt1 pt2)
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2)
)

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:TM (/ pt1 pt2 txt mid)
  (setq pt1 (getpoint "\n【TM】 첫 번째 점: "))
  (if pt1
    (setq pt2 (getpoint pt1 "\n【TM】 두 번째 점: "))
  )
  (if pt2
    (setq txt (getstring T "\n【TM】 문자 입력: "))
  )
  (if (and txt (/= txt ""))
    (progn
      (setq mid (trans (tm:midpoint pt1 pt2) 1 0))
      (entmake
        (list
          '(0 . "TEXT")
          (cons 10 mid)
          (cons 40 (getvar "TEXTSIZE"))
          (cons 1 txt)
          (cons 7 (getvar "TEXTSTYLE"))
          '(72 . 1)
          (cons 11 mid)
          '(73 . 2)
        )
      )
    )
  )
  (princ)
)

(princ)