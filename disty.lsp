;;; ==============================
;;; DISTY.LSP
;;;  - 두 점 사이의 Y 거리를 문자로 작성
;;;
;;; 원본파일:
;;;
;;; 명령어: DY
;;;
;;; 사용법
;;;  1) DY 실행
;;;  2) 첫 번째 점, 두 번째 점 클릭
;;;  3) 두 점의 중간 위치에 Y 거리 문자가 생성됨
;;;
;;; 가정 / 주의
;;;  - WCS 기준, 문자 높이는 TEXTSIZE, 현재 문자 스타일/도면층 사용
;;;  - 소수 자릿수는 LUPREC, 단위는 LUNITS 십진 표기 기준
;;;
;;; 동작방식
;;;  - dy = |y2 - y1| 을 rtos로 변환해 entmake로 TEXT 생성 (중앙 정렬)
;;; ==============================

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:DY (/ pt1 pt2 dist-y mid-pt text-str)
  (setq pt1 (getpoint "\n【DISTY】 첫 번째 점: "))
  (if pt1
    (progn
      (setq pt2 (getpoint pt1 "\n【DISTY】 두 번째 점: "))
      (if pt2
        (progn
          (setq dist-y (abs (- (cadr pt2) (cadr pt1))))
          (setq text-str (rtos dist-y 2 (getvar "LUPREC")))
          (setq mid-pt (trans (list (/ (+ (car pt1) (car pt2)) 2.0)
                                    (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                                    0.0)
                              1 0))
          (entmake
            (list
              '(0 . "TEXT")
              (cons 10 mid-pt)
              (cons 11 mid-pt)
              (cons 40 (getvar "TEXTSIZE"))
              (cons 7 (getvar "TEXTSTYLE"))
              (cons 1 text-str)
              '(72 . 1)
              '(73 . 2)
            )
          )
          (princ (strcat "\n【DISTY】 Y 거리: " text-str))
        )
      )
    )
  )
  (princ)
)

(princ)
