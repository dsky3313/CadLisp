;;; ==============================
;;; Rotate.LSP
;;;  - 선택 객체를 지정한 점 기준으로 X축 90도 회전 (ROTATE3D)
;;;
;;; 원본파일:
;;;
;;; 명령어: RX, RR, RRR
;;;
;;; 사용법
;;;  1) RX 입력 후 객체 선택
;;;  2) 회전 기준점 클릭
;;;
;;; 가정 / 주의
;;;  - 현재 UCS의 X축과 평행한 축으로 +90도 회전
;;;
;;; 동작방식
;;;  - ROTATE3D X축 옵션에 기준점과 각도 90을 전달
;;; ==============================

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:RX (/ cmdecho-orig ss base-pt *error*)
  (defun *error* (msg)
    (if cmdecho-orig
      (setvar "CMDECHO" cmdecho-orig)
    )
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【Rotate】 오류: " msg))
    )
    (princ)
  )

  (princ "\n【Rotate】 ")
  (setq ss (ssget))
  (if ss
    (progn
      (setq base-pt (getpoint "\n【Rotate】 회전 기준점 지정: "))
      (if base-pt
        (progn
          (setq cmdecho-orig (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (command "_.ROTATE3D" ss "" "_X" "_non" base-pt 90)
          (setvar "CMDECHO" cmdecho-orig)
        )
      )
    )
  )
  (princ)
)

;;------------------------------
;;  RR / RRR 단축 명령
;;------------------------------
(defun c:RR ()
  (command "rotate")
  (princ)
)

(defun c:RRR ()
  (command "ROTATE3D")
  (princ)
)

(princ)
