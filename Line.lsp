;;; ==============================
;;; Line.LSP
;;;  - 선 / 폴리선 / 무한선 단축 명령, 두 점 분할점에서 LINE 시작
;;;
;;; 원본파일: Line.lsp
;;;
;;; 명령어: 1, 1`, 1``, 11, 111, 112, 113, 12, 13
;;;
;;; 사용법
;;;  1) 1 = LINE, 1` = PLINE, 1`` = SPLINE, 11 = XLINE
;;;  2) 111 = XLINE 수직, 112 = XLINE 수평, 113 = XLINE 두 점의 중간선
;;;  3) 12 / 13 = 두 점 선택 후 1/2, 1/3 지점에서 LINE 시작 (MADE BY EEZEN)
;;;
;;; 가정 / 주의
;;;  - 12 / 13 은 LINE 시작점 지정 동안만 OSNAP 끔, 이후 즉시 복원
;;;
;;; 동작방식
;;;  - 두 점 사이 거리를 div 로 나눈 지점을 polar 로 계산해 LINE 시작점으로 사용
;;; ==============================


;;------------------------------
;;  기본 명령
;;------------------------------
(defun c:1 ()
  (command "line")
  (princ)
)

(defun c:1` ()
  (command "pline")
  (princ)
)

(defun c:1`` ()
  (command "spline")
  (princ)
)

(defun c:11 ()
  (command "xline")
  (princ)
)

(defun c:111 ()
  (command "xline" "v")
  (princ)
)

(defun c:112 ()
  (command "xline" "h")
  (princ)
)

;; XLINE midpoints
(defun c:113 ()
  (command "xline" "m2p")
  (princ)
)


;;------------------------------
;;  두 점 분할점에서 LINE 시작
;;------------------------------
(defun line:divpt (div / pt1 pt2 pt osmode-orig *error*)
  (defun *error* (msg)
    (if osmode-orig
      (setvar "osmode" osmode-orig)
    )
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【Line】 오류: " msg))
    )
    (princ)
  )

  (setq pt1 (getpoint "\n【Line】 첫 번째 점 선택: "))
  (if pt1
    (setq pt2 (getpoint "\n【Line】 두 번째 점 선택: "))
  )
  (if (and pt1 pt2)
    (progn
      (setq pt (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) div)))
      (setq osmode-orig (getvar "osmode"))
      (setvar "osmode" 0)
      (command "LINE" pt)
      (setvar "osmode" osmode-orig)
    )
  )
  (princ)
)

;; 2점의 MID점을 찾는다(MADE BY EEZEN)
(defun c:12 ()
  (line:divpt 2)
)

;; 2점의 1/3점을 찾는다(MADE BY EEZEN)
(defun c:13 ()
  (line:divpt 3)
)

(princ)