;;; ==============================
;;; 3D2D.LSP
;;;  - 선택한 객체의 Z값을 0으로 만들어 3D를 평면(2D)으로 변환
;;;
;;; 원본파일: 3d2d.lsp (Shin Jong-Hwa, DooSan ENG.)
;;;
;;; 명령어: 3D2D
;;;
;;; 사용법
;;;  1) 3D2D 입력
;;;  2) 변환할 객체 선택
;;;  3) 완료 후 소요 시간(ms) 확인
;;;
;;; 가정 / 주의
;;;  - 좌표 그룹(10 11 13 14 21 30 31)은 첫 번째 값만 변환 (3DFACE 12번 등 제외)
;;;  - 블록 내부, 3D 솔리드, 메시는 변환 안 됨. 잠긴 도면층 객체가 있으면 중간에 멈춤
;;;  - 실행 후 ELEVATION 이 0으로 바뀜
;;;
;;; 동작방식
;;;  - POLYLINE 은 꼭짓점 Z를, 그 외는 고도(38)와 좌표 Z를 0으로 바꿔 entmod
;;;  - 완료 후 전체 소요 시간(ms)을 표시
;;; ==============================

;;------------------------------
;;  보조 함수
;;------------------------------

(defun 3d2d:noele (dev num / old-data new-data data flat-pnt)
  (setq data (cdr (assoc num dev)))
  (setq old-data (assoc num dev))
  (setq flat-pnt (list (car data) (cadr data) 0))
  (setq new-data (cons num flat-pnt))
  (setq dev (subst new-data old-data dev))
)

;;------------------------------
;;  Dim Text Change
;;------------------------------

(defun c:3D2D (/ *error* cmdecho-orig dcl-id3 sel-set sel-count idnum edata obj name ename
               next-ent next-data old-pnt old-38 new-38 start-time end-time)

  (defun *error* (msg)
    (if cmdecho-orig (setvar "cmdecho" cmdecho-orig))
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【3D2D】 오류: " msg))
    )
    (princ)
  )

  (setq cmdecho-orig (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (princ "\n【3D2D】 ")
  (setq sel-set (ssget))
  (setq sel-count (if sel-set (sslength sel-set) 0))
  (setq start-time (getvar "MILLISECS"))
  (setq idnum 0.)
  (repeat sel-count
    (setq ename (ssname sel-set idnum))
    (setq edata (entget ename))
    (if (= (cdr (assoc 0 edata)) "POLYLINE")
      (progn  ;THEN
        (setq next-ent ename)  ;첫 엔티티 이름
        (while (and (setq next-ent (entnext next-ent))
                    (/= (cdr (assoc 0 (setq next-data (entget next-ent)))) "SEQEND")  ;엔티티정보축출
               )
          (if (= (cdr (assoc 0 next-data)) "VERTEX")  ;절점일때만
            (progn
              (setq old-pnt (cdr (assoc 10 next-data)))
              (setq next-data
                (subst (cons 10 (list (car old-pnt) (cadr old-pnt) 0))
                       (assoc 10 next-data)
                       next-data
                )
              )
              (entmod next-data)
            )  ;progn
          )  ;if
        )  ;while
        (entupd ename)
      )  ;progn
      (progn  ;ELSE
        (setq old-38 (assoc 38 edata))
        (setq new-38 (cons (car old-38) 0.0))
        (setq edata (subst new-38 old-38 edata))
        (setq obj (cdr (assoc 0 edata)))
        (setq name (cdr (assoc 1 edata)))
        (setq edata (3d2d:noele edata 10))
        (setq edata (3d2d:noele edata 11))
        (setq edata (3d2d:noele edata 13))
        (setq edata (3d2d:noele edata 14))
        (setq edata (3d2d:noele edata 21))
        (setq edata (3d2d:noele edata 30))
        (setq edata (3d2d:noele edata 31))
        (entmod edata)
      )  ;progn
    )  ;if
    (setq idnum (1+ idnum))
    (princ
      (strcat "【3D2D】 Converting Data Num : " (rtos idnum 2 0)
              "(" (rtos (* (/ idnum sel-count) 100) 2 0) "%)" (chr 13)
      )
    )
  )  ;repeat
  (setvar "ELEVATION" 0)
  (setq end-time (getvar "MILLISECS"))
  (princ (strcat "\n【3D2D】 Time: " (rtos (- end-time start-time) 2 0) " ms"))
  (princ "\n【3D2D】 Convert Complete")
  (setvar "cmdecho" cmdecho-orig)
  (princ)
)  ;defun

(princ)