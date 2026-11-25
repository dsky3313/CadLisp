;;; ==============================
;;; Excel.LSP
;;;  - CAD 안에서 엑셀 실행 / 선택한 텍스트를 엑셀로 내보내기
;;;
;;; 원본파일:
;;;
;;; 명령어: EXC, EXT
;;;
;;; 사용법
;;;  1) EXC : 엑셀 실행
;;;  2) EXT : 텍스트(TEXT/MTEXT) 선택 > 엑셀 A열에 한 개당 한 셀씩 입력
;;;
;;; 가정 / 주의
;;;  - AutoCAD LT 불가 (COM 사용), 엑셀 설치 필요
;;;  - 삽입점 X 기준 왼쪽>오른쪽 순서(같으면 위>아래)로 입력, 모두 문자열(@)로 저장 (001 같은 값 유지)
;;;  - MTEXT 서식코드(\P 등)는 변환 없이 그대로 입력됨
;;;
;;; 동작방식
;;;  - 실행 중인 엑셀이 있으면 재사용, 새 통합문서에 A1부터 기록
;;; ==============================

(vl-load-com)

;;------------------------------
;;  보조 함수
;;------------------------------
;; COM 객체 목록 해제 (이미 해제된 것은 무시)
(defun exc:release (objs / obj)
  (foreach obj objs
    (if (= (type obj) 'VLA-OBJECT)
      (vl-catch-all-apply 'vlax-release-object (list obj))
    )
  )
)

;; 선택셋 > (삽입점 . ename) 목록, X 오름차순 (같으면 Y 내림차순)
(defun exc:sort-left-right (ss / i en pt lst)
  (setq i 0)
  (repeat (sslength ss)
    (setq en (ssname ss i))
    (setq pt (cdr (assoc 10 (entget en))))
    (setq lst (cons (cons pt en) lst))
    (setq i (1+ i))
  )
  (vl-sort lst
    (function
      (lambda (a b)
        (if (equal (car (car a)) (car (car b)) 1e-6)
          (> (cadr (car a)) (cadr (car b)))
          (< (car (car a)) (car (car b)))
        )
      )
    )
  )
)

;; 셀 하나에 값 입력
(defun exc:put-cell (sheet addr val / rng)
  (setq rng (vlax-get-property sheet 'Range addr))
  (vlax-put-property rng 'Value2 val)
  (vlax-release-object rng)
)

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:EXC ()
  (startapp "cmd.exe" "/c start excel")
  (princ "\n[Excel] 엑셀 실행")
  (princ)
)

(defun c:EXT (/ *error* ss xl wbs wb sheet rng i item)
  (defun *error* (msg)
    (exc:release (list rng sheet wb wbs xl))
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n[Excel] 오류: " msg))
    )
    (princ)
  )
  (princ "\n[Excel] ")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (cond
    ((not ss)
      (princ "\n[Excel] 선택된 텍스트가 없습니다.")
    )
    ((not (setq xl (vlax-get-or-create-object "Excel.Application")))
      (princ "\n[Excel] 오류: 엑셀을 실행할 수 없습니다.")
    )
    (t
      (setq wbs (vlax-get-property xl 'Workbooks))
      (setq wb (vlax-invoke-method wbs 'Add))
      (setq sheet (vlax-get-property wb 'ActiveSheet))
      (setq rng (vlax-get-property sheet 'Range "A:A"))
      (vlax-put-property rng 'NumberFormat "@")  ; 문자열로 저장해 001 -> 1 변환 방지
      (vlax-release-object rng)
      (setq i 0)
      (foreach item (exc:sort-left-right ss)
        (exc:put-cell
          sheet
          (strcat "A" (itoa (1+ i)))
          (vla-get-textstring (vlax-ename->vla-object (cdr item)))
        )
        (setq i (1+ i))
      )
      (vlax-put-property xl 'Visible :vlax-true)
      (exc:release (list sheet wb wbs xl))
      (princ (strcat "\n[Excel] " (itoa i) "개 내보내기 완료"))
    )
  )
  (princ)
)

(princ)
