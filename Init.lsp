;;; ==============================
;;; Init.LSP
;;;  - 설정리습: 작업 환경 시스템 변수 일괄 설정
;;;
;;; 원본파일: OO.lsp
;;;
;;; 명령어: II
;;;
;;; 사용법
;;;  1) 명령행에 II 입력
;;;
;;; 가정 / 주의
;;;  - TRIM 모드 옵션(o)은 AutoCAD 2021 이상 기준
;;;  - LAYISO 설정 입력("s" "o" "o")은 미검증, 명령 기록(F2)으로 확인
;;;  - DWGCHECK 0 은 도면 열 때 무결성 검사를 끔
;;;
;;; 동작방식
;;;  - ORTHO, OSNAP, PICKADD 등을 command 로 설정하고 결과를 출력
;;;  - 설정 변경이 목적이라 원복하지 않음 (CMDECHO 만 복원)
;;; ==============================

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:II (/ cmdecho-orig *error*)
  (defun *error* (msg)
    (if cmdecho-orig
      (setvar "CMDECHO" cmdecho-orig)
    )
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【Init】 오류: " msg))
    )
    (princ)
  )

  (setq cmdecho-orig (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (command "dwgcheck" "0")
  (command "dynmode" "3")
  (command "layiso" "s" "o" "o" "")
  (command "HPDLGMODE" "1")
  (command "NAVBARDISPLAY" "0")
  (command "ORTHO" "on")
  (command "osnap" "end,mid,qua,ext,per,nea,int")
  (command "pickadd" "0")
  (command "pickfirst" "1")
  (command "trim" "o" "s" "" "")

  (setvar "CMDECHO" cmdecho-orig)

  (princ "\n【Init】 설정 적용 완료")
  (princ)
)

(princ)