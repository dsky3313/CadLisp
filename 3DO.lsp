;;; ==============================
;;; 3DO.LSP
;;;  - Shift+휠 투명 오빗(3DORBITTRANSPARENT) ON/OFF, 설정 기억
;;;
;;; 원본파일:
;;;
;;; 명령어: 3DO
;;;
;;; 사용법
;;;  1) 3DO -> [ON(1)/OFF(2)] 번호 클릭 또는 입력, Enter = 현재값 (기본값 OFF)
;;;  2) 선택은 레지스트리에 저장되어 AutoCAD 시작 때 자동 적용
;;;
;;; 가정 / 주의
;;;  - CUI 매크로가 '_3DORBITTRANSPARENT ('.' 접두사 없음) 라는 가정
;;;  - 시작 시 적용은 S::STARTUP 사용, 다른 LISP 의 S::STARTUP 과 겹치면 덮어씀
;;;
;;; 동작방식
;;;  - OFF: UNDEFINE / ON: REDEFINE. 값은 HKCU\Software\hodo\3DO 의 State
;;; ==============================

;;------------------------------
;;  설정
;;------------------------------
(setq tdo:key "HKEY_CURRENT_USER\\Software\\hodo\\3DO")

;;------------------------------
;;  보조 함수
;;------------------------------
;; 저장된 상태 (없으면 기본값 Off)
(defun tdo:state (/ value)
  (setq value (vl-registry-read tdo:key "State"))
  (if (= value "ON")
    "ON"
    "OFF"
  )
)

;; 상태를 AutoCAD 에 적용 (Off = UNDEFINE, On = REDEFINE)
(defun tdo:apply (mode / cmdecho-orig)
  (setq cmdecho-orig (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (vl-cmdf (if (= mode "OFF") "_.UNDEFINE" "_.REDEFINE") "3DORBITTRANSPARENT")
  (while (> (getvar "CMDACTIVE") 0)
    (command)  ; 이미 같은 상태라 프롬프트가 남으면 취소
  )
  (setvar "CMDECHO" cmdecho-orig)
  (princ)
)

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:3DO (/ key mode)
  (initget "1 2")
  (setq key (getkword (strcat "\n【3DO】 Shift+\U+D720 3D \U+D654\U+BA74 \U+D68C\U+C804 [ON(1)/OFF(2)] <" (tdo:state) ">: ")))
  (setq mode
    (cond
      ((= key "1") "ON")
      ((= key "2") "OFF")
      ((tdo:state))
    )
  )
  (vl-registry-write tdo:key "State" mode)
  (tdo:apply mode)
  (princ (strcat "\n【3DO】 Shift+\U+D720 3D \U+D654\U+BA74 \U+D68C\U+C804: " mode))
  (princ)
)

;;------------------------------
;;  로드 시 초기화
;;------------------------------
;; 시작 중에는 command 가 불안정해서 도면이 열린 뒤 적용
(defun s::startup ()
  (tdo:apply (tdo:state))
  (princ)
)

(princ)