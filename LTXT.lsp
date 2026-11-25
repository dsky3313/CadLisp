;;; ==============================
;;; LTXT.LSP
;;;  - 선택한 선의 길이를 클릭한 텍스트의 내용으로 바꾼다
;;;
;;; 원본파일:
;;;
;;; 명령어: LTXT
;;;
;;; 사용법
;;;  1) 선(LINE/PLINE/ARC/SPLINE 등)을 미리 선택하고 LTXT 실행, 또는 실행 후 클릭
;;;  2) 텍스트(TEXT/MTEXT)를 클릭하면 바로 길이로 수정되고, 다시 선 선택으로 돌아감
;;;  3) 선 선택 단계에서 Enter/ESC로 종료 (텍스트 단계의 Enter는 선 다시 선택)
;;;
;;; 가정 / 주의
;;;  - 길이는 소수점 4째 자리에서 반올림해 3째 자리까지 표기, 모자라면 0 채움 (8.56 → 8.560)
;;;  - MTEXT는 내부 서식이 사라지고 길이 숫자만 남는다. 잠긴 도면층은 건너뛴다
;;;
;;; 동작방식
;;;  - 클릭한 선은 텍스트 선택이 끝날 때까지 하이라이트(redraw)
;;;  - vlax-curve로 선 길이를 구해 문자열로 변환 후, 클릭마다 DXF 1번 코드를 entmod
;;; ==============================
(vl-load-com)

;;------------------------------
;;  보조 함수
;;------------------------------
;; 객체의 곡선 길이 (실패 시 nil)
(defun ltxt:curve-length (ent / end-param result)
  (setq end-param (vl-catch-all-apply 'vlax-curve-getEndParam (list ent)))
  (if (and end-param (not (vl-catch-all-error-p end-param)))
    (progn
      (setq result (vl-catch-all-apply 'vlax-curve-getDistAtParam (list ent end-param)))
      (if (not (vl-catch-all-error-p result))
        result
      )
    )
  )
)

;; 소수점 3자리 문자열 (모자란 자리는 0으로 채움)
;; rtos는 DIMZIN 설정에 따라 끝자리 0을 지우므로 문자열로 직접 채운다 (TextRound 방식)
(defun ltxt:format-length (len / str dot-pos frac-len)
  (setq str (rtos len 2 3))
  (setq dot-pos (vl-string-position 46 str))
  (if dot-pos
    (progn
      (setq frac-len (- (strlen str) dot-pos 1))
      (while (< frac-len 3)
        (setq str (strcat str "0"))
        (setq frac-len (1+ frac-len))
      )
    )
    (setq str (strcat str ".000"))
  )
  str
)

;; 도면층 잠김 여부
(defun ltxt:layer-locked-p (data / layer-data)
  (setq layer-data (tblsearch "LAYER" (cdr (assoc 8 data))))
  (and layer-data (= 4 (logand 4 (cdr (assoc 70 layer-data)))))
)

;;------------------------------
;;  명령 함수
;;------------------------------
;; 텍스트 하나를 길이 문자열로 수정. 성공하면 T
(defun ltxt:apply-text (ent len-str / data)
  (setq data (entget ent))
  (cond
    ((not (member (cdr (assoc 0 data)) '("TEXT" "MTEXT")))
      (princ "\n【LTXT】 오류: 텍스트가 아닙니다.")
      nil
    )
    ((ltxt:layer-locked-p data)
      (princ "\n【LTXT】 오류: 잠긴 도면층입니다.")
      nil
    )
    (t
      (entmod (subst (cons 1 len-str) (assoc 1 data) data))
      (entupd ent)
      T
    )
  )
)

;; 선 하나에 대해 텍스트를 선택받아 수정 (수정 성공 또는 Enter/ESC 시 반환)
(defun ltxt:pick-text (len-str / pick done)
  (setq done nil)
  (while (and (not done) (setq pick (entsel "\n【LTXT】 수정할 텍스트 선택 <선 다시 선택>: ")))
    (setq done (ltxt:apply-text (car pick) len-str))
  )
)

;; 미리 선택된 객체(PICKFIRST) 중 첫 번째를 꺼낸다. 없으면 nil
(defun ltxt:preselected-ent (/ ss ent)
  (setq ss (ssget "_I"))
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (sssetfirst nil nil)  ; 그립/선택 표시 해제 (하이라이트와 겹치지 않게)
      ent
    )
  )
)

(defun c:LTXT (/ *error* pick pre-ent ent-line len)
  ;; ESC로 중단돼도 하이라이트가 남지 않게 해제
  (defun *error* (msg)
    (if ent-line
      (redraw ent-line 4)
    )
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【LTXT】 오류: " msg))
    )
    (princ)
  )
  (setq pre-ent (ltxt:preselected-ent))
  (while (setq pick (if pre-ent (list pre-ent) (entsel "\n【LTXT】 길이를 가져올 선 선택 <종료>: ")))
    (setq pre-ent nil)
    (setq ent-line (car pick))
    (setq len (ltxt:curve-length ent-line))
    (if len
      (progn
        (redraw ent-line 3)
        (princ (strcat "\n【LTXT】 길이: " (ltxt:format-length len)))
        (ltxt:pick-text (ltxt:format-length len))
        (redraw ent-line 4)
      )
      (princ "\n【LTXT】 오류: 길이를 구할 수 없는 객체입니다.")
    )
    (setq ent-line nil)
  )
  (princ)
)

(princ)
