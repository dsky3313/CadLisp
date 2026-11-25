;; freeze_dimtext.lsp
;; 치수 텍스트값 고정/복구 루틴
;; 명령어: DFR
;;   DFR [Enter/Space/F] : 선택 치수 텍스트값 고정
;;   DFR U               : 선택 치수 텍스트값 복구
;; 대상: 선형, 정렬, 각도, 반지름, 지름, 호길이, 세로좌표 등 모든 치수

;;; ─── 스타일 테이블 DXF값 읽기 ──────────────────────────────────────
(defun _dfr-get-style-val ( stylename dxfcode / dimstyle )
  (if (and stylename (tblsearch "DIMSTYLE" stylename))
    (progn
      (setq dimstyle (entget (tblobjname "DIMSTYLE" stylename)))
      (cdr (assoc dxfcode dimstyle))
    )
  )
)

;;; ─── 선형 소수점 자리수 ─────────────────────────────────────────────
(defun _dfr-get-dimdec ( ed / stylename dimdec )
  (setq dimdec (cdr (assoc 271 ed)))
  (if (not dimdec)
    (progn
      (setq stylename (cdr (assoc 3 ed)))
      (setq dimdec (_dfr-get-style-val stylename 271))
    )
  )
  (if (not dimdec) (setq dimdec (getvar "DIMDEC")))
  dimdec
)

;;; ─── 각도 소수점 자리수 ─────────────────────────────────────────────
(defun _dfr-get-dimadec ( ed / stylename dimadec )
  (setq dimadec (cdr (assoc 179 ed)))
  (if (not dimadec)
    (progn
      (setq stylename (cdr (assoc 3 ed)))
      (setq dimadec (_dfr-get-style-val stylename 179))
    )
  )
  (if (not dimadec) (setq dimadec (getvar "DIMADEC")))
  dimadec
)

;;; ─── 각도 치수 판별 (DXF 70: 2=각도2점, 5=각도3점) ─────────────────
(defun _dfr-is-angular ( ed )
  (member (cdr (assoc 70 ed)) '(2 5))
)

;;; ─── 텍스트 포맷 ────────────────────────────────────────────────────
(defun _dfr-fmt-linear ( meas dec )
  (rtos meas 2 dec)
)

(defun _dfr-fmt-angular ( meas_rad dimadec ed / dimaunit stylename )
  (setq dimaunit (cdr (assoc 275 ed)))
  (if (not dimaunit)
    (progn
      (setq stylename (cdr (assoc 3 ed)))
      (setq dimaunit (_dfr-get-style-val stylename 275))
    )
  )
  (if (not dimaunit) (setq dimaunit (getvar "DIMAUNIT")))
  (cond
    ( (= dimaunit 0) (angtos meas_rad 0 dimadec) )
    ( (= dimaunit 1) (angtos meas_rad 1 dimadec) )
    ( (= dimaunit 2) (angtos meas_rad 6 dimadec) )
    ( (= dimaunit 3) (angtos meas_rad 4 dimadec) )
    ( (= dimaunit 4) (angtos meas_rad 5 dimadec) )
    ( t              (angtos meas_rad 0 dimadec) )
  )
)

;;; ─── ssget → ename 리스트 ───────────────────────────────────────────
(defun _dfr-ss->enlist ( ss / i result )
  (setq i 0  result nil)
  (if ss
    (while (< i (sslength ss))
      (setq result (cons (ssname ss i) result))
      (setq i (1+ i))
    )
  )
  result
)

;;; ─── 치수 텍스트 고정 ───────────────────────────────────────────────
(defun _dfr-freeze ( enlist / ent ed meas fixedtxt existing cnt skip )
  (setq cnt 0  skip 0)
  (foreach ent enlist
    (setq ed       (entget ent))
    (setq existing (cdr (assoc 1 ed)))
    ;;; 이미 재지정 있음 → 건너뜀
    (if (and existing (/= existing "") (/= existing "<>"))
      (setq skip (1+ skip))
      (progn
        (setq meas (cdr (assoc 42 ed)))
        (if (_dfr-is-angular ed)
          (setq fixedtxt (_dfr-fmt-angular meas (_dfr-get-dimadec ed) ed))
          (setq fixedtxt (_dfr-fmt-linear  meas (_dfr-get-dimdec  ed)))
        )
        (entmod (subst (cons 1 fixedtxt) (assoc 1 ed) ed))
        (setq cnt (1+ cnt))
      )
    )
  )
  (list cnt skip)
)

;;; ─── 치수 텍스트 복구 ───────────────────────────────────────────────
(defun _dfr-unfreeze ( enlist / ent ed cnt )
  (setq cnt 0)
  (foreach ent enlist
    (setq ed (entget ent))
    (entmod (subst (cons 1 "") (assoc 1 ed) ed))
    (setq cnt (1+ cnt))
  )
  cnt
)

;;; ─── DFR 메인 ───────────────────────────────────────────────────────
(defun c:DFR ( / kwd ss enlist res cnt skip )
  (initget "F U")
  (setq kwd (getkword "\n【DFR】 옵션 [고정(F)/복구(U)] <고정>: "))

  (if (or (not kwd) (= kwd "F"))
    ;;; 고정 모드
    (progn
      (setq ss (progn (princ "\n【DFR】 ") (ssget '((0 . "DIMENSION")))))
      (if (not ss) (progn (princ "\n선택된 치수가 없습니다.") (princ) (exit)))
      (setq enlist (_dfr-ss->enlist ss))
      (setq res  (_dfr-freeze enlist))
      (setq cnt  (car res))
      (setq skip (cadr res))
      (princ (strcat "\n" (itoa cnt) "개 치수 텍스트 고정 완료."))
      (if (> skip 0)
        (princ (strcat " (재지정 있는 " (itoa skip) "개 건너뜀)"))
      )
    )
    ;;; 복구 모드
    (progn
      (setq ss (progn (princ "\n【DFR】 ") (ssget '((0 . "DIMENSION")))))
      (if (not ss) (progn (princ "\n선택된 치수가 없습니다.") (princ) (exit)))
      (setq cnt (_dfr-unfreeze (_dfr-ss->enlist ss)))
      (princ (strcat "\n" (itoa cnt) "개 치수 텍스트 복구 완료."))
    )
  )
  (princ)
)