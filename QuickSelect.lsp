;;; ==============================
;;; QuickSelect.LSP
;;;  - ZWCAD 스마트 선택(SMARTSEL)을 참고한 신속 선택 대화상자
;;;
;;; 원본파일: ZwSmartSelect.zrx (ZWCAD 2026, REA 분석)
;;;
;;; 명령어: QQ
;;;
;;; 사용법
;;;  1) 객체를 미리 선택하거나 명령 실행 후 선택한다.
;;;  2) 대화상자에서 속성별 값을 클릭해 체크(■)/해제(□)한다. 아래 개수가 실시간으로 갱신된다.
;;;  3) [찾기]를 누르면 조건에 맞는 객체만 선택된다. [객체 선택]으로 다시 고를 수 있다.
;;;
;;; 가정 / 주의
;;;  - 현재 공간(모형/배치 탭)의 객체만 대상. 블록 내부 객체는 제외.
;;;  - DCL 모달 창이라 창이 열린 동안 도면 조작은 불가. 임시 DCL 파일을 만들고 지운다.
;;;
;;; 동작방식
;;;  - 속성 사이는 AND, 한 속성 안에서 체크한 값끼리는 OR. 해당 속성이 없는 객체는 제외.
;;;  - 11개 속성 목록을 항상 같은 순서/배치로 표시하고, 결과는 sssetfirst로 선택 상태로 넘긴다.
;;; ==============================

;;------------------------------
;;  설정
;;------------------------------
(setq qsel:names
  '("객체 유형" "색상" "도면층" "선종류" "선종류 축척" "블록 이름"
    "선가중치" "문자 높이" "문자 스타일" "치수 스타일" "해치 패턴 이름")
)
;; 대화상자에 표시할 속성 번호와 순서 (한 줄에 3개)
(setq qsel:order
  '(0 2 1    ; 객체 유형, 도면층, 색상
    8 7 9    ; 문자 스타일, 문자 높이, 치수 스타일
    3 4 6    ; 선종류, 선종류 축척, 선가중치
    ;; 5     ; 블록 이름 (미사용)
    ;; 10    ; 해치 패턴 이름 (미사용)
   )
)(setq qsel:g-data nil)   ; (ename . entget) 목록
(setq qsel:g-inv nil)    ; 속성별 (값 . 개수) 목록
(setq qsel:g-sel nil)    ; 속성별 체크된 값 목록
(setq qsel:g-shown nil)  ; 목록으로 표시하는 속성 번호
(setq qsel:g-scope 0)    ; 0=현재 선택, 1=전체 도면

;;------------------------------
;;  속성 읽기
;;------------------------------
(defun qsel:color-label (code)
  (cond
    ((= code 256) "도면층별")
    ((= code 0) "블록별")
    ((= code 1) "빨간색")
    ((= code 2) "노란색")
    ((= code 3) "초록색")
    ((= code 4) "하늘색")
    ((= code 5) "파란색")
    ((= code 6) "선홍색")
    ((= code 7) "흰색")
    (t (strcat "색상 " (itoa code)))
  )
)

(defun qsel:lweight-label (code)
  (cond
    ((= code -1) "도면층별")
    ((= code -2) "블록별")
    ((= code -3) "기본값")
    (t (strcat (rtos (/ code 100.0) 2 2) " mm"))
  )
)

(defun qsel:linetype-label (name)
  (cond
    ((null name) "도면층별")
    ((= (strcase name) "BYLAYER") "도면층별")
    ((= (strcase name) "BYBLOCK") "블록별")
    (t name)
  )
)

;; 객체 유형 한글명: ZWCAD ko-KR dotNetSmartSelectRes.dll 명칭표 기준
(defun qsel:type-label (ed / type-name flag pair)
  (setq type-name (cdr (assoc 0 ed)))
  (cond
    ((= type-name "DIMENSION")
      (setq flag (logand (cdr (assoc 70 ed)) 7))
      (nth flag
        '("회전된 치수" "정렬된 치수" "각도 치수" "지름 치수"
          "반지름 치수" "3점 각도 치수" "세로 좌표 치수" "회전된 치수")
      )
    )
    ((= type-name "POLYLINE")
      (setq flag (cdr (assoc 70 ed)))
      (cond
        ((= (logand flag 8) 8) "3D 폴리선")
        ((= (logand flag 16) 16) "다각형 메쉬")
        ((= (logand flag 64) 64) "폴리면 메쉬")
        (t "2D 폴리선")
      )
    )
    ((and (= type-name "INSERT")
          (or (> (cond ((cdr (assoc 70 ed))) (t 1)) 1)
              (> (cond ((cdr (assoc 71 ed))) (t 1)) 1)))
      "블록 다중 삽입"
    )
    (t
      (setq pair
        (assoc type-name
          '(("LINE" . "선") ("CIRCLE" . "원") ("ARC" . "호") ("ELLIPSE" . "타원")
            ("LWPOLYLINE" . "폴리선") ("SPLINE" . "스플라인")
            ("TEXT" . "문자") ("MTEXT" . "여러 줄 문자") ("INSERT" . "블록 참조")
            ("ATTRIB" . "속성") ("ATTDEF" . "속성") ("HATCH" . "해치")
            ("ARC_DIMENSION" . "호 길이 치수") ("LEADER" . "지시선")
            ("MULTILEADER" . "다중 지시선") ("POINT" . "점") ("SOLID" . "솔리드")
            ("3DSOLID" . "3D 솔리드") ("3DFACE" . "3D 면") ("REGION" . "영역")
            ("RAY" . "광선") ("XLINE" . "X선") ("TRACE" . "추적") ("SHAPE" . "쉐이프")
            ("TOLERANCE" . "공차") ("IMAGE" . "래스터 이미지") ("VIEWPORT" . "뷰포트")
            ("ACAD_TABLE" . "테이블") ("WIPEOUT" . "객체 가리기") ("OLE2FRAME" . "OLE")
            ("BODY" . "본체") ("ARCALIGNEDTEXT" . "호 정렬 문자")
          )
        )
      )
      (if pair (cdr pair) type-name)
    )
  )
)
;; 객체의 idx번째 속성 값을 문자열로 반환. 해당 속성이 없으면 nil
(defun qsel:prop (ed idx / type-name value)
  (setq type-name (cdr (assoc 0 ed)))
  (cond
    ((= idx 0) (qsel:type-label ed))
    ((= idx 1)
      (setq value (cdr (assoc 62 ed)))
      (qsel:color-label (if value value 256))
    )
    ((= idx 2) (cdr (assoc 8 ed)))
    ((= idx 3) (qsel:linetype-label (cdr (assoc 6 ed))))
    ((= idx 4)
      (setq value (cdr (assoc 48 ed)))
      (rtos (if value value 1.0) 2 4)
    )
    ((= idx 5) (if (= type-name "INSERT") (cdr (assoc 2 ed))))
    ((= idx 6)
      (setq value (cdr (assoc 370 ed)))
      (qsel:lweight-label (if value value -1))
    )
    ((= idx 7)
      (if (member type-name '("TEXT" "MTEXT" "ATTRIB" "ATTDEF"))
        (rtos (cdr (assoc 40 ed)) 2 4)
      )
    )
    ((= idx 8)
      (if (member type-name '("TEXT" "MTEXT" "ATTRIB" "ATTDEF"))
        (cdr (assoc 7 ed))
      )
    )
    ((= idx 9) (if (= type-name "DIMENSION") (cdr (assoc 3 ed))))
    ((= idx 10) (if (= type-name "HATCH") (cdr (assoc 2 ed))))
  )
)

;; 속성 idx의 (값 . 개수) 목록
(defun qsel:inventory (data-list idx / result label pair item)
  (foreach item data-list
    (setq label (qsel:prop (cdr item) idx))
    (if label
      (progn
        (setq pair (assoc label result))
        (if pair
          (setq result (subst (cons label (1+ (cdr pair))) pair result))
          (setq result (cons (cons label 1) result))
        )
      )
    )
  )
  (vl-sort result '(lambda (a b) (< (car a) (car b))))
)

(defun qsel:match (ed / i ok)
  (setq i 0)
  (setq ok T)
  (while (and ok (< i 11))
    (if (nth i qsel:g-sel)
      (setq ok (member (qsel:prop ed i) (nth i qsel:g-sel)))
    )
    (setq i (1+ i))
  )
  ok
)

;;------------------------------
;;  선택 대상 / 상태
;;------------------------------
;; 선택 집합을 (ename . entget) 목록으로 복사
(defun qsel:ss->data (ss / i ename result)
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq result (cons (cons ename (entget ename)) result))
    (setq i (1+ i))
  )
  (reverse result)
)

(defun qsel:set-nth (lst idx value / i result item)
  (setq i 0)
  (foreach item lst
    (setq result (cons (if (= i idx) value item) result))
    (setq i (1+ i))
  )
  (reverse result)
)

(defun qsel:split (str / tokens token ch i)
  (setq token "")
  (setq i 1)
  (while (<= i (strlen str))
    (setq ch (substr str i 1))
    (if (= ch " ")
      (progn
        (if (/= token "")
          (setq tokens (cons token tokens))
        )
        (setq token "")
      )
      (setq token (strcat token ch))
    )
    (setq i (1+ i))
  )
  (if (/= token "")
    (setq tokens (cons token tokens))
  )
  (reverse tokens)
)

;; 대상이 바뀔 때마다 속성 목록을 다시 만들고 체크 상태를 비운다
(defun qsel:build (data-list / idx inv)
  (setq qsel:g-data data-list)
  (setq qsel:g-inv nil)
  (setq qsel:g-sel nil)
  (setq qsel:g-shown qsel:order)
  (setq idx 0)
  (while (< idx 11)
    (setq inv (qsel:inventory data-list idx))
    (setq qsel:g-inv (append qsel:g-inv (list inv)))
    (setq qsel:g-sel (append qsel:g-sel (list nil)))
    (setq idx (1+ idx))
  )
)

(defun qsel:count-match (/ count item)
  (setq count 0)
  (foreach item qsel:g-data
    (if (qsel:match (cdr item))
      (setq count (1+ count))
    )
  )
  count
)

;;------------------------------
;;  대화상자
;;------------------------------
(defun qsel:update-count (/ count)
  (setq count (qsel:count-match))
  (set_tile "count"
    (strcat (itoa count) "개 / 전체 " (itoa (length qsel:g-data)) "개")
  )
  (mode_tile "find" (if (= count 0) 1 0))
)

(defun qsel:item-text (idx n / item)
  (setq item (nth n (nth idx qsel:g-inv)))
  (strcat
    (if (member (car item) (nth idx qsel:g-sel)) "■ " "□ ")
    (car item) "  (" (itoa (cdr item)) ")"
  )
)

(defun qsel:fill-list (idx / n item)
  (start_list (strcat "lb" (itoa idx)))
  (setq n 0)
  (foreach item (nth idx qsel:g-inv)
    (add_list (qsel:item-text idx n))
    (setq n (1+ n))
  )
  (end_list)
)

;; 항목을 클릭하면 체크(■)/해제(□)를 토글. 선택을 비워 같은 항목을 다시 눌러도 반응하게 한다
(defun qsel:on-list (idx value / key n label current)
  (setq key (strcat "lb" (itoa idx)))
  (setq n (atoi value))
  (setq label (car (nth n (nth idx qsel:g-inv))))
  (setq current (nth idx qsel:g-sel))
  (if (member label current)
    (setq current (vl-remove label current))
    (setq current (append current (list label)))
  )
  (setq qsel:g-sel (qsel:set-nth qsel:g-sel idx current))
  (start_list key 1 n)
  (add_list (qsel:item-text idx n))
  (end_list)
  (set_tile key "")
  (qsel:update-count)
)

(defun qsel:clear-tiles (/ idx)
  (foreach idx qsel:g-shown
    (setq qsel:g-sel (qsel:set-nth qsel:g-sel idx nil))
    (qsel:fill-list idx)
  )
  (qsel:update-count)
)
;; 11개 속성을 고정 순서로 배치한 DCL을 만든다 (한 줄에 3열)
(defun qsel:write-dcl (path / file shown col)
  (setq file (open path "w"))
  (write-line "qsel_dlg : dialog {" file)
  (write-line "  label = \"신속 선택\";" file)
  (write-line "  : row {" file)
  (write-line "    : text { label = \"찾기:\"; width = 6; }" file)
  (write-line "    : popup_list { key = \"scope\"; width = 16; }" file)
  (write-line "    : button { key = \"pick\"; label = \"객체 선택\"; width = 12; }" file)
  (write-line "    : text { key = \"count\"; width = 28; }" file)
  (write-line "  }" file)
  (setq shown qsel:g-shown)
  (setq col 0)
  (while shown
    (if (= (rem col 3) 0)
      (write-line "  : row {" file)
    )
    (write-line
      (strcat
        "    : boxed_column { label = \"" (nth (car shown) qsel:names) "\";"
        " : list_box { key = \"lb" (itoa (car shown)) "\";"
        " width = 24; height = 10; } }"
      )
      file
    )
    (setq col (1+ col))
    (setq shown (cdr shown))
    (if (or (= (rem col 3) 0) (null shown))
      (write-line "  }" file)
    )
  )
  (write-line "  : row {" file)
  (write-line "    : button { key = \"find\"; label = \"찾기\"; is_default = true; width = 12; }" file)
  (write-line "    : button { key = \"clear\"; label = \"초기화\"; width = 12; }" file)
  (write-line "    : cancel_button { label = \"닫기\"; width = 12; }" file)
  (write-line "  }" file)
  (write-line "}" file)
  (close file)
)

;; 반환: 0=닫기, 1=찾기, 2=객체 선택, 3=범위 변경
(defun qsel:run-dialog (dcl-path / dcl-id status idx)
  (setq dcl-id (load_dialog dcl-path))
  (if (new_dialog "qsel_dlg" dcl-id)
    (progn
      (start_list "scope")
      (add_list "현재 선택")
      (add_list "전체 도면")
      (end_list)
      (set_tile "scope" (itoa qsel:g-scope))
      (foreach idx qsel:g-shown
        (qsel:fill-list idx)
        (action_tile (strcat "lb" (itoa idx)) (strcat "(qsel:on-list " (itoa idx) " $value)"))
      )
      (action_tile "scope" "(setq qsel:g-scope (atoi $value)) (done_dialog 3)")
      (action_tile "pick" "(done_dialog 2)")
      (action_tile "clear" "(qsel:clear-tiles)")
      (action_tile "find" "(done_dialog 1)")
      (qsel:update-count)
      (setq status (start_dialog))
    )
    (setq status 0)
  )
  (unload_dialog dcl-id)
  status
)

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:QQ (/ start-time ss picked dcl-path status done result-ss count item)
  (setq start-time (getvar "MILLISECS"))
  (setq qsel:g-scope 0)
  (setq ss (cadr (ssgetfirst)))
  (if (null ss)
    (progn
      (princ "\n【QuickSelect】 ")
      (setq ss (ssget))
    )
  )
  (if ss
    (progn
      (setq picked (qsel:ss->data ss))
      (setq dcl-path (vl-filename-mktemp "qsel.dcl"))
      (while (not done)
        (if (= qsel:g-scope 0)
          (qsel:build picked)
          (progn
            (setq ss (ssget "_X" (list (cons 410 (getvar "CTAB")))))
            (qsel:build (if ss (qsel:ss->data ss)))
          )
        )
        (qsel:write-dcl dcl-path)
        (setq status (qsel:run-dialog dcl-path))
        (cond
          ((= status 1)
            (setq result-ss (ssadd))
            (foreach item qsel:g-data
              (if (qsel:match (cdr item))
                (ssadd (car item) result-ss)
              )
            )
            (setq count (sslength result-ss))
            (sssetfirst nil result-ss)
            (princ
              (strcat "\n【QuickSelect】 " (itoa count) "개 선택됨, Time: "
                      (itoa (- (getvar "MILLISECS") start-time)) " ms")
            )
            (setq done T)
          )
          ((= status 2)
            (princ "\n【QuickSelect】 ")
            (setq ss (ssget))
            (if ss
              (progn
                (setq picked (qsel:ss->data ss))
                (setq qsel:g-scope 0)
              )
            )
          )
          ((= status 3) nil)
          (t (setq done T))
        )
      )
      (vl-file-delete dcl-path)
    )
  )
  (princ)
)

(princ)
