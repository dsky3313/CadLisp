;;; ==============================
;;; MultiPlotPDF.LSP
;;;  - 도곽 블록마다 한 페이지씩, 여러 페이지 PDF 한 개로 출력
;;;
;;; 원본파일: MPL.lsp (v34)
;;;
;;; 명령어: PPP
;;;
;;; 사용법
;;;  1) PPP 실행 후 대화상자에서 프린터, 용지, CTB, 방향, 축척, 옵션, 도곽 블록 선택
;;;  2) 등록 도곽이 선택돼 있으면 바로 수집, 아니면 도곽 블록 선택 (Enter = 블록 이름 입력)
;;;  3) PDF 저장 위치 지정 (기본: 도면 이름.pdf), 출력 후 PDF 자동 열기
;;;
;;; 가정 / 주의
;;;  - 모형 탭 전용, 도면 저장 필수, AutoCAD LT 불가 (vla-, .NET 사용)
;;;  - MultiPlotPDF.dll 을 이 파일과 같은 폴더에 둔다 (첫 실행 때 NETLOAD)
;;;
;;; 동작방식
;;;  - 도곽 범위를 읽는 순서(좌->우, 다음 줄)로 정렬해 DLL 의 mpp-plot 에 넘긴다
;;;  - DLL 이 플롯 엔진으로 페이지를 이어 PDF 한 개를 만들고 책갈피를 지운다
;;; ==============================

(vl-load-com)

;;------------------------------
;;  설정
;;------------------------------

;; 대화상자는 매번 이 값으로 연다
(defun mpp:defaults ()
  (setq mpp:pc3 "DWG To PDF.pc3")
  (setq mpp:paper "*FULL_BLEED_A3_(420*")  ; wcmatch 패턴 -> ISO full bleed A3 (420 x 297)
  ;; 마지막으로 고른 용지가 있으면 그것 (AutoCAD 재시작 후에도 유지)
  (if (getenv "MultiPlotPDF_Paper") (setq mpp:paper (getenv "MultiPlotPDF_Paper")))
  (setq mpp:ctb "acad.ctb")                 ; "" = 플롯 스타일 없음, 기본 acad.ctb
  ;; 가장 최근 사용한 CTB가 있으면 그걸로 (NONE = 없음)
  (if (getenv "MultiPlotPDF_Ctb") (setq mpp:ctb (if (= (getenv "MultiPlotPDF_Ctb") "NONE") "" (getenv "MultiPlotPDF_Ctb"))))
  (setq mpp:orient "L")                    ; L 가로, P 세로, AUTO 도곽 모양 기준
  (setq mpp:scale "FIT")                   ; FIT 용지에 맞춤, N = 1 : mpp:scalen
  (setq mpp:scalen "100")
  (setq mpp:lw T)                          ; 선가중치
  (setq mpp:tr T)                          ; 투명도
  (setq mpp:range "ALL")                   ; ALL 같은 이름 전체, SEL 선택한 것만
)
(mpp:defaults)

(setq mpp:none "(없음)")                   ; CTB 목록의 "없음" 항목
(setq mpp:pfx "MPP_")                      ; 대화상자용 임시 페이지 설정 이름
(if (not mpp:last) (setq mpp:last ""))     ; 마지막으로 입력한 블록 이름
;; nil = 이 파일 옆의 MultiPlotPDF.dll 자동 탐색, 또는 전체 경로 지정
(setq mpp:dll nil)
;; 등록 도곽 목록 파일 (없는 폴더는 처음 저장할 때 만든다)
(setq mpp:regfile (strcat (getenv "APPDATA") "\\dodo\\mpp\\blocks.txt"))

;;------------------------------
;;  도곽 수집 / 정렬
;;------------------------------

;; VLA 객체의 범위 -> ((minx miny minz) (maxx maxy maxz))
(defun mpp:bbox (obj / pt-min pt-max)
  (vla-getboundingbox obj 'pt-min 'pt-max)
  (list (vlax-safearray->list pt-min) (vlax-safearray->list pt-max))
)

;; 선택셋 INSERT 들의 블록 이름 (대문자, 동적 블록은 EffectiveName)
(defun mpp:ss-names (ss / i name names)
  (repeat (setq i (sslength ss))
    (setq i (1- i))
    (setq name (strcase (vla-get-effectivename (vlax-ename->vla-object (ssname ss i)))))
    (if (not (member name names)) (setq names (cons name names)))
  )
  names
)

;; 모형 공간에서 이름이 NAMES 에 있는 모든 INSERT -> 범위 목록
(defun mpp:frames-by-names (names / ss i obj res)
  (if (setq ss (ssget "_X" '((0 . "INSERT") (410 . "Model"))))
    (repeat (setq i (sslength ss))
      (setq i (1- i))
      (setq obj (vlax-ename->vla-object (ssname ss i)))
      (if (member (strcase (vla-get-effectivename obj)) names)
        (setq res (cons (mpp:bbox obj) res))
      )
    )
  )
  (mpp:dedupe res)
)

;; 선택한 INSERT 만 (NAMES 가 있으면 그 이름만)
(defun mpp:frames-sel (ss names / i obj res)
  (repeat (setq i (sslength ss))
    (setq i (1- i))
    (setq obj (vlax-ename->vla-object (ssname ss i)))
    (if (or (not names) (member (strcase (vla-get-effectivename obj)) names))
      (setq res (cons (mpp:bbox obj) res))
    )
  )
  (mpp:dedupe res)
)

;; 왼쪽 아래 모서리가 1 이내로 겹치는 도곽은 하나만 남긴다
(defun mpp:dedupe (frames / out)
  (foreach frame frames
    (if (not (vl-some '(lambda (prev)
                         (and (< (abs (- (car (car frame)) (car (car prev)))) 1.0)
                              (< (abs (- (cadr (car frame)) (cadr (car prev)))) 1.0)
                         )
                       )
                      out
             )
        )
      (setq out (cons frame out))
    )
  )
  (reverse out)
)

;; 도곽 하나를 찍어 블록 이름을 얻는다
(defun mpp:pick-name (/ ent)
  (if (and (setq ent (car (entsel "\n【MultiPlotPDF】 도곽 블록 선택: ")))
           (= "INSERT" (cdr (assoc 0 (entget ent))))
      )
    (vla-get-effectivename (vlax-ename->vla-object ent))
  )
)

;; 블록 이름 입력 (Enter = 마지막 이름, 없으면 도곽을 찍음) -> 이름 또는 ""
(defun mpp:ask-name (/ name)
  (setq name (getstring T (strcat "\n【MultiPlotPDF】 도곽 블록 이름 <"
                                  (if (= mpp:last "") "선택" mpp:last)
                                  ">: "
                          )
             )
  )
  (cond
    ((/= name "") (setq mpp:last name))
    ((/= mpp:last "") (setq name mpp:last))
    ((setq name (mpp:pick-name)) (setq mpp:last name))
    (t (setq name ""))
  )
  name
)

(defun mpp:mid-y (frame)
  (/ (+ (cadr (car frame)) (cadr (cadr frame))) 2.0)
)

;; 읽는 순서: 줄 안에서 좌->우, 그다음 아래 줄.
;; 위에서부터 보며 줄 첫 도곽보다 도곽 높이 절반 넘게 내려가면 새 줄로 본다.
(defun mpp:sort (frames / tol by-y rows row row-y out)
  (setq tol (* 0.5 (/ (apply '+ (mapcar '(lambda (frame) (- (cadr (cadr frame)) (cadr (car frame))))
                                        frames
                                )
                      )
                      (length frames)
                   )
            )
  )
  (setq by-y (vl-sort frames '(lambda (a b) (> (mpp:mid-y a) (mpp:mid-y b)))))
  (foreach frame by-y
    (if (and row (<= (- row-y (mpp:mid-y frame)) tol))
      (setq row (cons frame row))
      (progn
        (if row (setq rows (cons row rows)))
        (setq row (list frame))
        (setq row-y (mpp:mid-y frame))
      )
    )
  )
  (if row (setq rows (cons row rows)))
  (foreach row (reverse rows)
    (setq out (append out (vl-sort row '(lambda (a b) (< (car (car a)) (car (car b)))))))
  )
  out
)

;; 등록 이름 -> 선택 -> 이름 입력 순으로 도곽을 모은다
(defun mpp:collect (/ regnames frames ss name)
  (setvar "CTAB" "Model")
  (setq regnames (mapcar '(lambda (rec) (strcase (car rec))) (vl-remove-if-not 'cdr mpp:reg)))
  ;; "선택한 것만" 은 등록 이름이 있어도 항상 선택을 받는다
  (if (and regnames (= mpp:range "ALL"))
    (progn
      (setq frames (mpp:frames-by-names regnames))
      (if (not frames)
        (princ "\n【MultiPlotPDF】 등록된 이름의 블록이 없어 직접 선택합니다.")
      )
    )
  )
  (if (not frames)
    (progn
      (princ "\n【MultiPlotPDF】 ")
      (setq ss (ssget '((0 . "INSERT"))))
      (cond
        ((and ss (= mpp:range "SEL")) (setq frames (mpp:frames-sel ss regnames)))
        (ss (setq frames (mpp:frames-by-names (mpp:ss-names ss))))
        ((/= (setq name (mpp:ask-name)) "")
         (setq frames (mpp:frames-by-names (list (strcase name))))
        )
      )
    )
  )
  (if frames
    (princ (strcat "\n【MultiPlotPDF】 도곽 " (itoa (length frames)) "개"))
  )
  frames
)

;;------------------------------
;;  등록 도곽 목록 (mpp:regfile)
;;  mpp:reg = (("NAME" . 선택여부) ...), 파일 한 줄 "1|NAME" / "0|NAME"
;;------------------------------

;; "a\b\c" -> ("a" "b" "c")
(defun mpp:split (str delim / pos out)
  (while (setq pos (vl-string-search delim str))
    (setq out (cons (substr str 1 pos) out))
    (setq str (substr str (+ pos 2)))
  )
  (reverse (cons str out))
)

;; 없는 상위 폴더까지 모두 만든다
(defun mpp:mkdirs (dir / cur)
  (setq cur "")
  (foreach part (mpp:split dir "\\")
    (setq cur (if (= cur "") part (strcat cur "\\" part)))
    (if (and (not (wcmatch cur "*:")) (not (vl-file-directory-p cur)))
      (vl-mkdir cur)
    )
  )
)

(defun mpp:reg-load (/ file line)
  (setq mpp:reg nil)
  (if (and (findfile mpp:regfile) (setq file (open mpp:regfile "r")))
    (progn
      (while (setq line (read-line file))
        (if (> (strlen line) 2)
          (setq mpp:reg (cons (cons (substr line 3) (= (substr line 1 1) "1")) mpp:reg))
        )
      )
      (close file)
      (setq mpp:reg (reverse mpp:reg))
    )
  )
  mpp:reg
)

(defun mpp:reg-save (/ file)
  (mpp:mkdirs (vl-filename-directory mpp:regfile))
  (if (setq file (open mpp:regfile "w"))
    (progn
      (foreach rec mpp:reg
        (write-line (strcat (if (cdr rec) "1|" "0|") (car rec)) file)
      )
      (close file)
    )
    (princ (strcat "\n【MultiPlotPDF】 오류: 저장할 수 없음 " mpp:regfile))
  )
)

;; 목록 표시, 선택된 이름 = 출력 대상
(defun mpp:fill-blocks (/ i sel)
  (start_list "blk")
  (mapcar 'add_list (mapcar 'car mpp:reg))
  (end_list)
  (setq i 0)
  (setq sel "")
  (foreach rec mpp:reg
    (if (cdr rec) (setq sel (strcat sel (itoa i) " ")))
    (setq i (1+ i))
  )
  (set_tile "blk" sel)
)

;; 목록 선택 상태를 mpp:reg 에 반영
(defun mpp:blk-sync (/ idx i)
  (setq idx (read (strcat "(" (get_tile "blk") ")")))
  (setq i -1)
  (setq mpp:reg (mapcar '(lambda (rec)
                           (setq i (1+ i))
                           (cons (car rec) (if (member i idx) T nil))
                         )
                        mpp:reg
                )
  )
)

;; "삭제": 선택한 이름 제거 (바로 저장)
(defun mpp:blk-del ()
  (mpp:blk-sync)
  (setq mpp:reg (vl-remove-if 'cdr mpp:reg))
  (mpp:reg-save)
  (mpp:fill-blocks)
)

;; "목록 초기화": 전부 제거 (바로 저장)
(defun mpp:blk-clear ()
  (setq mpp:reg nil)
  (mpp:reg-save)
  (mpp:fill-blocks)
)

;; "도면에서 추가" 로 대화상자가 닫힌 뒤: 블록을 찍어 선택 상태로 등록
(defun mpp:pick-add (/ name hit)
  (if (setq name (mpp:pick-name))
    (progn
      (setq hit (vl-some '(lambda (rec) (if (= (strcase (car rec)) (strcase name)) rec)) mpp:reg))
      (if hit
        (setq mpp:reg (subst (cons (car hit) T) hit mpp:reg))
        (setq mpp:reg (append mpp:reg (list (cons name T))))
      )
      (mpp:reg-save)
      (princ (strcat "\n【MultiPlotPDF】 등록: " name))
    )
  )
)

;;------------------------------
;;  대화상자 (DCL 은 TEMP 에 임시로 쓴다)
;;------------------------------

;; DCL 라벨은 \U+ 로 쓴다 (파일 인코딩과 상관없이 한글이 깨지지 않게)
(defun mpp:write-dcl (/ file path)
  (setq path (strcat (getenv "TEMP") "\\mpp_dlg.dcl"))
  (setq file (open path "w"))
  (foreach line '("mpp_dlg : dialog {"
                  "  label = \"MultiPlotPDF\";"
                  "  : popup_list { key = \"dev\"; label = \"\U+D504\U+B9B0\U+D130/\U+D50C\U+B85C\U+D130\"; edit_width = 42; }"
                  "  : popup_list { key = \"paper\"; label = \"\U+C6A9\U+C9C0 \U+D06C\U+AE30\"; edit_width = 42; }"
                  "  : popup_list { key = \"ctb\"; label = \"\U+D50C\U+B86F \U+C2A4\U+D0C0\U+C77C (CTB)\"; edit_width = 42; }"
                  "  : boxed_column {"
                  "    label = \"\U+B3C4\U+BA74 \U+BC29\U+D5A5\";"
                  "    : radio_row {"
                  "      : radio_button { key = \"ol\"; label = \"\U+AC00\U+B85C\"; }"
                  "      : radio_button { key = \"op\"; label = \"\U+C138\U+B85C\"; }"
                  "      : radio_button { key = \"oa\"; label = \"\U+C790\U+B3D9 (\U+B3C4\U+ACFD \U+AE30\U+C900)\"; }"
                  "    }"
                  "    : spacer { height = 0.1; }"
                  "  }"
                  "  : spacer { height = 0.3; }"
                  "  : row {"
                  "    : boxed_column {"
                  "      label = \"\U+CD95\U+CC99\";"
                  "      : row { height = 1.5; fixed_height = true;"
                  "        : radio_row { : radio_button { key = \"sfit\"; label = \"\U+C6A9\U+C9C0\U+C5D0 \U+B9DE\U+CDA4\"; } }"
                  "      }"
                  "      : row { height = 1.5; fixed_height = true; fixed_width = true; alignment = left;"
                  "        : radio_row { fixed_width = true; : radio_button { key = \"sn\"; label = \"\"; fixed_width = true; } }"
                  "        : edit_box { key = \"sval\"; label = \"1 :\"; edit_width = 6; fixed_width = true; }"
                  "      }"
                  "      : spacer { height = 0.1; }"
                  "    }"
                  "    : boxed_column {"
                  "      label = \"\U+D50C\U+B86F \U+C635\U+C158\";"
                  "      : row { height = 1.6; fixed_height = true;"
                  "        : toggle { key = \"lw\"; label = \"\U+C120\U+AC00\U+C911\U+CE58\"; }"
                  "      }"
                  "      : row { height = 1.5; fixed_height = true;"
                  "        : toggle { key = \"tr\"; label = \"\U+D22C\U+BA85\U+B3C4\"; }"
                  "      }"
                  "      : spacer { height = 0.1; }"
                  "    }"
                  "  }"
                  "  : spacer { height = 0.3; }"
                  "  : boxed_column {"
                  "    label = \"\U+B3C4\U+ACFD \U+BE14\U+B85D (\U+C120\U+D0DD = \U+CD9C\U+B825)\";"
                  "    : boxed_column {"
                  "      label = \"\U+CD9C\U+B825 \U+BC94\U+C704\";"
                  "      : radio_row {"
                  "        : radio_button { key = \"rall\"; label = \"\U+AC19\U+C740 \U+C774\U+B984 \U+C804\U+CCB4\"; }"
                  "        : radio_button { key = \"rsel\"; label = \"\U+C120\U+D0DD\U+D55C \U+AC83\U+B9CC\"; }"
                  "      }"
                  "      : spacer { height = 0.1; }"
                  "    }"
                  "    : list_box { key = \"blk\"; multiple_select = true; height = 6; width = 42; }"
                  "    : row {"
                  "      : button { key = \"blkadd\"; label = \"\U+B3C4\U+BA74\U+C5D0\U+C11C \U+CD94\U+AC00\"; }"
                  "      : button { key = \"blkdel\"; label = \"\U+C0AD\U+C81C\"; }"
                  "      : button { key = \"blkclr\"; label = \"\U+BAA9\U+B85D \U+CD08\U+AE30\U+D654\"; }"
                  "    }"
                  "    : spacer { height = 0.1; }"
                  "  }"
                  "  ok_cancel;"
                  "}"
                 )
    (write-line line file)
  )
  (close file)
  path
)

;; ITEM 의 위치 (대소문자 무시), 없으면 0
(defun mpp:idx (item lst / i found)
  (setq i 0)
  (while (and lst (not found))
    (if (= (strcase (car lst)) (strcase item)) (setq found i))
    (setq i (1+ i))
    (setq lst (cdr lst))
  )
  (if found found 0)  ; (or found 0) 은 T 를 돌려줘서 안 됨
)

;; CTB 목록에서 이름 찾기 (대소문자 무시), 없으면 nil
(defun mpp:ctb-find (name)
  (vl-some '(lambda (item) (if (= (strcase item) (strcase name)) item)) mpp:ctbs)
)

;; 사용할 CTB 결정: 최근 사용 > acad.ctb > 없음(목록에 없으면 다음 순서로)
(defun mpp:ctb-resolve ()
  (setq mpp:ctb (cond
                  ((= mpp:ctb "") "")
                  ((mpp:ctb-find mpp:ctb))
                  ((mpp:ctb-find "acad.ctb"))
                  (t "")
                )
  )
)

;; 프린터 DEV 의 용지 목록 -> mpp:media = ((표시이름 . 표준이름) ...)
;; COM 이 느려서 대화상자 열기 전, 프린터를 바꿨을 때만 읽는다
(defun mpp:load-media (dev / canon local-name)
  (setq mpp:media nil)
  (vl-catch-all-apply 'vla-put-configname (list mpp:cfg dev))
  (vl-catch-all-apply 'vla-refreshplotdeviceinfo (list mpp:cfg))
  (setq canon (vl-catch-all-apply
                '(lambda ()
                   (vlax-safearray->list (vlax-variant-value (vla-getcanonicalmedianames mpp:cfg)))
                 )
                nil
              )
  )
  (if (vl-catch-all-error-p canon) (setq canon nil))
  (foreach name canon
    ;; GetLocaleMediaName 은 인덱스가 아니라 표준 이름을 받는다
    (setq local-name (vl-catch-all-apply 'vla-getlocalemedianame (list mpp:cfg name)))
    (if (vl-catch-all-error-p local-name) (setq local-name name))
    (setq mpp:media (cons (cons local-name name) mpp:media))
  )
  (setq mpp:media (reverse mpp:media))
)

;; 표준 이름이 mpp:paper 패턴에 맞는 첫 용지 위치 (없으면 0)
(defun mpp:paper-idx (pairs / i found)
  (setq i 0)
  (foreach pair pairs
    (if (and (not found) (wcmatch (strcase (cdr pair)) (strcase mpp:paper))) (setq found i))
    (setq i (1+ i))
  )
  (if found found 0)
)

;; 읽어 둔 용지 목록 표시 (여기서는 COM 호출 없음)
(defun mpp:fill-papers ()
  (setq mpp:pairs mpp:media)
  (start_list "paper")
  (if mpp:pairs
    (mapcar 'add_list (mapcar 'car mpp:pairs))
    (add_list "(용지 없음)")
  )
  (end_list)
  (set_tile "paper" (itoa (mpp:paper-idx mpp:pairs)))
)

;; 대화상자 값을 전역에 저장 (확인, "도면에서 추가" 전)
(defun mpp:dlg-store (/ pick)
  (mpp:blk-sync)
  (setq mpp:pc3 mpp:dev)
  (setq pick (nth (atoi (get_tile "paper")) mpp:pairs))
  (if (and mpp:pairs pick) (setq mpp:paper (strcase (cdr pick))))
  (setq pick (nth (atoi (get_tile "ctb")) mpp:ctbs))
  (setq mpp:ctb (if (or (not pick) (= pick mpp:none)) "" pick))
  (setq mpp:orient (cond
                     ((= (get_tile "ol") "1") "L")
                     ((= (get_tile "op") "1") "P")
                     (t "AUTO")
                   )
  )
  (setq mpp:scale (if (= (get_tile "sn") "1") "N" "FIT"))
  (setq mpp:scalen (get_tile "sval"))
  (setq mpp:lw (= (get_tile "lw") "1"))
  (setq mpp:tr (= (get_tile "tr") "1"))
  (setq mpp:range (if (= (get_tile "rsel") "1") "SEL" "ALL"))
)

;; 확인: 1:N 값 검사 후 저장하고 닫는다
(defun mpp:dlg-ok (/ val)
  (if (and (= (get_tile "sn") "1")
           (not (and (setq val (distof (get_tile "sval"))) (> val 0)))
      )
    (alert "축척 값을 확인하세요 (1 : N)")
    (progn
      (mpp:dlg-store)
      (mpp:reg-save)
      (setenv "MultiPlotPDF_Paper" mpp:paper)
      (setenv "MultiPlotPDF_Ctb" (if (= mpp:ctb "") "NONE" mpp:ctb))
      (done_dialog 1)
    )
  )
)

;; 전역 값으로 타일을 채우고 동작을 연결한다
(defun mpp:dlg-init ()
  (start_list "dev")
  (mapcar 'add_list mpp:devs)
  (end_list)
  (set_tile "dev" (itoa (mpp:idx mpp:dev mpp:devs)))
  (start_list "ctb")
  (mapcar 'add_list mpp:ctbs)
  (end_list)
  (set_tile "ctb" (itoa (mpp:idx (if (= mpp:ctb "") mpp:none mpp:ctb) mpp:ctbs)))
  (mpp:fill-papers)
  (set_tile (cond
              ((= mpp:orient "L") "ol")
              ((= mpp:orient "P") "op")
              (t "oa")
            )
            "1"
  )
  ;; 축척 버튼 둘은 1:N 칸을 옆에 두려고 다른 radio_row 에 있어서 직접 배타 처리
  (set_tile "sfit" (if (= mpp:scale "N") "0" "1"))
  (set_tile "sn" (if (= mpp:scale "N") "1" "0"))
  (set_tile "sval" mpp:scalen)
  (mode_tile "sval" (if (= mpp:scale "N") 0 1))
  (set_tile "lw" (if mpp:lw "1" "0"))
  (set_tile "tr" (if mpp:tr "1" "0"))
  (set_tile (if (= mpp:range "SEL") "rsel" "rall") "1")
  (mpp:fill-blocks)
  (action_tile "sfit" "(set_tile \"sfit\" \"1\") (set_tile \"sn\" \"0\") (mode_tile \"sval\" 1)")
  (action_tile "sn" "(set_tile \"sn\" \"1\") (set_tile \"sfit\" \"0\") (mode_tile \"sval\" 0) (mode_tile \"sval\" 2)")
  (action_tile "dev" "(setq mpp:dev (nth (atoi $value) mpp:devs)) (mpp:load-media mpp:dev) (mpp:fill-papers)")
  (action_tile "blkadd" "(mpp:dlg-store) (done_dialog 2)")
  (action_tile "blkdel" "(mpp:blk-del)")
  (action_tile "blkclr" "(mpp:blk-clear)")
  (action_tile "accept" "(mpp:dlg-ok)")
)

;; 확인(또는 대화상자를 못 열 때) T, 취소 nil
(defun mpp:dialog (doc / dcl-file dcl-id res state)
  (mpp:defaults)
  (vl-catch-all-apply 'mpp:clean (list doc))
  ;; 장치, CTB, 용지 목록은 임시 페이지 설정에서 읽고 끝나면 지운다
  (setq mpp:cfg (vla-add (vla-get-plotconfigurations doc) (strcat mpp:pfx "000") :vlax-true))
  (setq mpp:devs (vlax-safearray->list (vlax-variant-value (vla-getplotdevicenames mpp:cfg))))
  (setq mpp:dev (nth (mpp:idx mpp:pc3 mpp:devs) mpp:devs))
  (setq mpp:ctbs (cons mpp:none
                       (vl-remove-if-not
                         '(lambda (name) (wcmatch (strcase name) "*.CTB"))
                         (vlax-safearray->list
                           (vlax-variant-value (vla-getplotstyletablenames mpp:cfg))
                         )
                       )
                 )
  )
  (mpp:ctb-resolve)
  (mpp:load-media mpp:dev)
  (setq dcl-file (mpp:write-dcl))
  (setq dcl-id (load_dialog dcl-file))
  (mpp:reg-load)
  ;; state 2 = "도면에서 추가" 로 닫힘: 블록을 찍고 다시 연다
  (setq state 2)
  (while (= state 2)
    (cond
      ((not (new_dialog "mpp_dlg" dcl-id))
       (princ "\n【MultiPlotPDF】 대화상자를 열 수 없어 기본 설정으로 진행합니다.")
       (setq res T)
       (setq state 0)
      )
      (t
       (mpp:dlg-init)
       (setq state (start_dialog))
       (setq res (= state 1))
       (if (= state 2) (mpp:pick-add))
      )
    )
  )
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file)
  (vl-catch-all-apply 'vla-delete (list mpp:cfg))
  res
)

;; 대화상자 오류는 알리고 기본 설정으로 계속한다
(defun mpp:dialog-safe (doc / res)
  (setq res (vl-catch-all-apply 'mpp:dialog (list doc)))
  (cond
    ((vl-catch-all-error-p res)
     (princ (strcat "\n【MultiPlotPDF】 대화상자 오류: " (vl-catch-all-error-message res)
                    " - 기본 설정으로 진행합니다."
            )
     )
     (vl-catch-all-apply 'vla-delete (list mpp:cfg))
     T
    )
    (t res)
  )
)

;; 남은 임시 페이지 설정(MPP_###) 삭제
(defun mpp:clean (doc / cfgs names)
  (setq cfgs (vla-get-plotconfigurations doc))
  (vlax-for cfg cfgs
    (if (wcmatch (strcase (vla-get-name cfg)) (strcat (strcase mpp:pfx) "###"))
      (setq names (cons (vla-get-name cfg) names))
    )
  )
  (foreach name names
    (vl-catch-all-apply 'vla-delete (list (vla-item cfgs name)))
  )
)

;;------------------------------
;;  플롯 엔진 (MultiPlotPDF.dll)
;;------------------------------

;; APPLOAD 시작하기 세트/기록에 있는 MultiPlotPDF.lsp 의 폴더들
(defun mpp:appload-dirs (key / dirs entry n i value)
  (foreach entry '(("\\Startup" "NumStartup" "Startup") ("\\History" "NumHistory" "History"))
    (setq n (atoi (vl-princ-to-string (vl-registry-read (strcat key (car entry)) (cadr entry)))))
    (setq i 0)
    (while (< i n)
      (setq i (1+ i))
      (setq value (vl-registry-read (strcat key (car entry)) (strcat (itoa i) (caddr entry))))
      (if (and (= (type value) 'STR)
               (wcmatch (strcase value) "MULTIPLOTPDF.LSP,*\\MULTIPLOTPDF.LSP")
          )
        (setq dirs (cons (vl-filename-directory value) dirs))
      )
    )
  )
  (reverse dirs)
)

;; DLL 위치: mpp:dll 지정 경로, 아니면 이 파일 옆.
;; 폴더 후보: 지원 경로 -> APPLOAD 시작하기/기록 -> APPLOAD 마지막 폴더 -> Documents\LISP
(defun mpp:dll-path (/ key dirs path value)
  (if mpp:dll
    (findfile mpp:dll)
    (progn
      (if (setq path (findfile "MultiPlotPDF.lsp"))
        (setq dirs (list (vl-filename-directory path)))
      )
      (setq key (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles\\"
                        (getvar "CPROFILE") "\\Dialogs\\Appload"
                )
      )
      (setq dirs (append dirs (mpp:appload-dirs key)))
      (foreach name '("MainDialog" "AddAppDialog")
        (setq value (vl-registry-read key name))
        (if (= (type value) 'STR)
          (setq dirs (append dirs (list (vl-string-right-trim "\\" value))))
        )
      )
      (setq dirs (append dirs (list (strcat (getenv "USERPROFILE") "\\Documents\\LISP"))))
      (vl-some '(lambda (dir) (findfile (strcat dir "\\MultiPlotPDF.dll"))) dirs)
    )
  )
)

;; mpp-plot 이 없으면 DLL 을 NETLOAD (세션당 한 번). 준비되면 nil, 아니면 이유
(defun mpp:engine-ready (/ path)
  (cond
    ((vl-symbol-value 'mpp-plot) nil)
    ((not (setq path (mpp:dll-path)))
     "MultiPlotPDF.dll 없음 - MultiPlotPDF.lsp 와 같은 폴더에 두세요"
    )
    (t
     (command "_.NETLOAD" path)
     (if (vl-symbol-value 'mpp-plot)
       nil
       (strcat "MultiPlotPDF.dll 을 불러오지 못함 (보안 경고에서 거부?): " path)
     )
    )
  )
)

;; 도곽을 PDF 한 개로 출력 -> T 또는 오류 문자열
(defun mpp:plot (frames out / res)
  (setq res (mpp:engine-ready))
  (if (not res)
    (setq res (vl-catch-all-apply
                'mpp-plot
                (list out mpp:pc3 mpp:paper mpp:ctb mpp:orient
                      (if (= mpp:scale "N") (distof mpp:scalen) 0.0)
                      mpp:lw
                      (mapcar '(lambda (frame)
                                 (list (float (car (car frame))) (float (cadr (car frame)))
                                       (float (car (cadr frame))) (float (cadr (cadr frame)))
                                 )
                               )
                              frames
                      )
                )
              )
    )
  )
  (cond
    ((= res T) T)
    ((vl-catch-all-error-p res) (vl-catch-all-error-message res))
    (t (vl-princ-to-string res))
  )
)

;; 플롯할 때마다 도면 폴더와 PDF 폴더에 쌓이는 plot.log 삭제
(defun mpp:del-plotlog (out / log)
  (foreach dir (list (getvar "DWGPREFIX") (strcat (vl-filename-directory out) "\\"))
    (setq log (strcat dir "plot.log"))
    (if (findfile log) (vl-file-delete log))
  )
)

;;------------------------------
;;  명령
;;------------------------------

(defun c:PPP (/ *error* doc frames out res time-start
                cmdecho-orig filedia-orig plottrans-orig
             )
  (defun *error* (msg)
    (if cmdecho-orig (setvar "CMDECHO" cmdecho-orig))
    (if filedia-orig (setvar "FILEDIA" filedia-orig))
    (if plottrans-orig (setvar "PLOTTRANSPARENCYOVERRIDE" plottrans-orig))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*")))
      (princ (strcat "\n【MultiPlotPDF】 오류: " msg))
    )
    (princ)
  )
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (cond
    ((/= 1 (getvar "DWGTITLED"))
     (alert "【MultiPlotPDF】 도면을 먼저 저장하세요.")
    )
    ((not (mpp:dialog-safe doc))
     (princ "\n【MultiPlotPDF】 취소")
    )
    ((not (setq frames (mpp:collect)))
     (princ "\n【MultiPlotPDF】 도곽 블록이 없습니다.")
    )
    ((not (setq out (getfiled "PDF 저장"
                              (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) ".pdf")
                              "pdf"
                              1
                    )
          )
     )
     (princ "\n【MultiPlotPDF】 취소")
    )
    (t
     (setq cmdecho-orig (getvar "CMDECHO"))
     (setq filedia-orig (getvar "FILEDIA"))
     (setq plottrans-orig (getvar "PLOTTRANSPARENCYOVERRIDE"))
     (setvar "CMDECHO" 0)
     (setvar "FILEDIA" 0)  ; NETLOAD 가 파일 대화상자를 띄우지 않게
     (setvar "PLOTTRANSPARENCYOVERRIDE" (if mpp:tr 2 0))  ; 2 항상 투명도, 0 안 함
     (setq time-start (getvar "MILLISECS"))
     (setq res (mpp:plot (mpp:sort frames) out))
     (*error* nil)
     (mpp:del-plotlog out)
     (cond
       ((= res T)
        (princ (strcat "\n【MultiPlotPDF】 완료: " out))
        (princ (strcat "\n【MultiPlotPDF】 Time: " (itoa (- (getvar "MILLISECS") time-start)) " ms"))
        (startapp "explorer.exe" (strcat "\"" out "\""))
       )
       (t (princ (strcat "\n【MultiPlotPDF】 오류: " res)))
     )
    )
  )
  (princ)
)

(princ)
