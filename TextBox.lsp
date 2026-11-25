;;; ==============================
;;; TextBox.LSP
;;;  - TEXT / MTEXT 연동(Associative) 박스 + MText 배경마스크
;;;
;;; 원본파일: Lee Mac - Associative Textbox (tbox / rtbox) www.lee-mac.com
;;;
;;; 명령어: TB, TBR
;;;
;;; 사용법
;;;  1) TB 실행 (객체를 미리 선택해 두어도 됨)
;;;  2) 오프셋 계수 / 박스 모양 [Circle/Rectangle] / 배경 마스크 [On/Off] 입력
;;;  3) 미리 선택하지 않았다면 TEXT/MTEXT 다중 선택 후 Enter
;;;  4) TBR : 글자 또는 박스를 선택해 연동 해제
;;;
;;; 가정 / 주의
;;;  - 박스 여백 = (계수-1) * tbox:k * 글자높이, 마스크와 크기가 다르면 tbox:k / tbox:kx 조정
;;;  - 마스크 On 이면 TEXT 를 MTEXT 로 변환 (폭비율, 기울기, %%u %%o 는 유지되지 않음)
;;;  - vla- 함수 사용으로 AutoCAD LT 에서는 불가
;;;
;;; 동작방식
;;;  - 글자의 xData(dodo_TB)에 박스 핸들을 저장하고 객체 리액터로 이동/수정 시 박스를 갱신
;;;  - 박스를 수정하면 글자 기준으로 복원, 글자를 복사하면 박스도 복사 (박스만 복사하면 삭제)
;;;  - 로드 시 도면의 xData 를 찾아 리액터를 복원
;;; ==============================

(vl-load-com)

;;------------------------------
;;  설정
;;------------------------------

(setq tbox:app "dodo_TB")   ; AppID
(setq tbox:k   1.0)         ; 마스크-박스 크기 보정 계수 (상하좌우 공통)
(setq tbox:kx  0.13)        ; 마스크 On 일 때 좌우에만 더하는 여백 (글자높이 배수)
(setq tbox:f   1.3)         ; 오프셋 계수 기본값
(setq tbox:typ "Rectangle") ; 박스 모양 기본값
(setq tbox:msk "On")        ; 배경 마스크 기본값

;;------------------------------
;;  보조 함수
;;------------------------------

;; 선택셋을 ename 리스트로 복사 (command 실행 시 선택셋이 해제되므로)
(defun tbox:sslist (sel / i lst)
  (if sel
    (repeat (setq i (sslength sel))
      (setq lst (cons (ssname sel (setq i (1- i))) lst))
    )
  )
  lst
)

(defun tbox:replace (old new str / pos)
  (setq pos 0)
  (while (setq pos (vl-string-search old str pos))
    (setq str (vl-string-subst new old str pos))
    (setq pos (+ pos (strlen new)))
  )
  str
)

;; TEXT 문자열을 MTEXT 문자열로 (특수문자 코드와 서식 기호 처리)
(defun tbox:mtextstr (str / pr)
  (setq str (tbox:replace "\\" "\\\\" str))
  (setq str (tbox:replace "{" "\\{" str))
  (setq str (tbox:replace "}" "\\}" str))
  (foreach pr
   '(("%%d" . "\\U+00B0") ("%%D" . "\\U+00B0")
     ("%%p" . "\\U+00B1") ("%%P" . "\\U+00B1")
     ("%%c" . "\\U+00D8") ("%%C" . "\\U+00D8")
    )
    (setq str (tbox:replace (car pr) (cdr pr) str))
  )
  str
)

;; TEXT 를 같은 위치/높이/회전/스타일의 MTEXT 로 바꾸고 새 ename 반환
(defun tbox:text2mtext (ent / ang col edata hjust new-ent normal pt row vjust)
  (setq edata (entget ent))
  (setq hjust (cond ((cdr (assoc 72 edata))) (0)))
  (setq vjust (cond ((cdr (assoc 73 edata))) (0)))
  (if (= hjust 4) (setq vjust 2))
  (setq normal (cdr (assoc 210 edata)))
  (setq ang (cdr (assoc 50 edata)))
  (setq pt
    (if (and (or (/= hjust 0) (/= vjust 0)) (not (member hjust '(3 5))))
      (cdr (assoc 11 edata))
      (cdr (assoc 10 edata))
    )
  )
  (setq col (cond ((member hjust '(1 4)) 1) ((= hjust 2) 2) (0)))
  (setq row (cond ((= vjust 3) 0) ((= vjust 2) 1) (2)))
  (setq new-ent
    (entmakex
      (append
        (list '(0 . "MTEXT") '(100 . "AcDbEntity"))
        (tbox:props edata)
        (list
         '(100 . "AcDbMText")
          (cons 10 (trans pt normal 0))
          (cons 40 (cdr (assoc 40 edata)))
         '(41 . 0.0)
          (cons 71 (+ 1 col (* 3 row)))
         '(72 . 1)
          (cons 1 (tbox:mtextstr (cdr (assoc 1 edata))))
        )
        (if (assoc 7 edata) (list (assoc 7 edata)))
        (list
          (cons 210 normal)
          (cons 11 (trans (list (cos ang) (sin ang) 0.0) normal 0 T))
        )
      )
    )
  )
  (if new-ent
    (progn
      (entdel ent)
      (vl-catch-all-apply 'vla-update (list (vlax-ename->vla-object new-ent)))
      new-ent
    )
    ent
  )
)

;; 마스크 On 이면 TEXT 를 MTEXT 로 변환, 아니면 그대로 반환
(defun tbox:convert (ent)
  (if (and (= "TEXT" (cdr (assoc 0 (entget ent)))) (/= "Off" tbox:msk))
    (tbox:text2mtext ent)
    ent
  )
)

;; 객체를 맨 앞으로 (command 호출 없이 ActiveX 사용)
(defun tbox:tofront (ent / blk dic tbl)
  (setq blk (vla-get-block (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object)))))
  (setq dic (vla-getextensiondictionary blk))
  (setq tbl (vl-catch-all-apply 'vla-item (list dic "ACAD_SORTENTS")))
  (if (vl-catch-all-error-p tbl)
    (setq tbl (vla-addobject dic "ACAD_SORTENTS" "AcDbSortentsTable"))
  )
  (vla-movetotop tbl
    (vlax-make-variant
      (vlax-safearray-fill
        (vlax-make-safearray vlax-vbobject '(0 . 0))
        (list (vlax-ename->vla-object ent))
      )
    )
  )
)

(defun tbox:mask (ent fill-on factor / edata)
  (vla-put-BackgroundFill (vlax-ename->vla-object ent) (if fill-on :vlax-true :vlax-false))
  (if fill-on
    (progn
      (setq edata (entget ent))
      (entmod (subst (cons 45 factor) (assoc 45 edata) edata))
    )
  )
)

;; 박스 여백(절대값) = (계수 - 1) * tbox:k * 글자높이
(defun tbox:off (edata factor)
  (* (- factor 1.0) tbox:k (cdr (assoc 40 edata)))
)

;; 박스에 복사할 도면층/색상 (글자와 동일)
(defun tbox:props (edata)
  (vl-remove nil
    (list (assoc 8 edata) (assoc 62 edata) (assoc 420 edata) (assoc 430 edata))
  )
)

(defun tbox:mid (pt1 pt2)
  (mapcar (function (lambda (val1 val2) (/ (+ val1 val2) 2.0))) pt1 pt2)
)

;; Matrix x Vector - Vladimir Nesterovsky
(defun tbox:mxv (mat vec)
  (mapcar '(lambda (row) (apply '+ (mapcar '* row vec))) mat)
)

;; Active Document - Lee Mac
(defun tbox:acdoc nil
  (eval (list 'defun 'tbox:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
  (tbox:acdoc)
)

(defun tbox:unlocklayers (/ lay lck)
  (vlax-for lay (vla-get-layers (tbox:acdoc))
    (if (= :vlax-true (vla-get-lock lay))
      (progn
        (vla-put-lock lay :vlax-false)
        (setq lck (cons lay lck))
      )
    )
  )
  lck
)

(defun tbox:mate (ent / xd)
  (if (setq xd (cdadr (assoc -3 (entget ent (list tbox:app)))))
    (handent (cdr (assoc 1005 xd)))
  )
)

;; 짝(박스<->글자)이 서로를 가리키는지 확인 (지워진 짝은 undo 대비 유효로 간주)
(defun tbox:valid-p (ent / ed me mate)
  (and
    (setq ed (entget ent (list tbox:app)))
    (assoc -3 ed)
    (setq mate (tbox:mate ent))
    (or
      (vl-catch-all-error-p (vl-catch-all-apply 'vlax-ename->vla-object (list mate)))
      (vlax-erased-p (vlax-ename->vla-object mate))
      (and
        (setq me (entget mate (list tbox:app)))
        (assoc -3 me)
        (= (cdr (assoc 1005 (cdadr (assoc -3 me)))) (cdr (assoc 5 ed)))
      )
    )
  )
)

(defun tbox:removexdata (ent / edata)
  (if (member (cdr (assoc 0 (setq edata (entget ent)))) '("TEXT" "MTEXT"))
    (entmod (append (vl-remove (assoc 40 edata) edata) (list (list -3 (list tbox:app)))))
    (entmod (append edata (list (list -3 (list tbox:app)))))
  )
)

(defun tbox:removeowner (obj)
  (if (wcmatch (vla-get-objectname obj) "AcDb*Text")
    (if (= 'vlr-object-reactor (type tbox:textreactor))
      (vlr-owner-remove tbox:textreactor obj)
    )
    (if (= 'vlr-object-reactor (type tbox:boxreactor))
      (vlr-owner-remove tbox:boxreactor obj)
    )
  )
)

;; 글자 외곽 4점 (gile 코드 기반, Lee Mac 사용)
(defun tbox:textbox (edata off / base height just normal off-x org pts rot tb width)
  (if
    (setq pts
      (cond
        ((= "TEXT" (cdr (assoc 0 edata)))
          (setq base (cdr (assoc 10 edata)))
          (setq rot  (cdr (assoc 50 edata)))
          (setq tb   (textbox edata))
          (list
            (list (- (caar  tb) off) (- (cadar  tb) off))
            (list (+ (caadr tb) off) (- (cadar  tb) off))
            (list (+ (caadr tb) off) (+ (cadadr tb) off))
            (list (- (caar  tb) off) (+ (cadadr tb) off))
          )
        )
        ((= "MTEXT" (cdr (assoc 0 edata)))
          (setq normal (cdr (assoc 210 edata)))
          (setq base   (trans (cdr (assoc 10 edata)) 0 normal))
          (setq rot    (angle '(0.0 0.0 0.0) (trans (cdr (assoc 11 edata)) 0 normal)))
          (setq width  (cdr (assoc 42 edata)))
          (setq height (cdr (assoc 43 edata)))
          (setq just   (cdr (assoc 71 edata)))
          (setq org
            (list
              (cond
                ((member just '(2 5 8)) (/ width -2.0))
                ((member just '(3 6 9)) (- width))
                (0.0)
              )
              (cond
                ((member just '(1 2 3)) (- height))
                ((member just '(4 5 6)) (/ height -2.0))
                (0.0)
              )
            )
          )
          (setq off-x
            (if (and (assoc 90 edata) (/= 0 (cdr (assoc 90 edata))))
              (+ off (* tbox:kx (cdr (assoc 40 edata))))
              off
            )
          )
          (list
            (list (- (car org)       off-x) (- (cadr org)        off))
            (list (+ (car org) width off-x) (- (cadr org)        off))
            (list (+ (car org) width off-x) (+ (cadr org) height off))
            (list (- (car org)       off-x) (+ (cadr org) height off))
          )
        )
      )
    )
    ((lambda (mat) (mapcar '(lambda (pt) (mapcar '+ (tbox:mxv mat pt) base)) pts))
      (list
        (list (cos rot) (sin (- rot)) 0.0)
        (list (sin rot) (cos rot)     0.0)
       '(0.0 0.0 1.0)
      )
    )
  )
)

(defun tbox:createbox (edata typ factor / pts)
  (if (setq pts (tbox:textbox edata (tbox:off edata factor)))
    (if (= "CIRCLE" typ)
      (entmakex
        (append
          (list '(0 . "CIRCLE"))
          (tbox:props edata)
          (list
            (cons 10 (tbox:mid (car pts) (caddr pts)))
            (cons 40 (cdr (assoc 40 edata)))
            (assoc 210 edata)
            (list -3
              (list tbox:app
               '(1002 . "{")
                (cons 1005 (cdr (assoc 5 edata)))
               '(1000 . "CIRCLE")
                (cons 1040 factor)
               '(1002 . "}")
              )
            )
          )
        )
      )
      (entmakex
        (append
         '(
            (0 . "LWPOLYLINE")
            (100 . "AcDbEntity")
           )
          (tbox:props edata)
          (list
           '(100 . "AcDbPolyline")
           '(90 . 4)
           '(70 . 1)
          )
          (list (cons 38 (caddar pts)))
          (mapcar '(lambda (pt) (list 10 (car pt) (cadr pt))) pts)
          (list (assoc 210 edata)
            (list -3
              (list tbox:app
               '(1002 . "{")
                (cons 1005 (cdr (assoc 5 edata)))
                (cons 1000 typ)
                (cons 1040 factor)
               '(1002 . "}")
              )
            )
          )
        )
      )
    )
  )
)

(defun tbox:updatebox (edata typ factor box / pts)
  (if (= 'vlr-object-reactor (type tbox:boxreactor))
    (vlr-remove tbox:boxreactor)
  )
  (if (setq pts (tbox:textbox edata (tbox:off edata factor)))
    (if (= "CIRCLE" typ)
      (entmod
        (subst
          (cons 10 (tbox:mid (car pts) (caddr pts)))
          (assoc 10 box)
          (subst
            (cons 40 (cdr (assoc 40 edata)))
            (assoc 40 box)
            (subst
              (assoc 210 edata)
              (assoc 210 box)
              box
            )
          )
        )
      )
      (entmod
        (append
          (subst
            (cons 38 (caddar pts))
            (assoc 38 box)
            (reverse (member (assoc 38 box) (reverse box)))
          )
          (mapcar '(lambda (pt) (list 10 (car pt) (cadr pt))) pts)
          (list (assoc 210 edata))
        )
      )
    )
  )
  (if (= 'vlr-object-reactor (type tbox:boxreactor))
    (vlr-add tbox:boxreactor)
  )
)

;;------------------------------
;;  리액터
;;------------------------------

;; 글자 객체 리액터 등록 (이미 있으면 대상만 추가)
(defun tbox:addtext (objs / obj)
  (if (= 'vlr-object-reactor (type tbox:textreactor))
    (foreach obj objs (vlr-owner-add tbox:textreactor obj))
    (vlr-set-notification
      (setq tbox:textreactor
        (vlr-object-reactor objs "tbox-textreactor"
         '(
            (:vlr-modified . tbox:textcallback)
            (:vlr-copied   . tbox:textcopied)
          )
        )
      )
      'active-document-only
    )
  )
)

;; 박스 객체 리액터 등록 (이미 있으면 대상만 추가)
(defun tbox:addbox (objs / obj)
  (if (= 'vlr-object-reactor (type tbox:boxreactor))
    (foreach obj objs (vlr-owner-add tbox:boxreactor obj))
    (vlr-set-notification
      (setq tbox:boxreactor
        (vlr-object-reactor objs "tbox-boxreactor"
         '(
            (:vlr-modified . tbox:boxcallback)
            (:vlr-copied   . tbox:boxcopied)
          )
        )
      )
      'active-document-only
    )
  )
)

(defun tbox:textcallback (owner reactor params / edata ent xd-type xdata)
  (if
    (and
      (vlax-read-enabled-p owner)
      (progn (vla-getxdata owner tbox:app 'xd-type 'xdata) xdata)
      (setq xdata (mapcar 'cons (vlax-safearray->list xd-type) (mapcar 'vlax-variant-value (vlax-safearray->list xdata))))
      (setq ent (handent (cdr (assoc 1005 xdata))))
      (setq edata (entget ent))
      (vlax-write-enabled-p (vlax-ename->vla-object ent))
    )
    (if (= "AcDbMText" (vla-get-objectname owner)) ;; MText DXF 는 mtedit 직후 바로 갱신되지 않음
      (tbox:boxcallback (vlax-ename->vla-object ent) nil nil)
      (tbox:updatebox (entget (vlax-vla-object->ename owner)) (cdr (assoc 1000 xdata)) (cdr (assoc 1040 xdata)) edata)
    )
  )
  (princ)
)

(defun tbox:textcopied (owner reactor params)
  (if (/= 0 (car params))
    (progn
      (setq tbox:owner (append tbox:owner (list (car params))))
      (vlr-command-reactor "tbox-textcopiedcommreactor"
       '(
          (:vlr-commandended     . tbox:textcopiedcommandended)
          (:vlr-commandcancelled . tbox:textcopiedcommandcancelled)
          (:vlr-commandfailed    . tbox:textcopiedcommandcancelled)
        )
      )
    )
  )
  (princ)
)

(defun tbox:textcopiedcommandended (reactor params / box edata ent xdata)
  (vlr-remove reactor)
  (if
    (and
      (setq ent (car tbox:owner))
      (setq edata (entget ent (list tbox:app)))
      (member (cdr (assoc 0 edata)) '("TEXT" "MTEXT"))
      (setq xdata (cdadr (assoc -3 edata)))
      (setq box (tbox:createbox edata (cdr (assoc 1000 xdata)) (cdr (assoc 1040 xdata))))
    )
    (progn
      (vl-catch-all-apply 'tbox:tofront (list box))
      (entmod
        (append (vl-remove (assoc 40 edata) (entget ent))
          (list
            (list -3
              (list tbox:app
               '(1002 . "{")
                (cons 1005 (cdr (assoc 5 (entget box))))
                (assoc 1000 xdata)
                (assoc 1040 xdata)
               '(1002 . "}")
              )
            )
          )
        )
      )
      (if (= 'vlr-object-reactor (type tbox:textreactor))
        (vlr-owner-add tbox:textreactor (vlax-ename->vla-object ent))
      )
      (if (= 'vlr-object-reactor (type tbox:boxreactor))
        (vlr-owner-add tbox:boxreactor (vlax-ename->vla-object box))
      )
    )
    ;; 실패 시 복사본이 원본 박스를 공유하지 않도록 xData 제거
    (if (and (= 'ename (type ent)) (entget ent) (assoc -3 (entget ent (list tbox:app))))
      (vl-catch-all-apply 'tbox:removexdata (list ent))
    )
  )
  (setq tbox:owner (cdr tbox:owner))
  (princ)
)

(defun tbox:textcopiedcommandcancelled (reactor params)
  (vlr-remove reactor)
  (setq tbox:owner nil)
  (princ)
)

(defun tbox:boxcallback (owner reactor params)
  (setq tbox:owner (cons owner tbox:owner))
  (vlr-command-reactor "tbox-commreactor"
   '(
      (:vlr-commandended     . tbox:commandended)
      (:vlr-commandcancelled . tbox:commandcancelled)
      (:vlr-commandfailed    . tbox:commandcancelled)
    )
  )
  (princ)
)

(defun tbox:commandended (reactor params / box txt xd-type xdata)
  (vlr-remove reactor)
  (if
    (and
      (setq box (car tbox:owner))
      (vlax-read-enabled-p  box)
      (vlax-write-enabled-p box)
      (progn (vla-getxdata box tbox:app 'xd-type 'xdata) xdata)
      (setq xdata (mapcar 'cons (vlax-safearray->list xd-type) (mapcar 'vlax-variant-value (vlax-safearray->list xdata))))
      (setq txt (entget (handent (cdr (assoc 1005 xdata)))))
    )
    (tbox:updatebox txt (cdr (assoc 1000 xdata)) (cdr (assoc 1040 xdata)) (entget (vlax-vla-object->ename box)))
  )
  (setq tbox:owner (cdr tbox:owner))
  (princ)
)

(defun tbox:commandcancelled (reactor params)
  (vlr-remove reactor)
  (setq tbox:owner nil)
  (princ)
)

(defun tbox:boxcopied (owner reactor params)
  (if (/= 0 (car params))
    (progn
      (setq tbox:owner (append tbox:owner (list (car params))))
      (vlr-command-reactor "tbox-boxcopiedcommreactor"
       '(
          (:vlr-commandended     . tbox:boxcopiedcommandended)
          (:vlr-commandcancelled . tbox:boxcopiedcommandcancelled)
          (:vlr-commandfailed    . tbox:boxcopiedcommandcancelled)
        )
      )
    )
  )
  (princ)
)

(defun tbox:boxcopiedcommandended (reactor params / ent)
  (vlr-remove reactor)
  (if
    (and
      (setq ent (car tbox:owner))
      (= 'ename (type ent))
      (member (cdr (assoc 0 (entget ent))) '("CIRCLE" "LWPOLYLINE"))
    )
    (entdel ent)
  )
  (setq tbox:owner (cdr tbox:owner))
  (princ)
)

(defun tbox:boxcopiedcommandcancelled (reactor params)
  (vlr-remove reactor)
  (setq tbox:owner nil)
  (princ)
)

;;------------------------------
;;  연동 생성 / 해제
;;------------------------------

;; 기존 연동이 있으면 박스를 지우고 글자의 연동 정보를 제거
(defun tbox:unlink (ent edata / old)
  (if (assoc -3 edata)
    (progn
      (setq old (handent (cdr (assoc 1005 (cdadr (assoc -3 edata))))))
      (if (and old (entget old) (tbox:valid-p ent) (tbox:valid-p old))
        (progn
          (tbox:removeowner (vlax-ename->vla-object old))
          (entdel old)
        )
      )
      (tbox:removeowner (vlax-ename->vla-object ent))
      (tbox:removexdata ent)
    )
  )
)

;; 글자에 박스 핸들을 저장하고 리액터 연결
(defun tbox:link (ent edata box)
  ;; 마스크에 가려지지 않도록 박스를 맨 앞으로 (실패해도 계속 진행)
  (vl-catch-all-apply 'tbox:tofront (list box))
  (entmod
    (append (vl-remove (assoc 40 edata) edata)
      (list
        (list -3
          (list tbox:app
           '(1002 . "{")
            (cons 1005 (cdr (assoc 5 (entget box))))
            (cons 1000 (strcase tbox:typ))
            (cons 1040 tbox:f)
           '(1002 . "}")
          )
        )
      )
    )
  )
  (tbox:addtext (list (vlax-ename->vla-object ent)))
  (tbox:addbox  (list (vlax-ename->vla-object box)))
)

;; 글자 1개에 박스 작성 (기존 연동이 있으면 해제 후 새로 작성)
(defun tbox:make (ent / box edata)
  (setq edata (entget ent (list tbox:app)))
  (cond
    ((= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (cdr (assoc 8 edata)))))))
      (princ "\n【TextBox】 잠긴 도면층의 객체입니다.")
    )
    (t
      (regapp tbox:app)
      (tbox:unlink ent edata)
      (setq ent (tbox:convert ent))
      (if (= "MTEXT" (cdr (assoc 0 (entget ent))))
        (tbox:mask ent (/= "Off" tbox:msk) tbox:f)
      )
      (setq edata (entget ent (list tbox:app)))
      (if (setq box (tbox:createbox edata (strcase tbox:typ) tbox:f))
        (tbox:link ent edata box)
        (princ "\n【TextBox】 박스를 만들 수 없습니다.")
      )
    )
  )
)

;; 글자 또는 박스의 연동 해제 (성공하면 nil, 실패하면 메시지 반환)
(defun tbox:release (ent / box edata obj)
  (setq edata (entget ent (list tbox:app)))
  (setq obj   (vlax-ename->vla-object ent))
  (cond
    ((null (assoc -3 edata))
      (princ "\n【TextBox】 연동 박스가 없는 객체입니다.")
    )
    ((null (vlax-write-enabled-p obj))
      (princ "\n【TextBox】 잠긴 도면층의 객체입니다.")
    )
    (t
      (setq box (handent (cdr (assoc 1005 (cdadr (assoc -3 edata))))))
      (tbox:removeowner obj)
      (tbox:removexdata ent)
      (if (and box (entget box))
        (progn
          (tbox:removeowner (vlax-ename->vla-object box))
          (tbox:removexdata box)
        )
      )
      (princ "\n【TextBox】 연동이 해제되었습니다.")
      nil
    )
  )
)

;;------------------------------
;;  명령
;;------------------------------

(defun c:TB (/ *error* done ent ent-list key pickfirst-orig res time-start)

  (defun *error* (msg)
    (if pickfirst-orig (setvar "PICKFIRST" pickfirst-orig))
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n【TextBox】 오류: " msg))
    )
    (princ)
  )

  (setq pickfirst-orig (getvar "PICKFIRST"))
  (setvar "PICKFIRST" 1)

  ;; 프롬프트 전에 미리 선택된 객체를 먼저 읽어둠
  (setq ent-list (tbox:sslist (ssget "_I" '((0 . "TEXT,MTEXT")))))

  (setq tbox:f
    (cond
      ((getreal (strcat "\n【TextBox】 오프셋 계수 <" (rtos tbox:f 2 2) ">: ")))
      (tbox:f)
    )
  )

  (initget "Circle Rectangle")
  (setq tbox:typ
    (cond
      ((getkword (strcat "\n【TextBox】 박스 모양 [Circle/Rectangle] <" tbox:typ ">: ")))
      (tbox:typ)
    )
  )

  (initget "1 2")
  (setq key (getkword (strcat "\n【TextBox】 \U+BC30\U+ACBD \U+B9C8\U+C2A4\U+D06C [ON(1)/OFF(2)] <" (strcase tbox:msk) ">: ")))
  (setq tbox:msk (cond ((= key "1") "On") ((= key "2") "Off") (tbox:msk)))

  ;; 미리 선택한 객체가 없으면 다중 선택 (선택 후 Enter/스페이스로 실행)
  (if (null ent-list)
    (progn
      (princ "\n【TextBox】 ")
      (setq ent-list (tbox:sslist (ssget '((0 . "TEXT,MTEXT")))))
    )
  )

  (if ent-list
    (progn
      (setq done 0)
      (setq time-start (getvar "MILLISECS"))
      (foreach ent ent-list
        (if (vl-catch-all-error-p (setq res (vl-catch-all-apply 'tbox:make (list ent))))
          (princ (strcat "\n【TextBox】 오류: " (vl-catch-all-error-message res)))
          (setq done (1+ done))
        )
      )
      (princ (strcat "\n【TextBox】 " (itoa done) "개 처리  Time: " (itoa (- (getvar "MILLISECS") time-start)) " ms"))
    )
  )

  ;; 정상 종료도 *error* 로 PICKFIRST 복원
  (*error* "exit")
)

(defun c:TBR (/ ent)
  (while
    (progn
      (setvar 'errno 0)
      (setq ent (entsel "\n【TextBox】 연동 해제할 글자 또는 박스 선택: "))
      (cond
        ((= 7 (getvar 'errno))
          (princ "\n【TextBox】 선택 실패, 다시 선택하세요.")
        )
        ((= 'ename (type (setq ent (car ent))))
          (tbox:release ent)
        )
      )
    )
  )
  (princ)
)

;;------------------------------
;;  로드 시 리액터 복원 (도면에 저장된 xData 기준)
;;------------------------------

(
  (lambda (/ *error* box ent inc lay lck obj sel txt typ vlr)
    (defun *error* (msg)
      (foreach lay lck (vla-put-lock lay :vlax-true))
      (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
        (princ (strcat "\n【TextBox】 오류: " msg))
      )
      (princ)
    )
    (setq lck (tbox:unlocklayers))
    (foreach typ (vlr-reactors :vlr-object-reactor :vlr-command-reactor)
      (foreach vlr (cdr typ)
        (if (and (= 'str (type (vlr-data vlr))) (wcmatch (vlr-data vlr) "tbox-*reactor"))
          (vlr-remove vlr)
        )
      )
    )
    (setq tbox:textreactor nil)
    (setq tbox:boxreactor nil)
    (if (setq sel (ssget "_X" (list '(0 . "TEXT,MTEXT,CIRCLE,LWPOLYLINE") (list -3 (list tbox:app)))))
      (progn
        (repeat (setq inc (sslength sel))
          (setq ent (ssname sel (setq inc (1- inc))))
          (setq obj (vlax-ename->vla-object ent))
          (cond
            ((not (tbox:valid-p ent))
              (vl-catch-all-apply 'tbox:removexdata (list ent))
            )
            ((member (cdr (assoc 0 (entget ent))) '("TEXT" "MTEXT"))
              (setq txt (cons obj txt))
            )
            ((setq box (cons obj box)))
          )
        )
        (if txt (tbox:addtext txt))
        (if box (tbox:addbox box))
      )
    )
    (foreach lay lck (vla-put-lock lay :vlax-true))
    (setq tbox:owner nil)
    (princ)
  )
)

(princ)