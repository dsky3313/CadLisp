;;; ==============================
;;; TextRound.LSP
;;;  - 문자(TEXT/MTEXT) 안의 소수를 올림/내림/반올림
;;;
;;; 원본파일:
;;;
;;; 명령어: TRND
;;;
;;; 사용법
;;;  1) TRND 실행 → 현재 설정 표시, 키워드로 변경 (올림 U / 내림 D / 반올림 R / 자릿수 P)
;;;  2) Enter → 문자 객체 선택 (미리 선택도 가능)
;;;  3) 선택한 문자 속 소수가 설정대로 처리됨 (예: 12.3456 + 반올림 + 2자리 → 12.35)
;;;
;;; 가정 / 주의
;;;  - 자릿수 = 남길 소수점 아래 자리수. 모자라면 0을 붙임 (11.46 → 11.460). 정수는 그대로 둠
;;;  - 올림/내림은 절댓값 기준(엑셀 ROUNDUP/ROUNDDOWN과 동일). 잠긴 도면층은 건너뜀
;;;
;;; 동작방식
;;;  - 숫자 문자열을 직접 자리 연산해 부동소수 오차 없음. MTEXT 서식코드, %%nnn은 건드리지 않음
;;;  - 설정은 레지스트리(HKCU\Software\hodo\TextRound)에 저장되어 다음 세션에도 유지
;;; ==============================

;;------------------------------
;;  설정
;;------------------------------
(setq trnd:regkey "HKEY_CURRENT_USER\\Software\\hodo\\TextRound")
(setq trnd:mode "Round")  ; Up / Down / Round
(setq trnd:digits 2)

;;------------------------------
;;  보조 함수
;;------------------------------
;; 설정 저장/복원
(defun trnd:save ()
  (vl-registry-write trnd:regkey "Mode" trnd:mode)
  (vl-registry-write trnd:regkey "Digits" (itoa trnd:digits))
)

(defun trnd:load (/ mode-saved digits-saved)
  (setq mode-saved (vl-registry-read trnd:regkey "Mode"))
  (if (member mode-saved '("Up" "Down" "Round"))
    (setq trnd:mode mode-saved)
  )
  (setq digits-saved (vl-registry-read trnd:regkey "Digits"))
  (if digits-saved
    (setq trnd:digits (max 0 (atoi digits-saved)))
  )
)

(defun trnd:label ()
  (cdr (assoc trnd:mode '(("Up" . "올림") ("Down" . "내림") ("Round" . "반올림"))))
)

;; 숫자열 문자열에 1 더하기 (자리올림 포함)
(defun trnd:incr (str / result carry code)
  (setq carry T)
  (setq result "")
  (foreach code (reverse (vl-string->list str))
    (cond
      ((and carry (= code 57))
        (setq result (strcat "0" result))
      )
      (carry
        (setq result (strcat (chr (1+ code)) result))
        (setq carry nil)
      )
      (t
        (setq result (strcat (chr code) result))
      )
    )
  )
  (if carry (strcat "1" result) result)
)

;; pos부터 이어지는 숫자열
(defun trnd:digit-run (str pos / run)
  (setq run "")
  (while (wcmatch (substr str pos 1) "#")
    (setq run (strcat run (substr str pos 1)))
    (setq pos (1+ pos))
  )
  run
)

;; 정수부/소수부 문자열 → 처리된 숫자 문자열
(defun trnd:round-num (int-str frac-str / pad-str keep rest-str inc-p all-digits total int-out)
  (if (<= (strlen frac-str) trnd:digits)
    (progn
      (setq pad-str frac-str)
      (while (< (strlen pad-str) trnd:digits)
        (setq pad-str (strcat pad-str "0"))
      )
      (strcat int-str "." pad-str)
    )
    (progn
      (setq keep (substr frac-str 1 trnd:digits))
      (setq rest-str (substr frac-str (1+ trnd:digits)))
      (setq inc-p
        (cond
          ((= trnd:mode "Up") (/= "" (vl-string-left-trim "0" rest-str)))
          ((= trnd:mode "Round") (>= (ascii (substr rest-str 1 1)) 53))
          (t nil)
        )
      )
      (setq all-digits (strcat int-str keep))
      (if inc-p
        (setq all-digits (trnd:incr all-digits))
      )
      (setq total (strlen all-digits))
      (setq int-out (substr all-digits 1 (- total trnd:digits)))
      (if (> trnd:digits 0)
        (strcat int-out "." (substr all-digits (1+ (- total trnd:digits))))
        int-out
      )
    )
  )
)

;; 문자열 전체를 훑어 소수만 치환
(defun trnd:process (str mtext-p / pos len out ch next-ch take piece semi-pos int-str frac-str)
  (setq pos 1)
  (setq len (strlen str))
  (setq out "")
  (while (<= pos len)
    (setq ch (substr str pos 1))
    (setq take 1)
    (setq piece ch)
    (cond
      ;; MTEXT 서식코드는 건드리지 않음 (숫자가 들어있음: \H2.5x; 등)
      ((and mtext-p (= ch "\\"))
        (setq next-ch (substr str (1+ pos) 1))
        (setq take 2)
        (if (wcmatch next-ch "【ACFHQTWSacfhqtwsp】")
          (progn
            (setq semi-pos (vl-string-position 59 str pos))
            (setq take (if semi-pos (- (+ semi-pos 2) pos) (- (1+ len) pos)))
          )
        )
        (setq piece (substr str pos take))
      )
      ;; %%nnn 문자코드
      ((and (= ch "%") (= (substr str (1+ pos) 1) "%") (wcmatch (substr str (+ pos 2) 1) "#"))
        (setq take 5)
        (setq piece (substr str pos take))
      )
      ((wcmatch ch "#")
        (setq int-str (trnd:digit-run str pos))
        (setq take (strlen int-str))
        (setq piece int-str)
        (if (and (= (substr str (+ pos take) 1) ".") (wcmatch (substr str (+ pos take 1) 1) "#"))
          (progn
            (setq frac-str (trnd:digit-run str (+ pos take 1)))
            (setq take (+ take 1 (strlen frac-str)))
            (if (and (= (substr str (+ pos take) 1) ".") (wcmatch (substr str (+ pos take 1) 1) "#"))
              (setq piece (substr str pos take))  ; 1.2.3 같은 버전/날짜형은 그대로
              (setq piece (trnd:round-num int-str frac-str))
            )
          )
        )
      )
    )
    (setq out (strcat out piece))
    (setq pos (+ pos take))
  )
  out
)

;; 문자 내용 읽기/쓰기 (MTEXT는 3번 코드 조각 + 1번 코드)
(defun trnd:get-string (edata)
  (if (= (cdr (assoc 0 edata)) "MTEXT")
    (strcat
      (apply 'strcat (mapcar 'cdr (vl-remove-if-not '(lambda (pair) (= (car pair) 3)) edata)))
      (cdr (assoc 1 edata))
    )
    (cdr (assoc 1 edata))
  )
)

(defun trnd:set-string (edata str / chunks result done pair)
  (setq chunks nil)
  (while (> (strlen str) 250)
    (setq chunks (cons (cons 3 (substr str 1 250)) chunks))
    (setq str (substr str 251))
  )
  (setq chunks (cons (cons 1 str) chunks))  ; 역순 보관: 맨 앞이 마지막 조각
  (foreach pair edata
    (cond
      ((not (member (car pair) '(1 3)))
        (setq result (cons pair result))
      )
      ((not done)
        (setq result (append chunks result))
        (setq done T)
      )
    )
  )
  (reverse result)
)

(defun trnd:locked-p (edata / layer-data)
  (setq layer-data (tblsearch "LAYER" (cdr (assoc 8 edata))))
  (and layer-data (= 4 (logand 4 (cdr (assoc 70 layer-data)))))
)

;;------------------------------
;;  명령 함수
;;------------------------------
(defun c:TRND (/ input num ss i edata old-str new-str changed-count locked-count)
  ;; 설정 확인/변경 (Enter로 선택 단계 진행)
  (setq input "")
  (while input
    (initget "Up Down Round Precision")
    (setq input
      (getkword
        (strcat "\n" (trnd:label) ", 소수 " (itoa trnd:digits)
                "자리  [올림(U)/내림(D)/반올림(R)/자릿수(P)] <객체선택>: ")
      )
    )
    (cond
      ((null input))
      ((= input "Precision")
        (initget 4)
        (setq num (getint (strcat "\n【TextRound】 남길 소수점 자릿수 <" (itoa trnd:digits) ">: ")))
        (if num
          (setq trnd:digits num)
        )
        (trnd:save)
      )
      (t
        (setq trnd:mode input)
        (trnd:save)
      )
    )
  )
  (setq ss (progn (princ "\n【TextRound】 ") (ssget '((0 . "TEXT,MTEXT")))))
  (if (null ss)
    (princ "\n【TextRound】 선택된 문자가 없음")
    (progn
      (setq changed-count 0)
      (setq locked-count 0)
      (setq i 0)
      (repeat (sslength ss)
        (setq edata (entget (ssname ss i)))
        (setq i (1+ i))
        (cond
          ((trnd:locked-p edata)
            (setq locked-count (1+ locked-count))
          )
          (t
            (setq old-str (trnd:get-string edata))
            (setq new-str (trnd:process old-str (= (cdr (assoc 0 edata)) "MTEXT")))
            (if (/= old-str new-str)
              (progn
                (entmod (trnd:set-string edata new-str))
                (setq changed-count (1+ changed-count))
              )
            )
          )
        )
      )
      (princ
        (strcat "\n【TextRound】 " (trnd:label) " " (itoa trnd:digits) "자리: "
                (itoa changed-count) "개 변경"
                (if (> locked-count 0) (strcat ", 잠긴 도면층 " (itoa locked-count) "개 제외") "")
        )
      )
    )
  )
  (princ)
)

;;------------------------------
;;  로드 시 초기화
;;------------------------------
(trnd:load)
(princ)
