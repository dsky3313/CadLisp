; NumberCheck.lsp
; 설   명: 선택한 객체 중 정수 텍스트를 지정 범위 내에서 순서대로 순회
; 사용법: N 실행 후 객체 선택, 시작/끝 번호 입력

(defun c:N (/ ss minnum maxnum i ename txtval num collected dynmode-orig idx item input next-num prev-num total hl-ename *error*)

  ; ESC/종료 시 DYNMODE 복원 + 하이라이트 해제
  (defun *error* (msg)
    (if dynmode-orig (setvar "DYNMODE" dynmode-orig))
    (if hl-ename (redraw hl-ename 4))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*취소*,*종료*"))
      (princ (strcat "\n오류: " msg))
    )
    (princ)
  )

  ; 순수 정수 문자열 여부 확인 (앞뒤 공백 제거 후 숫자만)
  (defun is-pure-integer (str / trimmed)
    (setq trimmed (vl-string-trim " " str))
    (and
      (> (strlen trimmed) 0)
      (vl-every
        (function (lambda (c) (and (>= c 48) (<= c 57))))
        (vl-string->list trimmed)
      )
    )
  )

  ; 텍스트 값 추출 (TEXT, MTEXT 모두 대응)
  (defun get-text-value (ename)
    (cdr (assoc 1 (entget ename)))
  )

  ; 누락 번호 구간 알림 (from~to 연속 구간을 한 번에)
  (defun alert-missing (from to)
    (if (<= from to)
      (alert (strcat "[ " (itoa from) (if (< from to) (strcat " ~ " (itoa to)) "") " ] 누락!"))
    )
  )

  ; 객체로 줌 이동 + 하이라이트 + 입력 대기, 반환값: 입력 문자열
  (defun show-item (ename num / input cmdecho-orig edata)
    (setq edata (entget ename))
    (setq cmdecho-orig (getvar "CMDECHO"))
    (setvar "CMDECHO" 0)
    (command "._ZOOM" "_C" (trans (cdr (assoc 10 edata)) 0 1) (/ (cdr (assoc 40 edata)) 0.05))  ; 약간 축소해서 주변 맥락 확보
    (setvar "CMDECHO" cmdecho-orig)
    (setq hl-ename ename)
    (redraw ename 3)
    (initget "Next Previous Quit")
    (setq input (getkword (strcat "\n【NumberCheck】 " (itoa num) "번 [다음(N)/이전(P)/종료(Q)] <다음>: ")))
    (redraw ename 4)
    (setq hl-ename nil)
    input
  )

  ; 객체 선택
  (princ "\n[P] 엔터 → 이전 객체 선택\n")
  (setq ss (progn (princ "\n【NumberCheck】 ") (ssget '((0 . "TEXT,MTEXT")))))
  (if (null ss)
    (progn (princ "\n객체를 선택하지 않았습니다.") (exit))
  )

  ; 시작/끝 번호 입력 - 동적 입력 끄고 커맨드라인에서 받기
  (setq dynmode-orig (getvar "DYNMODE"))
  (setvar "DYNMODE" 0)

  (princ "\n【NumberCheck】 시작 번호 입력 (엔터 = 1): ")
  (setq input (getstring))
  (setq minnum
    (if (= (vl-string-trim " " input) "")
      1
      (atoi input)
    )
  )
  (if (< minnum 1) (setq minnum 1))

  (princ (strcat "\n【NumberCheck】 끝 번호 입력 (엔터 = " (itoa minnum) "): "))
  (setq input (getstring))
  (setq maxnum
    (if (= (vl-string-trim " " input) "")
      minnum
      (atoi input)
    )
  )
  (setvar "DYNMODE" dynmode-orig)

  (if (< maxnum minnum)
    (progn (princ "\n끝 번호는 시작 번호 이상이어야 합니다.") (exit))
  )

  ; 선택 집합에서 정수 텍스트만 수집
  (setq collected '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq txtval (get-text-value ename))
    (if (and txtval (is-pure-integer txtval))
      (progn
        (setq num (atoi (vl-string-trim " " txtval)))
        (if (and (>= num minnum) (<= num maxnum))
          (setq collected (cons (list num ename) collected))
        )
      )
    )
    (setq i (+ i 1))
  )

  ; 숫자값 기준 오름차순 정렬
  (setq collected
    (vl-sort collected
      (function (lambda (a b) (< (car a) (car b))))
    )
  )

  (if (null collected)
    (progn (princ "\n해당 범위의 정수 텍스트를 찾지 못했습니다.") (exit))
  )

  ; 리스트를 벡터처럼 쓰기 위해 길이 저장
  (setq total (length collected))
  (princ (strcat "\n총 " (itoa total) "개 발견."))

  ; 인덱스 기반 순회
  (setq idx 0)
  (while (< idx total)
    (setq item  (nth idx collected))
    (setq num   (car item))
    (setq ename (cadr item))

    ; 줌 이동 + 입력 대기
    (setq input (show-item ename num))

    (cond
      ((= input "Quit")
       (princ "\n순회 종료.") (exit))
      ((= input "Previous")
       (if (> idx 0)
         (progn
           ; 현재~이전 항목 사이 누락 감지
           (setq prev-num (car (nth (- idx 1) collected)))
           (alert-missing (+ prev-num 1) (- num 1))
           (setq idx (- idx 1))
         )
       ))
      (t
       ; 다음 항목 번호 계산 (마지막이면 maxnum+1)
       (setq next-num
         (if (< idx (- total 1))
           (car (nth (+ idx 1) collected))
           (+ maxnum 1)
         )
       )
       ; 현재~다음 사이 누락 감지
       (alert-missing (+ num 1) (- next-num 1))
       (setq idx (+ idx 1)))
    )
  )

  (alert "순회 완료")
  (princ "\n순회 완료.")
  (princ)
)