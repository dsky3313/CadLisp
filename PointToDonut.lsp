;; PTD : 선택한 점(POINT)마다 도넛을 그린다. 실행하면 도넛 외경을 먼저 묻는다.
;;   - Enter 로 직전에 쓴 외경(처음에는 10)을 그대로 사용
;;   - 내경은 0 (꽉 찬 원). 바꾸려면 아래 *ptd-id* 값을 수정
;;   - 도넛을 모두 그린 뒤 원본 점(POINT)은 삭제한다 (남기려면 아래 *ptd-del* 을 nil 로)
(setq *ptd-id* 0.0)
(setq *ptd-del* T)                      ; T: 변환 후 원본 점 삭제

(defun c:PTD ( / od ss i ent pt n)
  (or *ptd-od* (setq *ptd-od* 10.0))
  (initget 6)                           ; 0 과 음수는 입력 불가
  (setq od (getdist (strcat "\n【PointToDonut】 도넛 외경 <" (rtos *ptd-od*) ">: ")))
  (if od (setq *ptd-od* od))

  (princ "\n변환할 Point(점) 객체들을 선택하세요: ")
  (if (setq ss (progn (princ "\n【PointToDonut】 ") (ssget '((0 . "POINT")))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              pt  (trans (cdr (assoc 10 (entget ent))) 0 1))   ; WCS -> 현재 UCS
        ;; 도넛 명령 (내경, 외경, 중심점) - _non 으로 오스냅 영향 제거
        (command "_.donut" *ptd-id* *ptd-od* "_non" pt "")
        (setq i (1+ i)))
      ;; 도넛을 모두 그린 뒤 원본 점 삭제 (잠긴 레이어의 점은 건너뜀)
      (setq n 0)
      (if *ptd-del*
        (repeat (sslength ss)
          (setq i (1- i))
          (if (not (vl-catch-all-error-p (vl-catch-all-apply (quote entdel) (list (ssname ss i)))))
            (setq n (1+ n)))))
      (setq i (sslength ss))
      (princ (strcat "\nPoint -> Donut 변환 완료 (" (itoa i) "개, 외경 " (rtos *ptd-od*) (if *ptd-del* (strcat ", 원본 점 " (itoa n) "개 삭제") "") ")."))))
  (princ))

(princ "\n[사용법] 명령창에 PTD 를 입력하세요.")
(princ)