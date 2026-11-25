; DonutSize.lsp
; 설   명: 선택한 도넛의 안지름/바깥지름 변경
; 사용법: DS 실행 후 도넛 선택, 안지름/바깥지름 입력

(defun c:DS (/ ss id od r w i ename edata pts p1 p2 cx cy d ux uy newpts n new cnt)

  (setq ss (progn (princ "\n【DonutSize】 ") (ssget '((0 . "LWPOLYLINE") (90 . 2)))))
  (if (null ss)
    (progn (princ "\n객체를 선택하지 않았습니다.") (exit))
  )

  (setq id (getdist "\n【DonutSize】 안지름 <0>: "))
  (if (null id) (setq id 0.0))
  (setq od (getdist "\n【DonutSize】 바깥지름: "))
  (if (or (null od) (<= od id))
    (progn (princ "\n바깥지름은 안지름보다 커야 합니다.") (exit))
  )

  (setq r (/ (+ id od) 4.0))
  (setq w (/ (- od id) 2.0))
  (setq cnt 0)
  (setq i 0)

  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq edata (entget ename))
    (if (= 1 (logand 1 (cdr (assoc 70 edata))))
      (progn
        (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) edata)))
        (setq p1 (car pts))
        (setq p2 (cadr pts))
        (setq cx (/ (+ (car p1) (car p2)) 2.0))
        (setq cy (/ (+ (cadr p1) (cadr p2)) 2.0))
        (setq d (distance p1 p2))
        (if (> d 1e-9)
          (progn
            (setq ux (/ (- (car p1) (car p2)) d))
            (setq uy (/ (- (cadr p1) (cadr p2)) d))
            (setq newpts
              (list
                (list (+ cx (* ux r)) (+ cy (* uy r)))
                (list (- cx (* ux r)) (- cy (* uy r)))
              )
            )
            (setq n 0)
            (setq new nil)
            (foreach item edata
              (cond
                ((= (car item) 10)
                 (setq new (cons (cons 10 (nth n newpts)) new))
                 (setq n (1+ n))
                )
                ((member (car item) '(40 41 43))
                 (setq new (cons (cons (car item) w) new))
                )
                (t (setq new (cons item new)))
              )
            )
            (entmod (reverse new))
            (entupd ename)
            (setq cnt (1+ cnt))
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (princ (strcat "\n도넛 " (itoa cnt) "개 변경 완료."))
  (princ)
)