;;; 텍스트 줄간격 맞추기 ;;;;
(defun c:tdd ( / sel_txt al_pt1 al_pt2 al_pt3 index n ch_txt1 ch_txt2 ins_pt ref_pt txtline_gap sort sorti m i ins_ypt)
(prompt "\n정렬할 문자 선택 : ")
  (setq sel_txt (progn (princ "\n【TDD】 ") (ssget '((-4 . "<or")
    (0 . "TEXT") (0 . "MTEXT")
    (-4 . "or>")
   )
  ))
  )
(setq txtline_gap (getint "\n【TDD】 텍스트 줄간격 : "  ))
(setq index 0)
(setq n (sslength sel_txt))
(repeat n
    (setq ent (ssname sel_txt index))
    (setq prop_ent (entget ent))
    (setq in_point (cdr (assoc 10 prop_ent)))
    (setq sort (append sort (list in_point)))
  (setq index (1+ index))
  ) 
  (vl-load-com)
  (setq sorti
  (vl-sort-i sort
      (function (lambda (e1 e2) (> (cadr e1) (cadr e2))))
  )
  )
 (setq m 0)
  (repeat n
    (setq i (nth m sorti))
    (setq ch_txt1 (ssname sel_txt i))
    (setq ch_txt2 (entget ch_txt1))
 
    (setq al_pt1 (cdr (assoc 72 ch_txt2)))
    (setq al_pt2 (cdr (assoc 73 ch_txt2)))
    
    (if (and (= al_pt1 0) (= al_pt2 0))
    (setq ins_pt (cdr (assoc 10 ch_txt2)))
    (setq ins_pt (cdr (assoc 11 ch_txt2)))
    )
  (if (= m 0)
    (progn
    (setq ins_xpt(car ins_pt))
    (setq ins_ypt(cadr ins_pt))
    )  
    (princ)
)
    (setq ins_pt (- ins_ypt (* txtline_gap m)))
    (setq ins_pt (list ins_xpt ins_pt ))
    
    (if (and (= al_pt1 0) (= al_pt2 0))
      (progn
    (setq ch_txt2 (subst (cons 10 ins_pt) (assoc 10 ch_txt2) ch_txt2)) ;;; 이게 중요함 10코드를 뽑아서 11번 코드로 넣어줘야 됨 ;;;
    (entmod ch_txt2)
    )
      
        (progn
    (setq ch_txt2 (subst (cons 11 ins_pt) (assoc 11 ch_txt2) ch_txt2)) ;;; 이게 중요함 10코드를 뽑아서 11번 코드로 넣어줘야 됨 ;;;
    (entmod ch_txt2)
    )
   )
        (setq m (1+ m))  
)
  )

제가 지금 쓰는 줄간격 맞추기 리습인데요
줄간격이 정수로만 설정이 되네요. 소수점도 되게 바꾸고 싶은데 가능한가요?
