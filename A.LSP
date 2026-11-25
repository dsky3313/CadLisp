;; 시작점, 끝점, 방향으로 호 그리기
(defun c:a ( / p1 p2)
  (setq p1 (getpoint "\n【A】 pick a start point : "))
  (setq p2 (getpoint "\n【A】 pick a end point : "))
  (command "ARC" p1 "E" p2 "d")
  (prin1))