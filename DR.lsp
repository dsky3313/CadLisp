;; 레이어 그리기 순서 정리 - 목록 앞쪽 레이어가 위, 뒤쪽 레이어가 아래로 쌓인다
(setq *dr-layers* '("!TEXT" "!0" "!2" "!1" "!6" "!5" "!4" "!3" "!7" "!01" "!999"))

(defun SETLAYERORDER (sslist)
  (command "_.draworder" sslist "" "_back"))

(defun c:DR ( / ss)
  (foreach lay *dr-layers*
    (if (setq ss (ssget "_X" (list (cons 8 lay))))
      (SETLAYERORDER ss)))
  (prompt "레이어 그리기 순서를 변경합니다.")
  (princ))