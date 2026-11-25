;;; ==============================
;;; Loader.LSP
;;;  - 같은 폴더의 .lsp / .fas / .vlx 를 일괄 로드한다
;;;
;;; 원본파일: 직접 작성
;;;
;;; 명령어: RELOAD, REE
;;;
;;; 사용법
;;;  1) APPLOAD 시작 목록에 이 파일 하나만 등록한다
;;;  2) 리습을 추가하려면 이 폴더에 파일을 넣고 RELOAD (또는 REE)
;;;  3) 로드하기 싫은 파일은 이름 앞에 _ 를 붙인다
;;;
;;; 가정 / 주의
;;;  - 같은 이름이 여럿이면 vlx > fas > lsp 중 하나만 로드, DLL 도 같은 폴더에 둔다
;;;  - .fas/.vlx 는 폴더가 TRUSTEDPATHS 에 없으면 보안 경고가 뜬다 (설정은 직접)
;;;
;;; 동작방식
;;;  - 폴더 위치: 기존값 > 지원 경로 > 시작 목록 레지스트리 > 문서\LISP
;;;  - first -> 나머지(이름순) -> last 순으로 로드, 실패해도 계속하고 요약 출력
;;; ==============================

(vl-load-com)

;;------------------------------
;;  설정
;;------------------------------
(setq *loader-ext* '(".vlx" ".fas" ".lsp"))   ; 앞쪽이 우선순위가 높다

(setq *loader-first*
  '("KorIME.lsp"))                ; 한글 명령 자동 영문 변환

(setq *loader-last*
  '("zCommand.lsp"                      ; 별칭 모음 - 앞의 정의를 덮어쓰도록 뒤쪽에
    "MPL.lsp"))                         ; 다중 페이지 PDF 출력

;;------------------------------
;;  폴더 위치
;;------------------------------
;; 시작 목록에 등록된 Loader.lsp 의 폴더 (찾지 못하면 nil)
(defun loader:regdir (/ key name result value)
  (setq key
    (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles\\"
            (getvar "CPROFILE") "\\Dialogs\\Appload\\Startup"))
  (foreach name (vl-registry-descendents key t)
    (setq value (vl-registry-read key name))
    (if (and (not result)
             (= (type value) 'STR)
             (wcmatch (strcase value) "*[\\/]LOADER.LSP"))
      (setq result (vl-filename-directory value))))
  result)

;; 시작 목록으로 로드되면 자기 위치를 알 수 없어 순서대로 찾는다
(defun loader:home (/ path result)
  (cond
    (*loader-dir*)
    ((setq path (findfile "Loader.lsp"))
     (vl-filename-directory path))
    ((and (setq result (vl-catch-all-apply 'loader:regdir nil))
          (not (vl-catch-all-error-p result)))
     result)
    (t
     (strcat (getenv "USERPROFILE") "\\Documents\\LISP"))))

;;------------------------------
;;  로드 목록
;;------------------------------
(defun loader:key (name)
  (strcase (vl-filename-base name)))

(defun loader:rank (name)
  (vl-position (strcase (vl-filename-extension name) t) *loader-ext*))

;; 폴더의 로드 후보 (_ 로 시작하는 파일과 Loader 자신은 제외)
(defun loader:scan (/ ext files)
  (foreach ext *loader-ext*
    (setq files
      (append files (vl-directory-files *loader-dir* (strcat "*" ext) 1))))
  (vl-remove-if
    '(lambda (name)
       (or (= (substr name 1 1) "_")
           (= (loader:key name) "LOADER")
           (not (loader:rank name))))
    files))

;; 같은 이름은 우선순위가 가장 높은 확장자 하나만 남긴다
(defun loader:best-only (files / name result seen)
  (foreach name
           (vl-sort files '(lambda (a b) (< (loader:rank a) (loader:rank b))))
    (if (not (member (loader:key name) seen))
      (progn
        (setq seen (cons (loader:key name) seen))
        (setq result (cons name result)))))
  (reverse result))

;; keys 순서대로 files 에서 이름이 맞는 파일을 뽑는다
(defun loader:pick (keys files / key name result)
  (foreach key keys
    (foreach name files
      (if (= (loader:key name) key)
        (setq result (cons name result)))))
  (reverse result))

;; first -> 나머지(이름순) -> last
(defun loader:list (/ files first-keys last-keys middle)
  (setq files (loader:best-only (loader:scan)))
  (setq first-keys (mapcar 'loader:key *loader-first*))
  (setq last-keys (mapcar 'loader:key *loader-last*))
  (setq middle
    (vl-remove-if
      '(lambda (name) (member (loader:key name) (append first-keys last-keys)))
      files))
  (append
    (loader:pick first-keys files)
    (vl-sort middle '(lambda (a b) (< (strcase a) (strcase b))))
    (loader:pick last-keys files)))

;;------------------------------
;;  로드
;;------------------------------
;; 오늘 날짜, 요일, 시간 (예: 2026-11-09 월요일 14:30:25)
(defun loader:now (/ cdate day-index)
  (setq cdate (rtos (getvar "CDATE") 2 6))
  (setq day-index (rem (fix (+ (getvar "DATE") 0.5)) 7))   ; 0 = 월요일
  (strcat (substr cdate 1 4) "-" (substr cdate 5 2) "-" (substr cdate 7 2) " "
          (nth day-index '("월" "화" "수" "목" "금" "토" "일")) "요일 "
          (substr cdate 10 2) ":" (substr cdate 12 2) ":" (substr cdate 14 2)))

;; 파일 하나 로드 -> 성공 T / 실패 시 사유 문자열
(defun loader:one (name / result)
  (setq result
    (vl-catch-all-apply 'load (list (strcat *loader-dir* "\\" name))))
  (if (vl-catch-all-error-p result)
    (vl-catch-all-error-message result)
    t))

(defun loader:report (failures / failure)
  (princ (strcat "\n【Loader】 오류: " (itoa (length failures)) "개 실패"))
  (foreach failure (reverse failures)
    (princ (strcat "\n  - " failure))))

(defun loader:run (/ failures name ok-count result start-ms)
  (cond
    ((not (vl-file-directory-p *loader-dir*))
     (princ (strcat "\n【Loader】 오류: 폴더 없음 " *loader-dir*)))
    (t
     (princ (strcat "\n【Loader】 " (loader:now)))
     (setq start-ms (getvar "MILLISECS"))
     (setq *kime-dir* *loader-dir*)          ; KorIME 는 로드할 때 읽는다
     (setq ok-count 0)
     (foreach name (loader:list)
       (setq result (loader:one name))
       (if (= result t)
         (setq ok-count (1+ ok-count))
         (setq failures (cons (strcat name " : " result) failures))))
     ;; MPL.lsp 는 로드할 때 *mp:dll* 을 nil 로 초기화하므로 로드 후에 지정한다
     (setq *mp:dll* (strcat *loader-dir* "\\MPL.dll"))
     (princ
       (strcat "\n【Loader】 LISP " (itoa ok-count) "개 로드 완료 ("
               (itoa (- (getvar "MILLISECS") start-ms)) " ms)"))
     (if failures
       (loader:report failures))
     (princ "\n【Loader】 즐거운 캐드!")))
  (princ))

;;------------------------------
;;  보안 경고 안내
;;------------------------------
;; 문자열 안의 \ 를 모두 \\ 로 바꾼다 (명령창에 그대로 붙여넣을 수 있게)
(defun loader:dbl (text / char result)
  (setq result "")
  (foreach char (vl-string->list text)
    (setq result (strcat result (if (= char 92) "\\\\" (chr char)))))
  result)

;; .fas/.vlx 가 있는데 폴더가 TRUSTEDPATHS 에 없으면 입력할 명령만 안내한다
(defun loader:trust-check (/ dir trusted)
  (setq trusted (vl-catch-all-apply 'getvar '("TRUSTEDPATHS")))
  (if (and (= (type trusted) 'STR)
           (or (vl-directory-files *loader-dir* "*.fas" 1)
               (vl-directory-files *loader-dir* "*.vlx" 1)))
    (progn
      (setq dir (strcase (vl-string-right-trim "\\" *loader-dir*)))
      (if (not (vl-string-search dir (strcase trusted)))
        (princ
          (strcat
            "\n【Loader】 .fas/.vlx 보안 경고를 없애려면 아래 명령을 한 번만 입력하세요:"
            "\n(setvar \"TRUSTEDPATHS\" (strcat (getvar \"TRUSTEDPATHS\") \";"
            (loader:dbl *loader-dir*) "\"))")))))
  (princ))

;;------------------------------
;;  명령
;;------------------------------
;; RELOAD / REE : AutoCAD 를 다시 켜지 않고 폴더의 리습을 다시 읽는다
(defun c:RELOAD ()
  (load (strcat *loader-dir* "\\Loader.lsp"))
  (princ))

(defun c:REE ()
  (c:RELOAD))

;;------------------------------
;;  로드 시 초기화
;;------------------------------
(setq *loader-dir* (vl-string-translate "/" "\\" (loader:home)))
(loader:run)
(loader:trust-check)
(princ)