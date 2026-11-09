;;; ==============================
;;; PlotImage.LSP
;;;  - 가이드 테두리 기준 PNG 내보내기
;;;
;;; 명령어: PP
;;;
;;; 사용법
;;;  1) 가이드가 되는 사각형(LINE/LWPOLYLINE 등, 여러 개 선택 가능)을 그려둔다
;;;  2) 모형(Model) 탭에서 PP 입력
;;;  3) 가이드 객체 선택 -> Enter
;;;  4) 저장 대화상자에서 파일명/위치 확인 또는 수정 (기본값: 도면명-배치명.png)
;;;  5) 가이드 영역으로 ZOOM Window -> 배경 흰색 전환 -> PNGOUT -> 원상복구
;;;  6) 흰 여백과 가이드 테두리 선까지 자동으로 잘라냄 (순수 내용물만 남김)
;;;  7) PNGOUT이 남기는 plot.log 자동 삭제
;;;
;;; 가정 / 주의
;;;  - 모형(Model) 탭 전용, ActiveX(vla-/vlax-) 기반이라 AutoCAD LT 불가
;;;  - 창 최대화는 복구 안 함 / 명령행·클린스크린은 끝나면 켠 상태로, 리본은 원래 켜져 있었을 때만 복구
;;;  - "PNGOUT"만 존재 (-PNGOUT 없음), FILEDIA 0으로 대화상자 없이 처리
;;;
;;; 동작방식
;;;  - PC3 플롯은 해상도가 고정이라 PC3 없이 PNGOUT(현재 화면 그대로)으로 내보냄
;;;  - 명령행 숨김 후 클린스크린은 SendCommand로 LISP 종료 후 반영시킨 뒤 ZOOM W + 흰 배경
;;;  - 테두리까지 같이 내보낸 뒤 흰 여백과 테두리 선 두께까지 트림 (PlotImage.dll 우선, 없으면 PowerShell)
;;; ==============================

(vl-load-com)

;;------------------------------
;;  설정
;;------------------------------

(setq pimg:dll nil)  ; 트림 엔진: nil = PlotImage.lsp 옆에서 PlotImage.dll 자동 검색, 또는 전체 경로 지정
(setq pimg:t0 nil)   ; 타이머: 마지막 tick 시각 (ms)
(setq pimg:tm nil)   ; 타이머: (라벨 ms) 목록
(setq pimg:job nil)  ; PP 단계 간 전달용 (1->2단계: 파일경로 p1 p2 리본상태 선택셋 / 2->3단계: 파일경로)

;;------------------------------
;;  보조 함수
;;------------------------------

;; 타이머: tick 으로 기준 시각을 잡고, tock 으로 직전 tick 이후 시간을 라벨에 기록
(defun pimg:tick ()
  (setq pimg:t0 (getvar "MILLISECS"))
)

(defun pimg:tock (label)
  (setq pimg:tm (cons (list label (- (getvar "MILLISECS") pimg:t0)) pimg:tm))
  (pimg:tick)
)

(defun pimg:report (/ total)
  (setq total 0)
  (princ "\n[PlotImage] 단계별 시간")
  (foreach item (reverse pimg:tm)
    (setq total (+ total (cadr item)))
    (princ (strcat "\n  " (car item) ": " (itoa (cadr item)) " ms"))
  )
  (princ (strcat "\n  합계 (클릭/저장창 제외): " (itoa total) " ms"))
)

;; 현재 활성 문서
(defun pimg:doc ()
  (vla-get-activedocument (vlax-get-acad-object))
)

;; 선택셋 전체 객체의 바운딩박스를 (최소점 최대점) 형태로 반환, 실패 시 nil
(defun pimg:ssbbox (ss / i ent obj min-pt max-pt min-x min-y max-x max-y found res)
  (setq i 0)
  (setq found nil)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq res (vl-catch-all-apply 'vla-getboundingbox (list obj 'min-pt 'max-pt)))
    (if (not (vl-catch-all-error-p res))
      (progn
        (setq min-pt (vlax-safearray->list min-pt))
        (setq max-pt (vlax-safearray->list max-pt))
        (if found
          (progn
            (if (< (car min-pt) min-x) (setq min-x (car min-pt)))
            (if (< (cadr min-pt) min-y) (setq min-y (cadr min-pt)))
            (if (> (car max-pt) max-x) (setq max-x (car max-pt)))
            (if (> (cadr max-pt) max-y) (setq max-y (cadr max-pt)))
          )
          (progn
            (setq min-x (car min-pt))
            (setq min-y (cadr min-pt))
            (setq max-x (car max-pt))
            (setq max-y (cadr max-pt))
            (setq found T)
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (if found
    (list (list min-x min-y) (list max-x max-y))
  )
)

;; 저장 대화상자로 PNG 경로를 받음 (기본값: 도면명-배치명.png), 취소 시 nil
(defun pimg:ask-path (doc / dwg-full dwg-dir dwg-base lay-name)
  (setq dwg-full (vla-get-fullname doc))
  (setq dwg-dir (vl-filename-directory dwg-full))
  (setq dwg-base (vl-filename-base dwg-full))
  (setq lay-name (vla-get-name (vla-get-activelayout doc)))
  (getfiled "PNG로 저장" (strcat dwg-dir "\\" dwg-base "-" lay-name) "png" 1)
)

;; PNGOUT이 남기는 plot.log 찌꺼기 파일 정리
(defun pimg:clean-log (doc / log-path)
  (setq log-path (findfile "plot.log"))
  (if log-path
    (vl-catch-all-apply 'vl-file-delete (list log-path))
  )
  (vl-catch-all-apply
    'vl-file-delete
    (list (strcat (vl-filename-directory (vla-get-fullname doc)) "\\plot.log"))
  )
)

;; 이름만 주는 명령 하나를 실행, 실패해도 오류를 던지지 않음
;; ('command 를 vl-catch-all-apply 로 직접 넘기면 "잘못된 함수" 오류가 남)
(defun pimg:safe-command (cmd-name)
  (vl-catch-all-apply (function (lambda () (command cmd-name))) nil)
)

;; 시스템 변수/화면 원상복구 (정상 종료 경로와 *error* 가 같이 사용)
;; 창 최대화는 일부러 복구하지 않음
(defun pimg:restore (filedia-orig cmdecho-orig disp bg-orig zoomed screen-on ribbon-orig vtenable-orig)
  (if filedia-orig (setvar "FILEDIA" filedia-orig))
  (if (and disp bg-orig)
    (vl-catch-all-apply 'vla-put-graphicswinmodelbackgrndcolor (list disp bg-orig))
  )
  (if zoomed
    (vl-catch-all-apply 'vla-zoomprevious (list (vlax-get-acad-object)))
  )
  (if vtenable-orig (setvar "VTENABLE" vtenable-orig))
  (if screen-on
    (progn
      (pimg:safe-command "CLEANSCREENOFF")
      ;; 클린스크린이 꺼버린 리본 복구 (원래 켜져 있었을 때만). command 로 바로 실행하면
      ;; 클린스크린 해제와 겹쳐 무시되므로, LISP 가 끝난 뒤 실행되게 SendCommand 사용
      (if (not (equal ribbon-orig 0))
        (vl-catch-all-apply 'vla-sendcommand (list (pimg:doc) "_.RIBBON "))
      )
      (pimg:safe-command "COMMANDLINE")
    )
  )
  (if cmdecho-orig (setvar "CMDECHO" cmdecho-orig))
)

;;------------------------------
;;  PNG 자동 트림
;;  파이썬/DLL 불필요 - Windows 기본 PowerShell + .NET 사용
;;------------------------------

;; trim.ps1 스크립트 내용을 지정 경로에 기록
;; 1단계: 밝기 기준으로 흰 여백 바깥을 잘라냄 (테두리 선 포함한 바운딩박스)
;; 2단계: 잘라낸 이미지의 상하좌우 중앙선을 안쪽으로 훑어 테두리 선 두께를 구하고,
;;        그만큼 한 번 더 깎아서 테두리 선 자체도 제거
;; 속도: 픽셀을 LockBits 로 한 번에 읽어 C# 에서 분석하고, 컴파일한 DLL 을
;;       %APPDATA%\dodo\pimg\AutoTrim_v2.dll 에 캐시해 다음 실행부터 컴파일을 건너뜀
;;       (C# 코드를 바꾸면 DLL 이름의 버전 번호를 올릴 것)
(defun pimg:write-trim-script (path / f lines)
  (setq lines
    (list
      "param("
      "    [string]$InPath,"
      "    [string]$OutPath,"
      "    [int]$Threshold = 250"
      ")"
      "Add-Type -AssemblyName System.Drawing"
      "$src = @'"
      "using System.Drawing;"
      "using System.Drawing.Imaging;"
      "using System.Runtime.InteropServices;"
      "public static class AutoTrim {"
      "    static bool Dark(byte[] px, int stride, int x, int y, int t) {"
      "        int i = y * stride + x * 4;"
      "        return (px[i] + px[i + 1] + px[i + 2]) / 3 < t;"
      "    }"
      "    public static int[] Analyze(Bitmap bmp, int threshold) {"
      "        int w = bmp.Width, h = bmp.Height;"
      "        BitmapData d = bmp.LockBits(new Rectangle(0, 0, w, h), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb);"
      "        int stride = d.Stride;"
      "        byte[] px = new byte[stride * h];"
      "        Marshal.Copy(d.Scan0, px, 0, px.Length);"
      "        bmp.UnlockBits(d);"
      "        int minX = w, maxX = -1, minY = h, maxY = -1;"
      "        for (int y = 0; y < h; y++) {"
      "            for (int x = 0; x < w; x++) {"
      "                if (Dark(px, stride, x, y, threshold)) {"
      "                    if (x < minX) { minX = x; }"
      "                    if (x > maxX) { maxX = x; }"
      "                    if (y < minY) { minY = y; }"
      "                    if (y > maxY) { maxY = y; }"
      "                }"
      "            }"
      "        }"
      "        int[] r = new int[8];"
      "        r[0] = minX; r[1] = minY; r[2] = maxX; r[3] = maxY;"
      "        if (maxX < minX || maxY < minY) { return r; }"
      "        int w1 = maxX - minX + 1, h1 = maxY - minY + 1;"
      "        int midX = minX + w1 / 2, midY = minY + h1 / 2;"
      "        int left = 0, top = 0, right = 0, bottom = 0;"
      "        while (top < h1 && Dark(px, stride, midX, minY + top, threshold)) { top++; }"
      "        while (bottom < h1 && Dark(px, stride, midX, maxY - bottom, threshold)) { bottom++; }"
      "        while (left < w1 && Dark(px, stride, minX + left, midY, threshold)) { left++; }"
      "        while (right < w1 && Dark(px, stride, maxX - right, midY, threshold)) { right++; }"
      "        r[4] = left; r[5] = top; r[6] = right; r[7] = bottom;"
      "        return r;"
      "    }"
      "}"
      "'@"
      "$dll = Join-Path $env:APPDATA 'dodo\\pimg\\AutoTrim_v2.dll'"
      "if (-not (Test-Path $dll)) {"
      "    try {"
      "        [void][System.IO.Directory]::CreateDirectory((Split-Path $dll))"
      "        Add-Type -TypeDefinition $src -ReferencedAssemblies System.Drawing -OutputAssembly $dll"
      "    } catch {}"
      "}"
      "if (Test-Path $dll) { try { Add-Type -Path $dll } catch {} }"
      "if (-not ('AutoTrim' -as [type])) { Add-Type -TypeDefinition $src -ReferencedAssemblies System.Drawing }"
      ""
      "$bmp = [System.Drawing.Bitmap]::FromFile($InPath)"
      "$r = [AutoTrim]::Analyze($bmp, $Threshold)"
      "$minX = $r[0]; $minY = $r[1]; $maxX = $r[2]; $maxY = $r[3]"
      ""
      "if ($maxX -ge $minX -and $maxY -ge $minY) {"
      "    $w1 = $maxX - $minX + 1"
      "    $h1 = $maxY - $minY + 1"
      "    $w2 = $w1 - $r[4] - $r[6]"
      "    $h2 = $h1 - $r[5] - $r[7]"
      "    if ($w2 -gt 0 -and $h2 -gt 0) {"
      "        $rect = New-Object System.Drawing.Rectangle(($minX + $r[4]), ($minY + $r[5]), $w2, $h2)"
      "        $code = 'OK'"
      "    } else {"
      "        $rect = New-Object System.Drawing.Rectangle($minX, $minY, $w1, $h1)"
      "        $code = 'OK-NOINSET'"
      "    }"
      "    $final = $bmp.Clone($rect, $bmp.PixelFormat)"
      "    $bmp.Dispose()"
      "    $final.Save($OutPath, [System.Drawing.Imaging.ImageFormat]::Png)"
      "    $final.Dispose()"
      "    $code"
      "} else {"
      "    $bmp.Dispose()"
      "    'NOCONTENT'"
      "}"
    )
  )
  (setq f (open path "w"))
  (foreach ln lines (write-line ln f))
  (close f)
  path
)

;; WScript.Shell로 명령 실행, 끝날 때까지 대기, 종료코드 반환
(defun pimg:run-wait (cmdline / wsh code)
  (setq wsh (vlax-create-object "WScript.Shell"))
  (setq code (vlax-invoke-method wsh 'run cmdline 0 :vlax-true))
  (vlax-release-object wsh)
  code
)

;; PlotImage.dll 위치: pimg:dll 지정값, 없으면 PlotImage.lsp 폴더 -> %APPDATA%\dodo\pimg
;; 존재하는 전체 경로, 없으면 nil
(defun pimg:dll-path (/ lsp-path dirs)
  (if pimg:dll
    (findfile pimg:dll)
    (progn
      (if (setq lsp-path (findfile "PlotImage.lsp"))
        (setq dirs (list (vl-filename-directory lsp-path)))
      )
      (setq dirs (append dirs (list (if (and (boundp '*loader-dir*) *loader-dir*) *loader-dir* "") (strcat (getenv "USERPROFILE") "\\Documents\\LISP") (strcat (getenv "APPDATA") "\\dodo\\pimg"))))
      (vl-some '(lambda (d) (findfile (strcat d "\\PlotImage.dll"))) dirs)
    )
  )
)

;; (pimg-trim) 를 쓸 수 있게 DLL 을 세션당 한 번 NETLOAD. 준비되면 T, 아니면 nil
(defun pimg:engine-ready (/ dll-path)
  (cond
    ((vl-symbol-value 'pimg-trim) T)
    ((setq dll-path (pimg:dll-path))
     (vl-catch-all-apply (function (lambda () (command "_.NETLOAD" dll-path))) nil)
     (if (vl-symbol-value 'pimg-trim) T)
    )
  )
)

;; png-path 이미지의 흰 여백과 테두리 선을 잘라 같은 경로에 덮어씀
;; DLL 이 있으면 AutoCAD 안에서 바로 처리, 없으면 PowerShell 로 처리
(defun pimg:trim-png (png-path)
  (if (pimg:engine-ready)
    (vl-catch-all-apply 'pimg-trim (list png-path 250))
    (pimg:trim-png-ps png-path)
  )
)

;; PowerShell 방식 (DLL 이 없을 때). 스크립트는 실행이 끝난 뒤에 지움 (삭제 타이밍 문제 방지)
(defun pimg:trim-png-ps (png-path / script-path cmdline code)
  (setq script-path (vl-filename-mktemp "trim_.ps1"))
  (pimg:write-trim-script script-path)
  (setq cmdline
    (strcat
      "powershell.exe -NoProfile -ExecutionPolicy Bypass -File "
      (chr 34) script-path (chr 34)
      " -InPath " (chr 34) png-path (chr 34)
      " -OutPath " (chr 34) png-path (chr 34)
    )
  )
  (setq code (vl-catch-all-apply 'pimg:run-wait (list cmdline)))
  (vl-catch-all-apply 'vl-file-delete (list script-path))
  code
)

;;------------------------------
;;  pimg:run-job  -  PP 2단계: 화면 정리가 끝난 뒤 실제 내보내기
;;  (1단계 c:PP 가 SendCommand 로 예약해 LISP 종료 후 실행됨)
;;------------------------------

(defun pimg:run-job (/ *error* png-path p1 p2 ribbon-orig doc acad disp bg-orig bg-result
                     export-ss filedia-orig cmdecho-orig vtenable-orig zoomed cmd-result)

  (defun *error* (msg)
    (pimg:restore filedia-orig cmdecho-orig disp bg-orig zoomed T ribbon-orig vtenable-orig)
    (if (not (wcmatch (strcase msg t) "*cancel*,*exit*,*취소*,*종료*"))
      (princ (strcat "\n[PlotImage] 오류: " msg))
    )
    (princ)
  )

  (pimg:tock "명령행/클린스크린 반영 대기")
  (setq png-path (nth 0 pimg:job))
  (setq p1 (nth 1 pimg:job))
  (setq p2 (nth 2 pimg:job))
  (setq ribbon-orig (nth 3 pimg:job))
  (setq export-ss (nth 4 pimg:job))
  (setq pimg:job nil)

  (setq doc (pimg:doc))
  (setq acad (vlax-get-acad-object))

  ;; 가이드 테두리도 내보내기 대상에 일부러 포함 (트림 단계의 기준선), 선택셋은 1단계에서 전달받음

  ;; 배경 흰색 전환 (끝나면 복구)
  (setq disp (vla-get-display (vla-get-preferences acad)))
  (setq bg-result (vl-catch-all-apply 'vla-get-graphicswinmodelbackgrndcolor (list disp)))
  (if (not (vl-catch-all-error-p bg-result))
    (setq bg-orig bg-result)
  )
  (vl-catch-all-apply 'vla-put-graphicswinmodelbackgrndcolor (list disp 16777215))  ; 흰색

  (setq filedia-orig (getvar "FILEDIA"))
  (setq cmdecho-orig (getvar "CMDECHO"))
  (setvar "FILEDIA" 0)
  (setvar "CMDECHO" 0)

  ;; ZOOM 부드러운 전환 애니메이션 끄기 (없는 버전이면 건너뜀)
  (setq vtenable-orig (vl-catch-all-apply 'getvar (list "VTENABLE")))
  (if (vl-catch-all-error-p vtenable-orig)
    (setq vtenable-orig nil)
    (setvar "VTENABLE" 0)
  )
  (pimg:tock "배경/시스템변수 설정")

  (vla-zoomwindow
    acad
    (vlax-3d-point (list (car p1) (cadr p1) 0.0))
    (vlax-3d-point (list (car p2) (cadr p2) 0.0))
  )
  (setq zoomed T)
  (pimg:tock "ZOOM")

  (setq cmd-result
    (vl-catch-all-apply
      (function (lambda () (command "PNGOUT" png-path export-ss "")))
      nil
    )
  )

  (pimg:tock "PNGOUT")

  (pimg:restore filedia-orig cmdecho-orig disp bg-orig zoomed T ribbon-orig vtenable-orig)
  (pimg:tock "원상복구")
  (pimg:clean-log doc)
  (pimg:tock "plot.log 정리")

  (cond
    ((vl-catch-all-error-p cmd-result)
     (princ (strcat "\n[PlotImage] PNGOUT 실행 중 오류: " (vl-catch-all-error-message cmd-result)))
    )
    ((not (findfile png-path))
     (princ "\n[PlotImage] 실행은 끝났지만 파일이 생기지 않았습니다. 위 명령행 메시지를 확인하세요.")
    )
    (t
     (princ (strcat "\n[PlotImage] 내보내기 완료: " png-path))
     ;; 리본 복구(위에서 SendCommand 로 예약됨)가 끝난 뒤에 트림하도록 뒤이어 예약
     (setq pimg:job (list png-path))
     (pimg:tick)
     (vla-sendcommand doc "(pimg:trim-job)\n")
    )
  )
  (princ)
)

;;------------------------------
;;  pimg:trim-job  -  PP 3단계: 리본 복구가 끝난 뒤 이미지 트림
;;------------------------------

(defun pimg:trim-job (/ png-path trim-code)
  (pimg:tock "리본 복구 대기")
  (setq png-path (nth 0 pimg:job))
  (setq pimg:job nil)
  (princ "\n[PlotImage] 여백+테두리 자동 트림 중...")
  (setq trim-code (pimg:trim-png png-path))
  (pimg:tock "이미지 트림")
  (cond
    ((vl-catch-all-error-p trim-code)
     (princ (strcat "\n[PlotImage] 트림 실패: " (vl-catch-all-error-message trim-code)))
    )
    ((and (= (type trim-code) 'STR) (wcmatch trim-code "ERR*"))
     (princ (strcat "\n[PlotImage] 트림 실패: " trim-code))
    )
    (t
     (princ "\n[PlotImage] 트림 완료.")
    )
  )
  (pimg:report)
  (princ)
)

;;------------------------------
;;  c:PP  -  가이드 테두리 기준 PNG 내보내기 (1단계: 선택/경로/화면 정리 예약)
;;------------------------------

;; command 로 CLEANSCREENON 등을 바로 실행하면 LISP 가 끝나기 전에는 화면 리사이즈가
;; 반영되지 않아 해상도가 낮게 나옴 (수동으로 미리 정리하고 실행하면 높게 나옴).
;; 그래서 SendCommand 로 "CLEANSCREENON -> pimg:run-job" 을 LISP 종료 후 순서대로 실행시킴
(defun c:PP (/ doc ss bbox p1 p2 export-ss png-path ribbon-orig)
  (setq doc (pimg:doc))
  (princ "\n[PlotImage] 가이드 경계가 되는 객체(사각형 선/폴리라인 등)를 선택하세요.")
  (setq ss (ssget))

  (cond
    ((null ss)
     (princ "\n[PlotImage] 선택된 객체가 없어 취소되었습니다.")
    )
    ((null (setq bbox (pimg:ssbbox ss)))
     (princ "\n[PlotImage] 선택한 객체에서 경계 상자를 구할 수 없습니다.")
    )
    ((progn
       (setq p1 (car bbox))
       (setq p2 (cadr bbox))
       (setq export-ss (ssget "W" p1 p2))
       (or (null export-ss) (= (sslength export-ss) 0))
     )
     (princ "\n[PlotImage] 내보낼 객체가 없습니다.")
    )
    ((null (setq png-path (pimg:ask-path doc)))
     (princ "\n[PlotImage] 파일명을 지정하지 않아 취소되었습니다.")
    )
    (t
     (vl-catch-all-apply 'vl-file-delete (list png-path))

     (setq ribbon-orig (vl-catch-all-apply 'getvar (list "RIBBONSTATE")))
     (if (vl-catch-all-error-p ribbon-orig)
       (setq ribbon-orig nil)
     )
     (setq pimg:job (list png-path p1 p2 ribbon-orig export-ss))
     (setq pimg:tm nil)
     (pimg:tick)

     (if (/= (vla-get-windowstate (vlax-get-acad-object)) 3)  ; 이미 최대화면 건너뜀
       (vl-catch-all-apply 'vla-put-windowstate (list (vlax-get-acad-object) 3))  ; acMax
     )
     ;; COMMANDLINEHIDE 는 SendCommand 로 보내면 확인창이 뜨므로 LISP 에서 직접 실행
     (pimg:safe-command "COMMANDLINEHIDE")
     (vla-sendcommand doc "CLEANSCREENON\n(pimg:run-job)\n")
    )
  )
  (princ)
)

;; DLL 미리 로드: 첫 PP 에서 NETLOAD 시간이 걸리지 않게 함 (실패해도 PP 실행 시 다시 시도)
(setq pimg:echo (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(vl-catch-all-apply (function (lambda () (pimg:engine-ready))) nil)
(setvar "CMDECHO" pimg:echo)
(setq pimg:echo nil)

(princ)