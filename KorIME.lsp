;;; KorIME.lsp - AutoCAD 2022
;;; 1) Loads KorIME.dll (automatic IME English switching, like ZWCAD SMARTINPUT)
;;; 2) Safety net: a Hangul command that reaches AutoCAD as "unknown command"
;;;    is converted to the QWERTY keys that were typed (2-set Dubeolsik) and re-sent.
;;;      e.g. (kime-convert "<hangul for LINE>") -> "LINE"
;;;
;;; This file is ASCII only on purpose; Hangul is handled by Unicode code points.
;;; Requires LISPSYS = 1 (AutoCAD 2021+ default) for Unicode strings.

(vl-load-com)

;;; ------------------------------------------------------------------
;;; Key tables (Dubeolsik)
;;; ------------------------------------------------------------------

;; Compatibility jamo U+3131 .. U+3163, indexed by (code - 0x3131)
(setq kime:*jamo*
  '("r" "R" "rt" "s" "sw" "sg" "e" "E" "f" "fr" "fa" "fq" "ft" "fx" "fv" "fg"
    "a" "q" "Q" "qt" "t" "T" "d" "w" "W" "c" "z" "x" "v" "g"
    "k" "o" "i" "O" "j" "p" "u" "P" "h" "hk" "ho" "hl" "y" "n" "nj" "np" "nl"
    "b" "m" "ml" "l"))

;; Syllable U+AC00 .. U+D7A3 = 0xAC00 + (cho*21 + jung)*28 + jong
(setq kime:*cho*
  '("r" "R" "s" "e" "E" "f" "a" "q" "Q" "t" "T" "d" "w" "W" "c" "z" "x" "v" "g"))
(setq kime:*jung*
  '("k" "o" "i" "O" "j" "p" "u" "P" "h" "hk" "ho" "hl" "y" "n" "nj" "np" "nl"
    "b" "m" "ml" "l"))
(setq kime:*jong*
  '("" "r" "R" "rt" "s" "sw" "sg" "e" "f" "fr" "fa" "fq" "ft" "fx" "fv" "fg"
    "a" "q" "qt" "t" "T" "d" "w" "c" "z" "x" "v" "g"))

(defun kime:hangul-p (c)
  (or (and (>= c 12593) (<= c 12643))    ; U+3131 .. U+3163
      (and (>= c 44032) (<= c 55203))))  ; U+AC00 .. U+D7A3

(defun kime:char->keys (c / i)
  (cond
    ((and (>= c 12593) (<= c 12643))
     (nth (- c 12593) kime:*jamo*))
    ((and (>= c 44032) (<= c 55203))
     (setq i (- c 44032))
     (strcat (nth (/ i 588) kime:*cho*)
             (nth (/ (rem i 588) 28) kime:*jung*)
             (nth (rem i 28) kime:*jong*)))
    (T (chr c))))

;;; (kime-has-hangul "str") -> T / nil
(defun kime-has-hangul (s)
  (and (= (type s) 'STR)
       (vl-some 'kime:hangul-p (vl-string->list s))))

(defun kime:dll-loaded-p ()
  (= (type kime-status) 'EXRXSUBR))

;;; (kime-convert "str") -> upper-case QWERTY string
;;; Uses the DLL when loaded (works with any LISPSYS); the LISP tables need LISPSYS = 1.
(defun kime-convert (s)
  (if (= (type kime-convert-net) 'EXRXSUBR)
    (kime-convert-net s)
    (strcase (apply 'strcat (mapcar 'kime:char->keys (vl-string->list s))))))

;;; ------------------------------------------------------------------
;;; Unknown-command safety net (LISP-only mode)
;;; When the DLL is loaded it handles unknown commands itself.
;;; ------------------------------------------------------------------

(defun kime:on-unknown (reactor params / cmd eng)
  (setq cmd (car params))
  (if (and (not (kime:dll-loaded-p))
           (= (getvar "LISPSYS") 1)
           (kime-has-hangul cmd)
           (setq eng (kime-convert cmd))
           (not (kime-has-hangul eng)))
    (progn
      (princ (strcat "\nーKIME― -> " eng))
      (vla-SendCommand
        (vla-get-ActiveDocument (vlax-get-acad-object))
        (strcat eng " "))))
  (princ))

(defun kime:install-reactor ()
  (if kime:*reactor* (vlr-remove kime:*reactor*))
  (setq kime:*reactor*
    (vlr-command-reactor "KorIME"
      '((:vlr-unknownCommand . kime:on-unknown)))))

;;; ------------------------------------------------------------------
;;; DLL loader
;;; ------------------------------------------------------------------

(defun kime:dll-path (/ f)
  (cond
    ((and (= (type *kime-dir*) 'STR)
          (findfile (setq f (strcat (vl-string-right-trim "\\/" *kime-dir*) "\\KorIME.dll"))))
     f)
    ((findfile "KorIME.dll"))
    ((setq f (findfile "KorIME.lsp"))
     (findfile (strcat (vl-filename-directory f) "\\KorIME.dll")))))

;; NETLOAD is queued with SendCommand so it runs once AutoCAD is idle:
;; (command) is unreliable while acaddoc.lsp is loading. Calling NETLOAD
;; through (command) inside the queued expression keeps the file dialog off.
;; SendCommand is rejected while a command such as APPLOAD is running; then
;; it is retried from a one-shot reactor right after that command ends.
(defun kime:netload-string (dll)
  (strcat "(command \"_.NETLOAD\" \"" (vl-string-translate "\\" "/" dll) "\")(princ) "))

(defun kime:send (str)
  (not (vl-catch-all-error-p
         (vl-catch-all-apply 'vla-SendCommand
           (list (vla-get-ActiveDocument (vlax-get-acad-object)) str)))))

(defun kime:on-cmd-done (reactor params)
  (vlr-remove reactor)
  (setq kime:*load-reactor* nil)
  (if (and (not (kime:dll-loaded-p)) kime:*pending-dll*)
    (kime:send (kime:netload-string kime:*pending-dll*))))

(defun kime:load-dll (/ dll)
  (cond
    ((kime:dll-loaded-p) T)                          ; already loaded this session
    ((setq dll (kime:dll-path))
     (if (not (kime:send (kime:netload-string dll)))
       (progn
         (setq kime:*pending-dll* dll)
         (if kime:*load-reactor* (vlr-remove kime:*load-reactor*))
         (setq kime:*load-reactor*
           (vlr-command-reactor "KorIMELoader"
             '((:vlr-commandEnded . kime:on-cmd-done)
               (:vlr-commandCancelled . kime:on-cmd-done)
               (:vlr-commandFailed . kime:on-cmd-done))))))
     T)
    (T
     (princ "\nーKIME― KorIME.dll not found. Add its folder to the Support File Search Path, or set *kime-dir*.")
     nil)))

;;; ------------------------------------------------------------------
;;; Init
;;; ------------------------------------------------------------------

(kime:install-reactor)
(if (not (kime:load-dll))
  (if (= (getvar "LISPSYS") 1)
    (princ "\nーKIME― Running in LISP-only mode (Hangul command conversion only).")
    (princ "\nーKIME― DLL not loaded and LISPSYS is 0 - Hangul conversion unavailable.")))
(princ)
