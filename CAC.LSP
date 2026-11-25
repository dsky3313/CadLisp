;;; ------------------------------------------------------------------------
;;;	CopyAlongCurve.lsp v1.1
;;;
;;;	Copyright(c) 03.26.10
;;;	Alan J. Thompson (alanjt)
;;;
;;;	Contact: alanjt @ TheSwamp.org, CADTutor.net
;;;
;;;	Permission to use, copy, modify, and distribute this software
;;;	for any purpose and without fee is hereby granted, provided
;;;	that the above copyright notice appears in all copies and
;;;	that both that copyright notice and the limited warranty and
;;;	restricted rights notice below appear in all supporting
;;;	documentation.
;;;
;;;	The following program(s) are provided "as is" and with all faults.
;;;	Alan J. Thompson DOES NOT warrant that the operation of the program(s)
;;;	will be uninterrupted and/or error free.
;;;
;;;	Allows user to copy selected object(s) along selected curve or picked points,
;;;	based on three different copying options.
;;;	 @ Divide: Divides object(s) evenly along curve.
;;;	 @ Dynamic: Copies along curve at specified distance (can be changed dynamically).
;;;	 @ Measure: Copies along entire curve at defined distance.
;;;
;;;	Revision History:
;;;
;;;	v1.1 (03.28.10) 1. Added subroutines: _angle _cmr _Points _PLine
;;;			2. Added option to rotate object(s) along selected curve.
;;;			3. Added additional rotation option (after copying completed)
;;;			   with values of 90¡Æ, 180¡Æ, 270¡Æ.
;;;			4. Added option to pick points for curve instead of selecting object.
;;;
;;; ------------------------------------------------------------------------

(defun c:CAC (/) (c:CopyAlongCurve))
(defun c:CopyAlongCurve (/ *error* AT:Entsel AT:DrawX AT:ClosestEndPoint _angle _cmr _Points _PLine
                         #SS #Pnt #Ent #PLine #Obj #Num #Seg #Add #Dist #Val #Cnt #TempPnt #TempObj
                         #Temp #List #Rot #Ang
                        )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUBROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; error handler
  (defun *error* (#Message)
    (redraw)
    (and #PLine
         (if (vl-catch-all-error-p (vl-catch-all-apply (function vla-delete) (list #PLine)))
           (alert "Temporary LWPolyline could not be deleted!")
         ) ;_ if
    ) ;_ and
    (and *AcadDoc* (vla-endundomark *AcadDoc*))
    (and #Message
         (not (wcmatch (strcase #Message) "*BREAK*,*CANCEL*,*QUIT*"))
         (princ (strcat "\nError: " #Message))
    ) ;_ and
  ) ;_ defun




;;; Entsel or NEntsel with options
;;; #Nested - Entsel or Nentsel (T for Nentsel, nil for Entsel)
;;; #Message - Selection message (if nil, "\nSelect object: " is used)
;;; #FilterList - DXF ssget style filtering (nil if not required)
;;;               "V" as first item in list to convert object to VLA-OBJECT (must be in list if no DXF filtering)
;;;               "L" as first item in list to ignore locked layers (must be in list if no DXF filtering)
;;; #Keywords - Keywords to match instead of object selection (nil if not required)
;;; Example: (AT:Entsel nil "\nSelect MText not on 0 layer [Settings]: " '("LV" (0 . "MTEXT")(8 . "~0")) "Settings")
;;; Example: (AT:Entsel T "\nSelect object [Settings]: " '("LV") "Settings")
;;; Alan J. Thompson, 04.16.09
;;; Updated: Alan J. Thompson, 06.04.09 (changed filter coding to work as ssget style dxf filtering)
;;; Updated: Alan J. Thompson, 09.07.09 (added option to ignore locked layers and convert object to VLA-OBJECT
;;; Updated: Alan J. Thompson, 09.18.09 (fixed 'missed pick' alert)
  (defun AT:Entsel (#Nested #Message #FilterList #Keywords / #Count #Message #Choice #Ent
                    #VLA&Locked #FilterList
                   )
    (vl-load-com)
    (setvar "errno" 0)
    (setq #Count 0)
    ;; fix message
    (or #Message (setq #Message "\nSelect object: "))
    ;; set entsel/nentsel
    (if #Nested
      (setq #Choice nentsel)
      (setq #Choice entsel)
    ) ;_ if
    ;; check if option to convert to vla-object or ignore locked layers in #FilterList variable
    (and (vl-consp #FilterList)
         (eq (type (car #FilterList)) 'STR)
         (setq #VLA&Locked (car #FilterList)
               #FilterList (cdr #FilterList)
         ) ;_ setq
    ) ;_ and
    ;; select object
    (while (and (not #Ent) (/= (getvar "errno") 52))
      ;; if keywords
      (and #Keywords (initget #Keywords))
      (cond
        ((setq #Ent (#Choice #Message))
         ;; if ignore locked layers
         (and #VLA&Locked
              (vl-consp #Ent)
              (wcmatch (strcase #VLA&Locked) "*L*")
              (not
                (zerop
                  (cdr (assoc 70
                              (entget (tblobjname
                                        "layer"
                                        (cdr (assoc 8 (entget (car #Ent))))
                                      ) ;_ tblobjname
                              ) ;_ entget
                       ) ;_ assoc
                  ) ;_ cdr
                ) ;_ zerop
              ) ;_ not
              (setq #Ent nil
                    #Flag T
              ) ;_ setq
         ) ;_ and
         ;; #FilterList check
         (if (and #FilterList (vl-consp #Ent))
           ;; process filtering from #FilterList
           (or
             (not
               (member
                 nil
                 (mapcar '(lambda (x)
                            (wcmatch
                              (strcase
                                (vl-princ-to-string
                                  (cdr (assoc (car x) (entget (car #Ent))))
                                ) ;_ vl-princ-to-string
                              ) ;_ strcase
                              (strcase (vl-princ-to-string (cdr x)))
                            ) ;_ wcmatch
                          ) ;_ lambda
                         #FilterList
                 ) ;_ mapcar
               ) ;_ member
             ) ;_ not
             (setq #Ent nil
                   #Flag T
             ) ;_ setq
           ) ;_ or
         ) ;_ if
        )
      ) ;_ cond
      (and (or (= (getvar "errno") 7) #Flag)
           (/= (getvar "errno") 52)
           (not #Ent)
           (setq #Count (1+ #Count))
           (prompt (strcat "\nNope, keep trying!  "
                           (itoa #Count)
                           " missed pick(s)."
                   ) ;_ strcat
           ) ;_ prompt
      ) ;_ and
    ) ;_ while
    (if (and (vl-consp #Ent)
             #VLA&Locked
             (wcmatch (strcase #VLA&Locked) "*V*")
        ) ;_ and
      (vlax-ename->vla-object (car #Ent))
      #Ent
    ) ;_ if
  ) ;_ defun



;;; Draw and "X" vector at specified point
;;; P - Placement point for "X"
;;; C - Color of "X" (must be integer b/w 1 & 255)
;;; Alan J. Thompson, 10.31.09 / 03.26.10
  (defun AT:DrawX (P C / d)
    (if (and (vl-consp P)
             (setq d (* (getvar "VIEWSIZE") 0.02))
        ) ;_ and
      (progn (grvecs (cons C
                           (mapcar
                             (function (lambda (#) (trans (polar P (* # pi) d) 0 1)))
                             '(0.25 1.25 0.75 1.75)
                           ) ;_ mapcar
                     ) ;_ cons
             ) ;_ grvecs
             P
      ) ;_ progn
    ) ;_ if
  ) ;_ defun



;;; Retrieve closest end point on object
;;; #EntPnt - List with object and point
;;; Alan J. Thompson, 11.10.09
  (defun AT:ClosestEndPoint (#EntPnt)
    (if (vl-consp #EntPnt)
      (car (vl-sort
             (list (vlax-curve-getstartpoint (car #EntPnt))
                   (vlax-curve-getendpoint (car #EntPnt))
             ) ;_ list
             (function
               (lambda (a b)
                 (< (distance (trans (cadr #EntPnt) 1 0) a) (distance (trans (cadr #EntPnt) 1 0) b))
               ) ;_ lambda
             ) ;_ function
           ) ;_ vl-sort
      ) ;_ car
    ) ;_ if
  ) ;_ defun



  (setq _angle (lambda (P O / _pt c p2)
                 (setq _pt (lambda (s)
                             (vlax-curve-getPointAtDist O (s (vlax-curve-getDistAtPoint O P) 0.00001))
                           ) ;_ lambda
                 ) ;_ setq
                 (if (and (vl-consp P)
                          (or (setq p2 (_pt +)) (setq p2 (setq c (_pt -))))
                     ) ;_ and
                   (if c
                     (+ pi (angle P p2))
                     (angle P p2)
                   ) ;_ if
                 ) ;_ if
               ) ;_ lambda
  ) ;_ setq


  (setq _cmr (lambda (o p2)
               (vla-move (setq #TempObj (vla-copy o)) #Pnt (setq #TempPnt (vlax-3D-point p2)))
               (setq #List (cons (cons #TempObj #TempPnt) #List))
               (and (eq *CAC:Align* "Yes") (vla-rotate #TempObj #TempPnt (_angle p2 #Obj)))
               #TempObj
             ) ;_ lambda
  ) ;_ setq



  (setq _Points (lambda (/ l p)
                  (if (eq "Points" #Ent)
                    (if (car (setq l (list (getpoint "\n¡¼CAC¡½ Specify first point: "))))
                      (progn
                        (while (setq p (getpoint (car l) "\n¡¼CAC¡½ Specify next point: "))
                          (grdraw (car (setq l (cons p l))) (cadr l) 3 -1)
                        ) ;_ while
                        (and (> (length l) 1) (_PLine l))
                      ) ;_ progn
                    ) ;_ if
                    T
                  ) ;_ if
                ) ;_ lambda
  ) ;_ setq



  (setq _PLine (lambda (l / p)
                 (if (vl-consp l)
                   (progn
                     (setq p    (entmakex (append (list '(0 . "LWPOLYLINE")
                                                        '(100 . "AcDbEntity")
                                                        '(100 . "AcDbPolyline")
                                                        '(8 . "0")
                                                        '(62 . 3)
                                                        (cons 90 (length l))
                                                  ) ;_ list
                                                  (mapcar (function (lambda (x) (cons 10 (trans x 1 0)))) l)
                                          ) ;_ append
                                ) ;_ entmakex
                           #Ent (list p (last l))
                     ) ;_ setq
                     (not (vla-highlight (setq #PLine (vlax-ename->vla-object p)) :vlax-true))
                   ) ;_ progn
                 ) ;_ if
               ) ;_ lambda
  ) ;_ setq






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (vl-load-com)

  (or *CAC:Option* (setq *CAC:Option* "Divide"))
  (or *CAC:Align* (setq *CAC:Align* "No"))

  (or *AcadDoc* (setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object))))
  (vla-startundomark *AcadDoc*)
  (redraw)

  (initget 0 "Divide dYnamic Measure")
  (cond
    ((and (setq *CAC:Option*
                 (cond ((getkword
                          (strcat "\nCopy Along Curve\nSpecify copy option [Divide/dYnamic/Measure] <"
                                  *CAC:Option*
                                  ">: "
                          ) ;_ strcat
                        ) ;_ getkword
                       )
                       (*CAC:Option*)
                 ) ;_ cond
          ) ;_ setq
          (princ (strcat "\nSelect object(s) to " *CAC:Option* " along curve: "))
          (setq #SS (progn (princ "\n¡¼CAC¡½ ") (ssget "_:L")))
          (setq #Pnt (getpoint "\n¡¼CAC¡½ Specify base point: "))
          (setq #Ent (AT:Entsel nil
                                "\nSelect curve or specify points [Points]: "
                                '((0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE"))
                                "Points"
                     ) ;_ AT:Entsel
          ) ;_ setq
          (_Points)
          (vl-consp #Ent)
          (setq #Obj (vlax-ename->vla-object (car #Ent)))
     ) ;_ and
     (setq #Len  (vlax-curve-GetDistAtParam #Obj (vlax-curve-GetEndParam #Obj))
           #Pnt  (vlax-3d-point (trans #Pnt 1 0))
           #Dist 0.
     ) ;_ setq
     (if (equal (AT:DrawX (AT:ClosestEndPoint #Ent) 1) (vlax-curve-getStartPoint #Obj))
       (setq #Val 0.)
       (setq #Val #Len)
     ) ;_ if
     (not (initget 0 "Yes No"))
     (setq *CAC:Align*
            (cond
              ((getkword (strcat "\n¡¼CAC¡½ Align object(s) along curve? [Yes/No] <" *CAC:Align* ">: ")))
              (*CAC:Align*)
            ) ;_ cond
     ) ;_ setq
     (initget 6)
     (cond

       ;; Divide
       ((and (eq *CAC:Option* "Divide") (setq #Num (getint "\n¡¼CAC¡½ Specify number of objects: ")))
        (setq #Cnt 0)
        (if (or (vl-position (vla-get-objectname #Obj) '("AcDbCircle" "AcDbEllipse"))
                (and (eq (vla-get-objectname #Obj) "AcDbPolyline")
                     (eq (vla-get-closed #Obj) :vlax-true)
                ) ;_ and
            ) ;_ or
          (setq #Add 0)
          (setq #Add 1)
        ) ;_ if
        (while (and (<= #Dist (- #Len (/ #Len (+ #Add #Num)))) (> #Num #Cnt))
          (setq #Dist (+ #Dist (/ #Len (+ #Add #Num)))
                #Cnt  (1+ #Cnt)
          ) ;_ setq
          (vlax-for x (setq #SS (vla-get-activeselectionset *AcadDoc*))
            (_cmr x (vlax-curve-getpointatdist #Obj #Dist))
          ) ;_ vlax-for
        ) ;_ while
       )

       ;; Measure
       ((and (eq *CAC:Option* "Measure") (setq #Seg (getdist "\n¡¼CAC¡½ Specify length of segment: ")))
        (while (<= #Dist (- #Len #Seg))
          (setq #Dist (+ #Dist #Seg))
          (vlax-for x (setq #SS (vla-get-activeselectionset *AcadDoc*))
            (_cmr x (vlax-curve-getpointatdist #Obj (abs (- #Val #Dist))))
          ) ;_ vlax-for
        ) ;_ while
       )

       ;; Dynamic
       ((and (eq *CAC:Option* "dYnamic") (setq #Seg 0.))
        (while (and (numberp #Seg) (<= #Dist #Len))
          (princ "\n*")
          (initget 6 "Exit X")
          (and
            (setq #Seg (cond ((getdist (strcat "\n¡¼CAC¡½ Total Dist: "
                                               (rtos #Len)
                                               " - Length left: "
                                               (rtos (- #Len #Dist))
                                               "\nDistance to copy [Exit] <"
                                               (rtos #Seg)
                                               ">: "
                                       ) ;_ strcat
                              ) ;_ getdist
                             )
                             (#Seg)
                       ) ;_ cond
            ) ;_ setq
            (numberp #Seg)
            (not (zerop #Seg))
            (setq #Temp (vlax-curve-getpointatdist #Obj (abs (- #Val (setq #Dist (+ #Dist #Seg))))))
            (vlax-for x (setq #SS (vla-get-activeselectionset *AcadDoc*)) (_cmr x #Temp))
          ) ;_ and
        ) ;_ while
       )

     ) ;_ cond

     ;; additional rotation for objects: 90¡Æ, 180¡Æ, 270¡Æ
     (and
       #List
       (eq *CAC:Align* "Yes")
       (while
         (and (not (initget "Yes No 1 2 3"))
              (setq #Rot (getkword "\n¡¼CAC¡½ Additional rotation? 1=90¡Æ, 2=180¡Æ, 3=270¡Æ [1/2/3/No] <No>: "))
              (not (eq #Rot "No"))
         ) ;_ and
          (if (cond ((eq #Rot "1") (setq #Ang (* pi 0.5)))
                    ((vl-position #Rot '("2" "Yes")) (setq #Ang pi))
                    ((eq #Rot "3") (setq #Ang (* pi 1.5)))
              ) ;_ cond
            (foreach x #List (vla-rotate (car x) (cdr x) #Ang))
          ) ;_ if
       ) ;_ while
     ) ;_ and

     (vl-catch-all-apply 'vla-delete (list #SS))
    )
  ) ;_ cond
  (*error* nil)
  (princ)
) ;_ defun