;;문자스타일 하나로 바꾸기
(defun c:cf( / acdoc col styles style nlst )

    
    ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
    ;;           Sub Function - 01              ;;
    ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
    (defun LM:GetAllItems ( collection / result ) (vl-load-com)
        (vlax-for item collection (setq result (cons item result)))
        (reverse result)
    )
    
    ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
    ;;           Sub Function - 02              ;;
    ;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;;
    (defun LM:ListBox ( title data multiple / file tmp dch return ) 
        (cond
            (
                (not
                    (and (setq file (open (setq tmp (vl-filename-mktemp nil nil ".dcl")) "w"))
                        (write-line
                            (strcat 
                                "listbox : dialog { label = \"" 
                                title
                                "\"; spacer; : list_box { key = \"list\"; multiple_select = "
                                (if multiple "true" "false") 
                                "; } spacer; ok_cancel;}"
                            )
                        file
                        )
                        (not (close file)) (< 0 (setq dch (load_dialog tmp))) (new_dialog "listbox" dch)
                    )
                )
            )
            (
                t     
                (start_list "list")
                (mapcar 'add_list data) (end_list)

                (setq return (set_tile "list" "0"))
                (action_tile "list" "(setq return $value)")

                (setq return
                    (if (= 1 (start_dialog))
                        (mapcar '(lambda ( x ) (nth x data)) (read (strcat "(" return ")")))
                    )
                )          
            )
        )
        (if (< 0 dch) (unload_dialog dch))
        (if (setq tmp (findfile tmp)) (vl-file-delete tmp))
        return
    )
    
    
    ;;==========================================;;
    ;;               Main Function              ;;
    ;;==========================================;;
    (setq acdoc  (vla-get-ActiveDocument(vlax-get-acad-object))
          col    (vla-get-textstyles acdoc)
          styles (LM:GetAllItems col)
    )
    
    (setq nlst  (mapcar '(lambda ( x ) (vla-get-name x)) styles)
          style (car(LM:ListBox "Select Style..." nlst nil))
    )

    (if style (progn
    (vlax-for blk (vla-get-Blocks acdoc)
        (vlax-for obj blk
            (and (wcmatch (vla-get-ObjectName obj) "AcDb*Text" )
                 (vla-put-stylename obj style)
            )
        )
    )
    (vla-Regen acdoc acAllViewports)
    
    (setvar 'textstyle style)
    (setvar 'dimtxsty style)
    ))
    (princ)
    
)(vl-load-com)