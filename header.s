.segment "HEADER"
.ifdef KBD
        jmp     LE68C
        .byte   $00,$13,$56
.endif
.ifdef AIM65
        jmp     COLD_START
        jmp     RESTART
        .word   AYINT,GIVAYF
.endif
.ifdef SYM1
        jmp     PR_WRITTEN_BY
.endif
.ifdef BADGER6502
        jmp     COLD_START                ;$C800
        jmp     RESTART                   ;$C803
        jmp     _cls                      ;$C806
        jmp     _loderunner               ;$C809
        jmp     read_char_async_apple     ;$C80C
        jmp     romdisk_load              ;$C80F
        jmp     dos                       ;$C812
        jmp     hires1                    ;$C815
        jmp     hires2                    ;$C818
        jmp     read_char_upper           ;$C81B
        jmp     read_char_async           ;$C81E
        jmp     textmode                  ;$C821
.endif
