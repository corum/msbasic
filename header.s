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
        jmp     COLD_START                ;$E000
        jmp     RESTART                   ;$E003
        jmp     _cls                      ;$E006
        jmp     _loderunner               ;$E009
        jmp     read_char_async_apple     ;$E00C
        jmp     romdisk_load              ;$E00F
        jmp     dos                       ;$E012
        jmp     hires1                    ;$E015
        jmp     hires2                    ;$E018
        jmp     read_char_upper           ;$E01B
        jmp     read_char_async           ;$E01E
        jmp     textmode                  ;$E021
.endif
