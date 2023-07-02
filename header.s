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
        jmp     COLD_START                ;$C100
        jmp     RESTART                   ;$C103
        jmp     _cls                      ;$C106
        jmp     _loderunner               ;$C109
        jmp     read_char_async_apple     ;$C10C
        jmp     romdisk_load              ;$C10F
        jmp     dos                       ;$C112
        jmp     dos                       ;$C115
.endif
