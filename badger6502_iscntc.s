.segment "CODE"
ISCNTC:
        jsr	read_char_async
        cmp	#$03
        beq	@ctrlc          ; if CTRL-C not pressed then exit
	clc	                ; Carry clear if control C not pressed
	rts

@ctrlc:
       sec                     ; Carry set if control C pressed
       lda     #$03
       cmp     #$03            ; Basic wants the zero flag set in this case

;!!! runs into "STOP"