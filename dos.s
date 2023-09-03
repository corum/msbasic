dos:
    jsr _cls
    jsr fat32_start

newprompt:
    ldx #0
    ldy #0
    stx dos_command
    jsr display_message
    .byte 10, 13, "/>", 0

read_command:
    jsr read_char
    cmp #$0D
    beq process_command
    jsr display_char
    sta dos_command,x
    inx
    bra read_command

process_command:
    lda #$00
    sta dos_command, x

    ; match with an existing command
    jsr match_command
    .byte "ECHO",0
    cpx #$4
    beq cmd_echo

    jsr match_command
    .byte "EXIT",0
    cpx #$4
    beq cmd_exit

    jsr match_command
    .byte "DIR",0
    cpx #$3
    beq cmd_dir

    jsr match_command
    .byte "CD",0
    cpx #$2
    beq cmd_chdir

    jsr display_message
    .byte 10, 13, "Unknown Command", 10, 13, 0
    bra newprompt

cmd_echo:
    jsr print_crlf

    inx
    lda dos_command, X
    beq newprompt
    jsr display_char
    bra cmd_echo

cmd_exit:
    jsr display_message
    .byte 10, 13, "Goodbye!", 10, 13, 0
    jmp WOZMON

cmd_dir:
    jsr fat32_dir
    jmp newprompt

cmd_chdir:
    inx
    ldy #>dos_command

    jsr fat32_finddirent
    bcc @found

    jsr display_message
    .byte 10, 13, "Directory not found", 10, 13, 0
    jmp newprompt

@found:
    jsr fat32_opendirent

    jsr display_message
    .byte 10, 13, "Directory found", 10, 13, 0

    jmp newprompt

; x contains the # of matching chars
match_command:
    ldx #0
    ldy	#0
    pla
    sta	MSG_ADDR
    pla
    sta	MSG_ADDR+1          ; get return address off the stack
    bne	@increturn

@nextchar:
    lda	(MSG_ADDR),Y		; next message character    
    beq	@pushreturnaddr		; done?	yes, exit
    jsr	match_char
    bne @increturn          ; doesn't match, exit
    inx

@increturn:					; next address
    inc	MSG_ADDR
    bne	@nextchar
    inc	MSG_ADDR+1	   	    ; fix MSB of next address
    bne	@nextchar

@pushreturnaddr:
    lda	MSG_ADDR+1
    pha
    lda	MSG_ADDR
    pha				; adjust return	address
    rts


    ; A contains the char to match
    ; X contains the index into the string
match_char:
    cmp dos_command, x
    bne @nomatch
    lda #0
@nomatch:
    rts

