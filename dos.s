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
    .byte "CD",0
    bcs @match_cls
    jmp cmd_chdir

@match_cls:
    jsr match_command
    .byte "CLS",0
    bcs @match_dir
    jmp cmd_cls

@match_dir:
    jsr match_command
    .byte "DIR",0
    bcs @match_echo
    jmp cmd_dir

@match_echo:
    jsr match_command
    .byte "ECHO",0
    bcs @match_exit
    jmp cmd_echo

@match_exit:
    jsr match_command
    .byte "EXIT",0
    bcs @match_quit
    jmp cmd_exit

@match_quit:
    jsr match_command
    .byte "QUIT",0
    bcs @match_woz
    jmp cmd_exit

@match_woz:
    jsr match_command
    .byte "WOZ",0
    bcs @unknown
    jmp WOZMON

@unknown:
    jsr display_message
    .byte 10, 13, "Unknown Command", 10, 13, 0
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
    jsr display_message
    .byte 10, 13, "Directory found", 10, 13, 0

    jmp newprompt

; ************************************************************
; COMMANDS

cmd_cls:
    jsr _cls
    jmp newprompt

cmd_dir:
    jsr fat32_dir
    jmp newprompt

cmd_echo:
    jsr print_crlf

@echoloop:
    inx
    lda dos_command, X
    beq @newprompt
    jsr display_char
    bra @echoloop
@newprompt:
    jmp newprompt

cmd_exit:
    jsr display_message
    .byte 10, 13, "Goodbye!", 10, 13, 0
    jmp WOZMON

;****************************************************************************
; STRING COMPARISON
; x contains the # of matching chars
; if carry is cleared, it matches, if carry is set, it doesn't match
match_command:
    clc 
    ldx #0
    ldy	#0
    pla
    sta	MSG_ADDR_LOW
    pla
    sta	MSG_ADDR_HIGH       ; get return address off the stack
    bne	@increturn

@nextchar:
    lda	(MSG_ADDR_LOW),Y	; next message character    
    beq	@pushreturnaddr		; null terminator?	yes, exit
    bcs @increturn          ; previous char didn't match - skip checking
    jsr	match_char
    inx
    
@increturn:					; next byte
    inc	MSG_ADDR_LOW
    bne	@nextchar
    inc	MSG_ADDR_HIGH	   	; carry
    bne	@nextchar

@pushreturnaddr:
    lda	MSG_ADDR_HIGH
    pha
    lda	MSG_ADDR_LOW
    pha				; adjust return	address
    rts


; A contains the char to match
; X contains the index into the string
; carry bit set if it doesn't match
match_char:
    cmp dos_command, x
    clc
    beq @matches
    sec                      ; carry flag means doesn't match
@matches:
    rts

