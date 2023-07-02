; DOS variables
; dos_command    = $A00   ; command line
; dos_params     = $AEF
; dos_param_4    = $AEE   ; the command
; dos_param_3    = $AED
; dos_param_2    = $AEC
; dos_param_1    = $AEB
; dos_param_0    = $AEA

dos:
    jsr _cls
    jsr fat32_start

newprompt:
    jsr display_message
    .byte 10, 13, "/>", 0
    ldx #0
    ldy #0
    stx dos_command

read_command:
    jsr read_char_upper
    and #$7F
    cmp #$0D
    beq @parse_command
    jsr display_char
    sta dos_command,x
    inx
    bra read_command

; @parse_command: 
; walks through the command string, terminated by a null, 5th param
; or $80th character
@parse_command:
    lda #$00
    sta dos_params
    sta dos_command,x
    ldy #$FF
@count_params:
    iny
    cpy #$80
    beq @process_command
    lda dos_command,y
    beq @process_command
    cmp #$20 ; is it a space
    bne @count_params
    ldx dos_params
    inc dos_params
    tya
    sta dos_param_0,x   ; x is the parameter number, a is the position in the string
    cpx #$04
    bne @count_params

; match with an existing command
@process_command:
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
    bcs @match_diskstat
    jmp cmd_dir

@match_diskstat:
    jsr match_command
    .byte "DISKSTAT",0
    bcs @match_echo
    jmp cmd_diskstat

@match_echo:
    jsr match_command
    .byte "ECHO",0
    bcs @match_exit
    jmp cmd_echo

@match_exit:
    jsr match_command
    .byte "EXIT",0
    bcs @match_load
    jmp cmd_exit

@match_load:
    jsr match_command
    .byte "LOAD",0
    bcs @match_quit
    jmp cmd_load

@match_quit:
    jsr match_command
    .byte "QUIT",0
    bcs @match_test
    jmp cmd_exit

@match_test:
    jsr match_command
    .byte "TEST",0
    bcs @match_woz
    jmp cmd_test

@match_woz:
    jsr match_command
    .byte "WOZ",0
    bcs @unknown
    jmp WOZMON

@unknown:
    jsr display_message
    .byte 10, 13, "Unknown Command", 10, 13, 0
    jmp newprompt

cmd_test:
    ;lda #$20
    ;sta zp_sd_address
    ;lda #$A0
    ;sta zp_sd_address+1
    jsr fat32_open_cd
    
    ldx dos_param_0
    inx
    stx zp_sd_temp      ; zp_sd_temp points to command line parameters
    ldy #>dos_command
    sty zp_sd_temp+1    ; set x and y to point to the command line parameter address

    jsr fat32_finddirent
    bcc @found1
    jsr display_message
    .byte 10, 13, "string doesn't match", 10, 13, 0
    jmp newprompt    
@found1:
    jsr display_message
    .byte 10, 13, "string matches", 10, 13, 0

    jmp newprompt

cmd_chdir:
    ;jsr fat32_open_cd

    ldx dos_param_0
    inx
    stx fat32_filenamepointer
    ldy #>dos_command ; set x and y to point to the command line parameter address
    sty fat32_filenamepointer+1
    
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

; ************************************************************
; COMMANDS

cmd_cls:
    jsr _cls
    jmp newprompt

cmd_dir:
    jsr fat32_dir
    jmp newprompt

cmd_diskstat:
    lda #$00
    sta zp_sd_temp + 1
    
    jsr display_message
    .byte 10,13,"fat32_fatstart            $", 0
    lda #fat32_fatstart
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"fat32_datastart           $", 0
    lda #fat32_datastart
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"fat32_rootcluster         $", 0
    lda #fat32_rootcluster
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"fat32_sectorspercluster   $", 0
    lda fat32_sectorspercluster
    jsr print_hex

    jsr display_message
    .byte 10,13,"fat32_pendingsectors      $", 0
    lda fat32_pendingsectors
    jsr print_hex

    jsr display_message
    .byte 10,13,"fat32_address             $", 0
    lda #fat32_address
    sta zp_sd_temp
    jsr print_hex_word

    jsr display_message
    .byte 10,13,"fat32_nextcluster         $", 0
    lda #fat32_nextcluster
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"fat32_bytesremaining      $", 0
    lda #fat32_bytesremaining
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"zp_sd_currentsector       $", 0
    lda #zp_sd_currentsector
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"zp_sd_cd_cluster          $", 0
    lda #zp_sd_cd_cluster
    sta zp_sd_temp
    jsr print_hex_dword

    jsr display_message
    .byte 10,13,"zp_sd_address             $", 0
    lda #zp_sd_cd_cluster
    sta zp_sd_temp
    jsr print_hex_word
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

cmd_load:
    inx
    stx zp_sd_temp
    lda #>dos_command
    sta zp_sd_temp+1
    clc
    lda #<dos_command
    adc zp_sd_temp
    sta zp_sd_temp       ; zp_sd_temp points to command line parameters

    ldx zp_sd_temp      ; set x and y to point to the command line parameter address
    ldy zp_sd_temp + 1

    jmp newprompt

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

