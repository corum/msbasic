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
    jsr print_crlf
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

@match_cat:
    jsr match_command
    .byte "CAT",0
    bcs @match_cd
    jmp cmd_cat

@match_cd:
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
    bcs @match_jump
    jmp cmd_exit

@match_jump:
    jsr match_command
    .byte "JUMP",0
    bcs @match_load
    jmp cmd_jump

@match_load:
    jsr match_command
    .byte "LOAD",0
    bcs @match_mount
    jmp cmd_load

@match_mount:
    jsr match_command
    .byte "MOUNT",0
    bcs @match_quit
    jmp cmd_mount

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
    ldx dos_param_0
    jsr dos_setfileparam
    jmp newprompt

cmd_cat:
    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_finddirent
    bcs @file_not_found_long

    jsr fat32_opendirent

    jsr fat32_cat_file
    jsr fat32_open_cd

    ;jsr fat32_load_file
    jmp newprompt

@file_not_found_long:
    jmp file_not_found

cmd_chdir:
    ;jsr fat32_open_cd

    ldx dos_param_0
    jsr dos_setfileparam

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
    
    jsr fat32_dump_diskstats
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

cmd_jump:
    ldx dos_param_0
    jsr dos_parse_hex_address
    bcs @invalid_address
    jmp (dos_addr_temp)

@invalid_address:
    jsr display_message
    .byte 10, 13, "Invalid Address", 10, 13, 0     
    jmp newprompt

cmd_load:
    ldx dos_param_0
    jsr dos_setfileparam

    ldx dos_param_1
    jsr dos_parse_hex_address
    bcs @invalid_address
    
    jsr fat32_finddirent
    bcs file_not_found

    jsr fat32_opendirent

    lda dos_addr_temp
    sta fat32_address
    lda dos_addr_temp+1
    sta fat32_address+1

    jsr fat32_file_read

    jmp newprompt

@invalid_address:
    jsr display_message
    .byte 10, 13, "Invalid Address", 10, 13, 0     
    jmp newprompt

cmd_mount:
    jsr fat32_start
    jmp newprompt

file_not_found:
    jsr fat32_open_cd
    jsr display_message
    .byte 10, 13, "File Not Found", 10, 13, 0
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

dos_setfileparam:
    phx
    phy
    inx
    stx fat32_filenamepointer
    ldy #>dos_command ; set x and y to point to the command line parameter address
    sty fat32_filenamepointer+1    
    jsr fat32_prep_fileparam
    ply
    plx
    rts


; A contains an ascii code, convert to a nyble
; carry set means invalid input
dos_convert_hex_nyble:
    cmp #'0'
    bcs @gt0
    bcc @garbage  ; out of range

@gt0:
    cmp #':'
    bcc @is_digit  ; in ascii range of '0'-'9'
    cmp #'A'
    bcs @gtA   ; >= 'A'
    bcc @garbage  ; out of range, > '9' and < 'A'

@gtA:
    cmp #'G'
    bcs @garbage

@is_alpha:    
    sec
    sbc #$37
    bra @done

@is_digit:
    sec
    sbc #$30
    bra @done

@garbage:
    sec
    lda #$00
    rts

@done:
    clc
    rts

; takes pointer to dos command line parameter, reads 4 bytes and converts to hex number
; stores in dos_addr_temp and dos_addr_temp+1
; destructive to A and x

dos_parse_hex_address:
    ; x points to the first byte of the address parameter
    
    ; high nyble of high byte
    inx
    lda dos_command,x
    jsr dos_convert_hex_nyble    
    bcs @error
    clc
    asl
    asl
    asl
    asl
    sta dos_addr_temp + 1

    ; low nyble of high byte
    inx
    lda dos_command,x
    jsr dos_convert_hex_nyble
    bcs @error
    ora dos_addr_temp + 1
    sta dos_addr_temp + 1

    ; high nyble of low byte
    inx
    lda dos_command,x
    jsr dos_convert_hex_nyble
    bcs @error
    clc
    asl
    asl
    asl
    asl
    sta dos_addr_temp

    ; low nyble of low byte
    inx
    lda dos_command,x
    jsr dos_convert_hex_nyble
    bcs @error
    ora dos_addr_temp
    sta dos_addr_temp
    rts

@error:
    lda #$00
    sta dos_addr_temp
    sta dos_addr_temp+1
    rts


; ********************************************************
; sample code for writing a file
; ********************************************************
;nf:
;  ;jsr refreshpath
;  ldx #<savemsg
;  ldy #>savemsg
;  jsr w_acia_full
;  ; Calculate file size (end - start)
;  sec
;  lda stackaccess
;  sbc savestart
;  sta fat32_bytesremaining 
;  pha
;  lda stackaccess+1
;  sbc savestart+1
;  sta fat32_bytesremaining+1
;  pha
;  ; Allocate all the clusters for this file
;  jsr fat32_allocatefile
;  ; Refresh
;  jsr refreshpath
;  ; Put the filename at fat32_filenamepointer
;  lda savepoint
;  sta fat32_filenamepointer
;  lda savepoint+1
;  sta fat32_filenamepointer+1 
;  pla
;  sta fat32_bytesremaining+1
;  pla
;  sta fat32_bytesremaining
;  ; Write a directory entry for this file
;  jsr fat32_writedirent
;  ; Now, to actually write the file...
;  lda savestart
;  sta fat32_address
;  lda savestart+1
;  sta fat32_address+1
;  jsr fat32_file_write
;  ; All Done!
;  ldx #<ends
;  ldy #>ends
;  jsr w_acia_full
;saveexit:
;  plx
;  rts