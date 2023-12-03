; DOS
;dos_command    = $CE00  ; command line
;dos_params     = dos_command + $7F
;dos_param_3    = dos_command + $7E  ; the command
;dos_param_2    = dos_command + $7D
;dos_param_1    = dos_command + $7C
;dos_param_0    = dos_command + $7B
;dos_file_param = dos_command + $70  ; 11 bytes
;dos_addr_temp  = dos_command + $6E  ; 2 bytes
;dos_cout_mode  = dos_command + $6F  ; 1 byte
;dos_cursor     = dos_command + $70  ; 1 byte

DOS_hook_cout = $03ea
;CSWL          = $36

hook_buffer:
    pha
    lda #<display_apple_char
    sta CSWL
    lda #>display_apple_char
    sta CSWL+1
    pla
    rts

setup_cout_hook:
    ; put a routine at DOS_hook_cout that jumps to the hook buffer
    pha
    lda #$20  ; JSR
    sta DOS_hook_cout
    lda #<hook_buffer
    sta DOS_hook_cout + 1
    lda #>hook_buffer
    sta DOS_hook_cout + 2
    lda #$60  ; RTS
    sta DOS_hook_cout + 3
    pla
    rts

dos:
    jsr setup_cout_hook
    jsr _cls
    jsr fat32_start

newprompt:
    jsr display_message
    .byte 10, 13, ">", 0
    ldx #0
    ldy #0
    stx dos_command
    stx dos_cout_mode
    stx dos_cursor

read_command:
    jsr read_char_upper
    and #$7F
    cmp #$0D
    beq @do_parse_command
    cmp #$08
    beq @backspace
    jsr display_char
    sta dos_command,x
    inx
    bra read_command
@backspace:
    jsr display_char
    lda #$00
    sta dos_command,x
    dex
    bmi newprompt
    bra read_command
    
; parse_command: 
; walks through the command string, terminated by a null, 5th param
; or $80th character
@do_parse_command:
    jsr print_crlf
    jsr parse_command
    jmp newprompt

parse_command:
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
    beq @delim
    cmp #$2C ; is it a comma?
    beq @delim
    bra @count_params
@delim:
    lda #$0            ; if it's a comma, write out a null
    sta dos_command,y

    ldx dos_params
    inc dos_params
    tya
    sta dos_param_0,x   ; x is the parameter number, a is the position in the string
    cpx #$04
    bne @count_params

; match with an existing command
@process_command:

@match_del:
    jsr match_command
    .byte "DEL", 0
    bcs @match_fload
    jmp cmd_del

@match_fload:
    jsr match_command
    .byte "FLOAD",0
    bcs @match_cat
    jmp cmd_fload
    
@match_cat:
    jsr match_command
    .byte "CAT",0
    bcs @match_dc
    jmp cmd_cat

@match_dc:
    jsr match_command
    .byte "DC",0
    bcs @match_hexdump
    jmp cmd_dc

@match_hexdump:
    jsr match_command
    .byte "HD",0
    bcs @match_cd
    jmp cmd_hexdump
    
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
    bcs @match_jump
    jmp cmd_dir

@match_jump:
    jsr match_command
    .byte "J",0
    bcs @match_bload
    jmp cmd_jump

@match_bload:
    jsr match_command
    .byte "BLOAD",0
    bcs @match_brun
    jmp cmd_bload

@match_brun:
    jsr match_command
    .byte "BRUN",0
    bcs @match_bsave
    jmp cmd_brun

@match_bsave:
    jsr match_command
    .byte "BSAVE",0
    bcs @match_quit
    jmp cmd_bsave

@match_quit:
    jsr match_command
    .byte "Q",0
    bcs @unknown
    jmp cmd_exit

@unknown:
    jsr display_message
    .byte "What?", 10, 13, 0
    rts

cmd_del:
    jsr fat32_set_readbuffer
    jsr fat32_set_target

    jsr fat32_open_cd

    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_finddirent
    bcs file_not_found_long

    jsr fat32_deletefile
    jsr fat32_open_cd
    rts

cmd_hexdump:
    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_finddirent
    bcs file_not_found_long

    jsr fat32_opendirent

    jsr fat32_hexdump_file
    jsr fat32_open_cd
    rts
 
cmd_dc:
    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_finddirent
    bcs file_not_found_long

    jsr fat32_opendirent

    jsr fat32_dump_cluster_chain
    jsr fat32_open_cd

    rts

cmd_cat:
    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_finddirent
    bcs file_not_found_long

    jsr fat32_opendirent

    jsr fat32_cat_file
    jsr fat32_open_cd
    rts

file_not_found_long:
    jmp file_not_found

cmd_chdir:
    jsr fat32_open_cd

    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_finddirent
    bcc @found

    jmp display_error

@found:
    jsr fat32_opendirent

@ok:
    jmp display_ok

display_error:
    jsr display_message
    .byte "Err", 10, 13, 0
    rts

display_ok:
    jsr display_message
    .byte "OK", 10, 13, 0
    rts
; ************************************************************
; COMMANDS

cmd_cls:
    jmp _cls

cmd_dir:
    jmp fat32_dir

cmd_exit:
    jmp WOZMON

cmd_jump:
    ldx dos_param_0
    jsr dos_parse_hex_address
    bcs invalid_address
    stz KEYRAM
    jmp (dos_addr_temp)

load_proc_3:
    ldx dos_param_2
    jsr dos_parse_hex_address
    bcs invalid_address

    lda dos_addr_temp
    sta fat32_byte_offset
    lda dos_addr_temp+1
    sta fat32_byte_offset+1

    ldx dos_param_3
    jsr dos_parse_hex_address
    bcs invalid_address

    lda dos_addr_temp
    sta fat32_file_bytes
    lda dos_addr_temp+1
    sta fat32_file_bytes+1

load_proc:
    ldx dos_param_1
    jsr dos_parse_hex_address
    bcs invalid_address

    ldx dos_param_0
    jsr dos_setfileparam

    jsr fat32_open_cd
    jsr fat32_finddirent
    bcs file_not_found
    
    jsr fat32_opendirent

    lda dos_addr_temp
    sta zp_fat32_destination
    lda dos_addr_temp+1
    sta zp_fat32_destination+1

    stz KEYRAM
    rts

invalid_address:
    jsr display_message
    .byte "Bad Address", 10, 13, 0     
    rts

file_not_found:
    jsr fat32_open_cd
    jsr display_message
    .byte "Not Found", 10, 13, 0
    rts

cmd_fload:
    jsr load_proc_3
    jsr fat32_file_read_part
    rts

cmd_bload:
    jsr load_proc
    jsr fat32_file_read
    rts

cmd_brun:
    jsr load_proc
    jsr fat32_file_read
    jmp (dos_addr_temp)

cmd_bsave:
    ldx dos_param_1              ; start address
    jsr dos_parse_hex_address
    bcs invalid_address

    lda dos_addr_temp
    pha
    lda dos_addr_temp+1
    pha
    
    ldx dos_param_2              ; length
    jsr dos_parse_hex_address
    bcs invalid_address

    ldx dos_param_0
    jsr dos_setfileparam

    jsr restore_bytesremaining

    jsr fat32_allocatefile

    lda #<dos_file_param
    sta fat32_filenamepointer
    lda #>dos_file_param
    sta fat32_filenamepointer+1
    
    jsr restore_bytesremaining

    jsr fat32_open_cd

    jsr fat32_writedirent
    bcs @error

    jsr restore_bytesremaining

    pla 
    sta fat32_address+1
    pla
    sta fat32_address
    
    jsr fat32_file_write
    bcs @error

    jsr fat32_open_cd
    rts
    ;bra @success

@error:
    jmp display_error

@success:
    jmp display_ok

restore_bytesremaining:
    lda dos_addr_temp
    sta fat32_bytesremaining
    lda dos_addr_temp+1
    sta fat32_bytesremaining + 1
    stz fat32_bytesremaining+2
    stz fat32_bytesremaining+3
    rts

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
    ; require A$ to proceed the address
@parseheader:
    ; high nyble of high byte
    inx
    lda dos_command,x
@normal:
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