.segment "A2MON"
.include "apple2rom.s"

.segment "BANKROM"

;Keyboard
KEYRAM  = $C000

;ACIA C0
A_RXD   = $C100
A_TXD   = $C100
A_STS   = $C101
A_RES   = $C101
A_CMD   = $C102
A_CTL   = $C103

; devices
;VIA D1
PORTB   = $C200
PORTA   = $C20F     ; PORTA is register 1, this is PORTA with no handshake
DDRB    = $C202
DDRA    = $C203
T1CL    = $C204
T1CH    = $C205
T1LL    = $C206
T1LH    = $C207
T2L     = $C208
T2H     = $C209
SHCTL   = $C20A
ACR     = $C20B     ; auxiliary control register
PCR     = $C20C     ; peripheral control register
IFR     = $C20D 
IER     = $C20E     ; interrupt enable register

;ROMDISK
RD_LOW         = $C300
RD_HIGH        = $C301
RD_BANK        = $C302
RD_DATA        = $C303

;ROMDISK VARIABLES

SOURCE_LOW     = $B0
SOURCE_HIGH    = $B1

RD_BYTES_LOW   = $B2
RD_BYTES_HIGH  = $B3

DEST_LOW       = $B4
DEST_HIGH      = $B5

MSG_ADDR_LOW   = $B6
MSG_ADDR_HIGH  = $B7

;VIA config flags 
ICLR   = %01111111  ; clear all VIA interrupts
CFGCA  = %00100010  ; configure CA2 for negative active edge for PS/2 clock

;SD card pins
SD_CS   = %00010000
SD_SCK  = %00001000
SD_MOSI = %00000100
SD_MISO = %00000010

PORTA_OUTPUTPINS = %11100000 | SD_CS | SD_SCK | SD_MOSI

;CONSOLE
CURSOR_X       = $CE00
CURSOR_Y       = $CE01

;GRAPHICS
YADD           = $CE02
XADD           = $CE03
X1_            = $CE04
Y1_            = $CE05
X2_            = $CE06

; TEXT MODE

TEXT           = $400
CURSOR_ADDR    = $50
CURSOR_ADDR_H  = $51
CHAR_DRAW      = $52

ORIGIN_L       = $53
ORIGIN_H       = $54
SCREEN_L       = $55
SCREEN_H       = $56

HIRESPAGE      = $57
HIRESPAGE_H    = $58

DRAW_WIDTH     = $CE07
DRAW_WIDTH_H   = $CE08
DRAW_HEIGHT    = $CE09
DRAW_COLOR     = $CE0A
X1             = $CE0B
X1_H           = $CE0C
X2             = $CE0D
X2_H           = $CE0E
Y1             = $CE0F
Y2             = $CE10
XD             = $CE11   ; xdelta for line drawing
YD             = $CE12   ; ydelta for line drawing
XT             = $CE13   ; x temp
YT             = $CE14   ; y temp
FONTPTR        = $CE15
FONTPTR_H      = $CE16

; PS/2 keyboard memory locations
KBSTATE        = $CE17
KBTEMP         = $CE18
KBCURR         = $CE19 
KBBIT          = $CE1A
KBEXTEND       = $CE1B
KBKEYUP        = $CE1C
KBDBG          = $CE1D
KBDBG2         = $CE1E
KEYTEMP        = $CE1F
KEYLAST        = $CE20
MUTE_OUTPUT    = $CE21

; keyboard processing states
PS2_START      = $00
PS2_KEYS       = $01
PS2_PARITY     = $02
PS2_STOP       = $03

; starting of 512 byte buffer used by fat32
fat32_workspace= $C800  ; $C800 - $C9FF

KBBUF           = $0200 ; $CA00
KEYSTATE        = $CB00
fat32_variables = $CC00

; DOS
dos_command    = $CD00  ; command line
dos_params     = dos_command + $7F
dos_param_3    = dos_command + $7E  ; the command
dos_param_2    = dos_command + $7D
dos_param_1    = dos_command + $7C
dos_param_0    = dos_command + $7B
dos_file_param = dos_command + $70  ; 11 bytes
dos_addr_temp  = dos_command + $6E  ; 2 bytes
dos_cout_mode  = dos_command + $6D  ; 1 byte
dos_cursor     = dos_command + $6C  ; 1 byte
dos_addr_p2    = dos_command + $6D  ; 2 bytes
dos_addr_p3    = dos_command + $6F  ; 2 bytes

; SOFT SWITCHES

SS_BASROM_OFF  = $C007
SS_GRAPHICS    = $C050 ; Display Graphics
SS_TEXT        = $C051 ; Display Text
SS_FULLSCREEN  = $C052 ; Display Full Screen
SS_SPLITSCREEN = $C053 ; Display Split Screen
SS_DISPLAY_1   = $C054 ; Display Page 1
SS_DISPLAY_2   = $C055 ; Display Page 2
SS_LORES       = $C056 ; Display LoRes Graphics
SS_HIRES       = $C057 ; Display HiRes Graphics

SS_R_BANK2     = $C080 ; Read RAM bank 2; no write also $C084 
SS_W_BANK2     = $C081 ; Read ROM, write RAM bank 2 also $C085

SS_R_ROM2      = $C082 ; Read ROM; no write also $C086
SS_RW_BANK2    = $C083 ; Read/write RAM bank 2 also $C087

SS_R_BANK1     = $C088 ; Read RAM bank 1; no write also $C08C
SS_W_BANK1     = $C089 ; Read ROM; write RAM bank 1 also $C08D
SS_R_ROM1      = $C08A ; Read ROM; no write also $C08E
SS_RW_BANK1    = $C08B ; Read/write RAM bank 1 also $C08F

;CSWL           = $36

via_init:
    lda #%11111111          ; Set all pins on port B to output
    sta DDRB

    lda #PORTA_OUTPUTPINS   ; Set various pins on port A to output
    sta DDRA

    lda #CFGCA
    sta PCR        ; configure CA2 for negative edge independent interrupt, for PS/2, CB2 for negative interrupt for keyboard strobe

    lda #$0
    sta ACR

    lda #%11111001 
    sta IER        ; enable interrupts for CA2, CB1, CB2 and Timer1, Timer2

    rts

;CODE
init:

; init PS/2 kb stuff
    lda #$00
    sta KBSTATE
    sta KBTEMP
    sta KBCURR
    sta KBBIT
    sta KBEXTEND
    sta KBKEYUP
    sta KBDBG
    sta KBDBG2
    sta KEYTEMP
    sta KEYLAST

    sta MUTE_OUTPUT

    ldx #$00           ; clear the key state and input buffers
@clrbufx:
    sta KEYSTATE, x
    sta KBBUF, x
    sta dos_command, x
    sta fat32_workspace, x 
    sta fat32_workspace+1, x
    sta fat32_variables, x
    inx
    bne @clrbufx

    sei
    cld
    
    ldx #STACK_TOP
    txs

    ; put machine into text mode with default font 
    bit SS_TEXT
    bit SS_DISPLAY_1
    bit SS_FULLSCREEN
    bit SS_HIRES
    sta SS_BASROM_OFF
    bit SS_R_ROM2

 ;   stz INPUTBUF
    stz INPUTBUFFER
    stz CURSOR_X
    stz CURSOR_Y

; initialize the ACIA
    stz A_RES      ; soft reset (value not important)

                   ; set specific modes and functions
                   ; no parity, no echo, no Tx interrupt, Rx interrupt, enable Tx/Rx
    lda #%00001001
    sta A_CMD      ; store to the command register

    ;lda #$00      ; 1 stop bits, 8 bit word length, external clock, 16x baud rate
    lda #$1F       ; 1 stop bits, 8 bit word length, internal clock, 19.2k baud rate
    sta A_CTL      ; program the ctl register


    ;jsr A2INIT

    jsr via_init
    
    ; set graphics page
    lda #$20
    sta SCREEN_H
    lda #$00
    sta SCREEN_L


    cli

    lda #$9B
@loop:
    ;jsr WOZMON

    jsr     SETNORM         ;  set screen mode
    jsr     A2INIT          ;  and init kbd/screen
    jsr     SETVID          ;  as I/O dev's
    jsr     SETKBD
    jsr     hook_buffer
    jsr     cls

    jsr     MON

    jmp @loop

hires1:
    bit SS_GRAPHICS
    bit SS_DISPLAY_1
    bit SS_HIRES
    rts

hires2:
    bit SS_GRAPHICS
    bit SS_DISPLAY_2
    bit SS_HIRES
    rts

textmode:
    bit SS_TEXT
    bit SS_DISPLAY_1
    bit SS_FULLSCREEN
    rts
    
; loderunner
_loderunner:
    lda #2
    sta DEST_HIGH
    sta SOURCE_HIGH

    stz DEST_LOW
    stz SOURCE_LOW
    stz RD_BYTES_LOW

    lda #$BE
    sta RD_BYTES_HIGH

    stz RD_LOW
    stz RD_HIGH
    stz RD_BANK

    jsr romdisk_load
    jmp $6000


.segment "OS"
.include "libfat32.s"

.segment "OS"
.include "dos.s"


MSG_FILENOTFOUND:
    .byte "NOT FOUND"
    .byte CR,LF,0

MSG_FILE_ERROR:
    .byte "ERROR"
    .byte CR,LF,0


joytest:
    
    jsr display_message
    .byte "X=",0

    ldx #$0
    jsr PREAD
    tya
    jsr print_hex

    jsr display_message
    .byte ",Y=",0

    ldx #$1
    jsr PREAD
    tya
    jsr print_hex
    jsr print_crlf

    jmp joytest

.segment "CODE"

; load and save for eb6502
eb_load:
    ; go get the filename from $200
    ; start of basic program is at $28,$29 little endian
    ; end of basic program is at $2E,$2f

    jsr fat32_start
    jsr parse_basic_filename
    
    jsr fat32_finddirent
    bcs @file_not_found

    jsr fat32_opendirent
    jsr fat32_basic_load
    bra @success

@file_not_found:
    lda     #<MSG_FILENOTFOUND
    ldy     #>MSG_FILENOTFOUND
    jsr     STROUT
    bra     @exit

@success:
    jsr     display_ok

@exit:
    jmp     FIX_LINKS

eb_save:
    pha

    ; go get filename from $200
    jsr fat32_start

    sec
    lda $2E ; end of program low byte
    sbc $28 ; start of program low byte
    sta fat32_bytesremaining
    pha
    lda $2f ; end of program high byte
    sbc $29 ; end of program low byte
    sta fat32_bytesremaining+1
    pha
    lda #$00
    sta fat32_bytesremaining+2
    sta fat32_bytesremaining+3

    jsr parse_basic_filename

    jsr fat32_allocatefile

    pla 
    sta fat32_bytesremaining + 1
    pla
    sta fat32_bytesremaining

    jsr fat32_open_cd

    ;jsr fat32_dump_diskstats
    
    jsr fat32_writedirent
    bcs @error

    lda $28
    sta fat32_address
    ;jsr print_hex

    lda $29
    sta fat32_address+1
    ;jsr print_hex

    jsr fat32_file_write
    bcc @success

@error:
    jsr display_error
    bra     @exit

@success:
    jsr display_ok

@exit:
    pla
    jmp     FIX_LINKS


; parse basic filename
parse_basic_filename:
    pha
    phx
    phy 

    lda #$00
    sta SOURCE_LOW
    lda #$02
    sta SOURCE_HIGH
    
    ldy #$00
    ldx #$00
    stx dos_param_0

@find_open_quote:
    lda (SOURCE_LOW),y
    iny
    cmp #'"'
    bne @find_open_quote

@copy_file_name:
    lda (SOURCE_LOW),y
    iny
    cmp #'"'
    beq @end_of_string
    sta dos_command,x
    inx
    cpx #$0b
    bne @copy_file_name

@end_of_string:
    inx
    lda #$00
    sta dos_command, x

    ldy #<dos_command
    sty fat32_filenamepointer
    ldy #>dos_command
    sty fat32_filenamepointer+1    

    ldx #$00
    jsr fat32_prep_fileparam

    lda #<dos_file_param
    sta fat32_filenamepointer
    lda #>dos_file_param
    sta fat32_filenamepointer+1

    ply
    plx
    pla
    rts
   
; load and save routines for the pico implementation
eb_load_pico:
    pha
    sta     $C0F1    ; load
    lda     $C0FF    ; get status value
    beq     @success

    cmp     #2
    bne     @foundfile

    lda     #<MSG_FILENOTFOUND
    ldy     #>MSG_FILENOTFOUND
    jsr     STROUT

@foundfile:
    lda     #<MSG_FILE_ERROR
    ldy     #>MSG_FILE_ERROR
    jsr     STROUT
    bra     @exit

@success:
    jsr     display_ok

@exit:
    pla
    jmp     FIX_LINKS

eb_save_pico:
    pha
    sta     $C0F0   ; save 
    lda     $C0FF   ; get status value
    
    beq     @success

    jsr     display_error
    bra     @exit

@success:
    jsr     display_ok

@exit:
    pla
    jmp     FIX_LINKS

.segment "OS"


wdc_pause:
    phx
    ldx #$B0

@wdc_pause_loop1:
    inx
    cpx #$00
    bne @wdc_pause_loop1

    ldx #$B0
@wdc_pause_loop2:
    inx
    cpx #$00
    bne @wdc_pause_loop2

    plx
    rts

tx_char_sync:
    pha
@wait:
    lda A_STS              ; get status byte
    and #$10               ; mask transmit buffer status flag
    beq @wait              ; loop if tx buffer full
    pla

    pha
    and #$7F
    sta A_TXD
    pla
    
    ; workaround for WDC chip
    ; skip pause for now since rendering text is so expensive
    jsr wdc_pause

    rts



;==========================================================================
; Keyboard
;==========================================================================

read_char_async_apple:
    lda KBCURR
    cmp #$00
    beq @exit
    jsr read_char_upper
@exit:
    rts

read_char_upper:
   jsr read_char
   bit KEYTEMP
   bvc @exit
   and #$DF
   sta KEYTEMP
@exit:
   ora #$80
   rts

read_char_async:
    lda KBCURR
    cmp #$00
    beq @exit
    jsr read_char
@exit:
    rts

;read_char_echo:
;    jsr read_char
;    jsr display_char
;    rts

read_char_upper_echo:
    jsr read_char_upper
    and #$7F
    jsr display_char
    rts

read_char:
    phx
@readloop:
    lda KBCURR
    cmp #$00
    beq @readloop  ; loop waiting for keyboard input
 
    sei
    lda KBBUF      ; this is our keyboard input
    sta KEYTEMP

    ldx #$00
@moveloop:
    inx
    lda KBBUF,x
    dex
    sta KBBUF,x
    inx
    cpx KBCURR
    bne @moveloop

    dec KBCURR

    lda KEYTEMP
    clc
    cli

    plx
    rts

; DISPLAY 

display_apple_char:
    pha
    bit dos_cout_mode
    bmi @collect_dos
    cmp #$84 ; ctrl+d means dos command
    beq @start_collect_dos

    jsr display_char
    
    pla
    rts

@start_collect_dos:
    lda #$ff
    sta dos_cout_mode
    lda #$0
    sta dos_command
    sta dos_cursor
    pla
    rts

@collect_dos:
    cmp #$8D ; carraige return means execute
    beq @execute_dos    
    phx
    ldx dos_cursor
    jsr tx_char_sync
    and #$7F
    sta dos_command,x
    inc dos_cursor
    plx
    pla
    rts
@execute_dos:
    phx
    jsr tx_char_sync
    ldx dos_cursor
    lda #$00
    sta dos_cout_mode
    sta dos_cursor
    phy
    jsr parse_command
    ply
    plx
    pla
    rts

print_char:
display_char:
    pha
    phx
    phy
    jsr tx_char_sync
    ldx MUTE_OUTPUT
    bne @done
    ora #$80    
    jsr COUT1
@done: 
    ply
    plx
    pla
    rts

display_message:
    pla
    sta	MSG_ADDR_LOW
    pla
    sta	MSG_ADDR_HIGH          ; get return address off the stack
    bne	@increturn

@nextchar:
    lda	(MSG_ADDR_LOW)		    ; next message character
    beq	@pushreturnaddr		    ; done?	yes, exit
    jsr	display_char

@increturn:					    ; next address
    inc	MSG_ADDR_LOW
    bne	@nextchar
    inc	MSG_ADDR_HIGH	   	    ; fix MSB of next address
    bne	@nextchar

@pushreturnaddr:
    lda	MSG_ADDR_HIGH
    pha
    lda	MSG_ADDR_LOW
    pha				; adjust return	address
    rts

print_crlf:
    pha
;    lda #10
;    jsr print_char
    lda #$8D
    jsr print_char
    pla
    rts

print_space:
    pha
    lda #$A0
    jsr print_char
    pla
    rts

    ; address stored in zp_sd_temp
print_hex_word:
    pha
    phy
    ldy #1
    lda (zp_sd_temp),y
    jsr print_hex
    dey
    lda (zp_sd_temp),y
    jsr print_hex

    ply
    pla
    rts

    ; address stored in zp_sd_temp
print_hex_dword:
    pha
    phy
    ldy #3
@loopdword:
    lda (zp_sd_temp),y
    jsr print_hex
    dey
    bpl @loopdword
    ply
    pla
    rts

print_hex:
    phx
    phy
    pha
    ror
    ror
    ror
    ror
    jsr print_nybble
    pla
    pha
    jsr print_nybble
    pla
    ply
    plx
    rts

print_nybble:
    and #15
    cmp #10
    bmi @skipletter
    adc #6
@skipletter:
    adc #48
    ora #$80
    jsr print_char
    rts

wozlong:
    jmp $FF00

;==========================================================================
; drawing routines
;==========================================================================

cls:
_cls:
    pha

    jsr HOME
 ;   lda #$00
 ;   sta DRAW_COLOR
 ;   sta CURSOR_X
 ;   sta CURSOR_Y

    ;jsr fillscreen
;    jsr clear_text_region

    pla
    rts

;fillscreen:    
;    pha

;    lda #$00
;    sta X1
;    sta Y1

;    lda #$28
;    sta X2    
    
;    lda #$C8  ; 200
;    sta Y2
    
;    jsr draw_char_rect

;    pla
;    rts

;draw a rectangle
;draw_char_rect:
;    pha
;    phx
;    phy

;; calculate the starting address in video memory
;    ldy Y1
;    lda eb_hires_lsb, y
;    sta ORIGIN_L

;    lda (HIRESPAGE),Y
;    sta ORIGIN_H

;    lda X1
;    clc
;    adc ORIGIN_L
;    bcc @nocarry
;    inc ORIGIN_H
;@nocarry:
;    sta ORIGIN_L

;    lda X2
;    sec
;    sbc X1
;    sta DRAW_WIDTH
    
;    lda Y2
;    sec
;    sbc Y1
;    sta DRAW_HEIGHT

;    lda DRAW_COLOR
    
;    ldx #$00
;@dr_looprow:
;    ldy #$00
;@dr_loopcol:
;    sta (ORIGIN_L),Y
;    iny
;	cpy DRAW_WIDTH
;	bne @dr_loopcol

    ; add 40 to ORIGIN_L
    ; if overflow, increment origin_H
 ;   lda #$28 ; 40
 ;   clc
 ;   adc ORIGIN_L
 ;   bcc @after_carry
 ;   inc ORIGIN_H
;@after_carry:
;    sta ORIGIN_L
;    lda DRAW_COLOR
;    inx
;	cpx DRAW_HEIGHT
;	bne @dr_looprow

;    ply
;    plx
;    pla
;    rts



; ==============================================
; CONSOLE AND FONT
; ==============================================

;clear_text_region:
;    pha
;    phx
;    phy

;@clearinput:
;    ; clear the last line and draw screen
;    ldy #$0
;    ldx #$0

;@loopx:
;    lda eb_text1_msb, x
;    sta DEST_HIGH
;    lda eb_text_lsb, x
;    sta DEST_LOW
;    lda #$00

;; clear the input line
;@loopy:
;    sta (DEST_LOW),Y

;    iny
;    cpy #$28
;    bne @loopy
;    inx
;    cpx #$18
;    bne @loopx

;    ply
;    plx
;    pla

;    rts

;console_add_char:
;    pha
;    phy
;    sta CHAR_DRAW

;; check for non printables
;    cmp #$00
;    beq @done
;    cmp #$0D
;    beq @docr
;    cmp #$0A
;    beq @dolf
;    cmp #$1b
;    beq @clear
;    cmp #$8
;    bne @storechar

;@backspace:
;;backspace handling
;    stz CHAR_DRAW  ; draw a blank char
;    lda CURSOR_X
;    bne @deletechar
;    inc CURSOR_X  ; inc here since we're already at 0 and we'll dec below
;    bra @storechar

;@deletechar:
;    dec CURSOR_X
;    ;jsr draw_char ; draw over the char
    
;@storechar:
;    ldy CURSOR_Y
;    lda eb_text1_msb, y
;    sta CURSOR_ADDR_H
;    lda eb_text_lsb, y
;    sta CURSOR_ADDR

;    ldy CURSOR_X
;    ; write to text screen mem
;    lda CHAR_DRAW
;    sta (CURSOR_ADDR),Y

;    ; if 0 char, skip to done
;    bne @draw
;    ;dec CURSOR_X
;    ;jsr draw_char    ; disable for text mode
;    bra @done

;@dolf:
;    lda #$00
;    sta CHAR_DRAW
;    inc CURSOR_Y
;    lda CURSOR_Y
;    cmp #$18    ; line 24?  scroll
;    bne @done
;    dec CURSOR_Y
;    jsr scroll_console
;    bra @done

;@docr:
;    stz CHAR_DRAW
;    stz CURSOR_X
;    bra @done

;@draw:
;    ;jsr draw_char

;@advance:
;    inc CURSOR_X
;    lda CURSOR_X
;    cmp #$38        ; 40 chars max
;    bne @done
;    stz CURSOR_X
;    bra @done


;@clear:
;    jsr cls

;@done:
;    ply
;    pla    
;    rts


; SCROLLING
;scroll_console:
;    pha
;    phy
;    phx

    ; copy from row 2-1, 3->2, etc...
;    ldx #$0
;@loopx:
;    lda eb_text1_msb, x
;    sta DEST_HIGH
;    lda eb_text_lsb, x
;    sta DEST_LOW

;    inx
;    cpx #$18
;    beq @clearinput
    
;    lda eb_text1_msb, x
;    sta SOURCE_HIGH
;    lda eb_text_lsb, x
;    sta SOURCE_LOW

;    ldy #$0
;@loopy:
    ; copy from source to dest
;    lda (SOURCE_LOW),y
;    sta (DEST_LOW),y

;;    inc SOURCE_LOW
;;    bne @skipinc1
;;    inc SOURCE_HIGH
;;@skipinc1:
    
;;    inc DEST_LOW
;;    bne @skipinc2
;;    inc DEST_HIGH
;;@skipinc2:
; 
;    iny
;    cpy #$28
;    bne @loopy
;    bra @loopx

;@clearinput:
;    ; clear the last line and draw screen
;    ldy #$0
;    ldx #$17

;    lda eb_text1_msb, x
;    sta DEST_HIGH
;    lda eb_text_lsb, x
;    sta DEST_LOW
;    lda #$00

;; clear the input line
;@loopclear:
;    sta (DEST_LOW),y

;    ;inc DEST_LOW
;    ;bne @skipinc3
;    ;inc DEST_HIGH
;;@skipinc3:
;    iny
;    cpy #$28
;    bne @loopclear

;@draw:
;    ;jsr draw_screen

;    plx
;    ply
;    pla    
;    rts

; DRAW SCREEN
; software text rendering code
;draw_screen:
;    pha
;    phy
;    phx

    ; stash the cursor
;    lda CURSOR_X
;    pha
;    lda CURSOR_Y
;    pha

;    ldx #$00
;@loopx:
;    stx CURSOR_Y
;    lda eb_text1_msb, x
;    sta DEST_HIGH
;    lda eb_text_lsb, x
;    sta DEST_LOW
;    ldy #$00

;@loopy: 
;    sty CURSOR_X
;    lda (DEST_LOW),Y
;    sta CHAR_DRAW
;    jsr draw_char

;;    inc DEST_LOW
;;    bne @skipinc
;;    inc DEST_HIGH
;;@skipinc:
;    iny
;    cpy #$28
;    bne @loopy

;    inx
;    cpx #$19
;    bne @loopx

;@done:
;    ; restore the cursor
;    pla
;    sta CURSOR_Y
;    pla
;    sta CURSOR_X
;    plx
;    ply
;    pla    

;    rts

; DRAW CHARACTER
; software character rendering
;draw_char:
;    pha
;    phx
;    phy


;    lda CHAR_DRAW
;    lsr
;    lsr
;    lsr
;    lsr
;    lsr
;    clc
;    adc #>font8x8
;    sta FONTPTR_H

;    lda CHAR_DRAW
;    asl
;    asl
;    asl
;    clc
;    adc #<font8x8
;    sta FONTPTR


;    clc
;    lda CURSOR_Y
;    asl
;    asl
;    asl
;    tay

;    ldx #$00
;@loopy:                  ; loop through 8 lines per char
;    lda (HIRESPAGE),y
;    sta ORIGIN_H
;    lda eb_hires_lsb,y

;    clc
;    adc CURSOR_X
;    bcc @skipcarry
;    inc ORIGIN_H
;@skipcarry:
;    sta ORIGIN_L
    
;    lda (FONTPTR)
;    sta (ORIGIN_L)

;    inc FONTPTR
;    bne @skipcarry2
;    inc FONTPTR_H
;@skipcarry2:
;    iny
;    inx
;    cpx #$8
;    bne @loopy

;    ply
;    plx
;    pla
;    rts


; ============================================================================================
; graphics tests
; ============================================================================================

;linetests:
;    jsr linetest1
;    jsr cls
;    jsr linetest2
;    jsr cls
;    jsr linetest3
;    rts

;linetest1:
;    stz X1
;    stz X1_H

;    stz X2_H
;    stz Y1

;    ldy #$C7
;    ldx #$01   
;    sty Y2
;@effectx:
;    ;inc DRAW_COLOR
;    stx X2
;    jsr draw_line
;    inx
;    bne @effectx
;
;    inc X2_H
;@effectx2:
;    ;inc DRAW_COLOR
;    stx X2
;    jsr draw_line
;    inx
;    cpx #$3F
;    bne @effectx2
;@donex:
;    dey
;    dex
;    stx X2

;@effecty:
;    ;inc DRAW_COLOR
;    sty Y2
;    jsr draw_line
;    dey
;    bne @effecty
;    rts


;linetest2:
;    pha
;    phx
;    phy


;    ; 0,0 - 255,128
;    lda #$02
;    sta DRAW_COLOR

;    stz X1
;    stz X1_H
;    stz Y1        ; Origin is (0,0)
    
;    lda #$01
;    sta X2_H

;    lda #$3F
;    sta X2    

;    lda #$C7
;    sta Y2        ; Destination (320x200)

;    jsr draw_line

    ; 0,128 - 255,0
;    lda #$04
;    sta DRAW_COLOR

;    stz X1_H
;    stz X1       ; (0, 200)
;    lda #$C7
;    sta Y1

;    lda #$01
;    sta X2_H
;    lda #$3F
;    sta X2        ; (320, 0)

;    stz Y2
;    jsr draw_line


;    ; draw backwards

;    ; 320,64 - 0,32 

;    lda #$01
;    sta X2_H

;    lda #$3F
;    sta X2    

;    lda #$40
;    sta Y2        ; Origin (320x64)

;    ;lda #$02
;    ;sta DRAW_COLOR

;    stz X1
;    stz X1_H

;    lda #$20
;    sta Y1        ; Dest is (0,32)
    

;    jsr draw_line

;    ply
;    plx
;    pla
;    rts

;linetest3:
;    pha
;    phx
;    phy

;    ;lda #$03
;    ;sta DRAW_COLOR

;    stz X1
;    stz X1_H
;    stz X2
;    stz X2_H
;    stz Y2
;    lda #$C7
;    sta Y1

;@loop:    

;    inc DRAW_COLOR
;    jsr draw_line

;@xlow:    
;    clc
;    lda X2
;    adc #$08
;    sta X2
;    bcs @xhigh
    
;    cmp #$3F
;    beq @xcheck
;    bra @y

;@xhigh:
;    inc X2_H

;@y:
;    sec
;    lda Y1
;    sbc #$06
;    bcc @exit
;    sta Y1    
;    bra @loop

;@xcheck:
;    lda X2_H
;    lsr
;    bcc @loop

;@exit:

;    ply
;    plx
;    pla
;    rts

;set_hires_page1:
;    pha
;    lda #<eb_hires1_msb
;    sta HIRESPAGE
;    lda #>eb_hires1_msb
;    sta HIRESPAGE_H
;    pla
;    rts

;set_hires_page2:
;    pha
;    lda #<eb_hires2_msb
;    sta HIRESPAGE
;    lda #>eb_hires2_msb
;    sta HIRESPAGE_H
;    pla
;    rts

.segment "BANKROM"
; ============================================================================================
; ROMDISK routines
; ============================================================================================
;ROMDISK VARIABLES
;RD_BYTES_LOW  = $BE
;RD_BYTES_HIGH = $BF
;RD_DEST_LOW   = $C0
;RD_DEST_HIGH  = $C1
;RD_SOURCE_LOW  = $BC
;RD_SOURCE_HIGH = $BD
;RD_LOW  = $D030
;RD_HIGH = $D031
;RD_BANK = $D032
;RD_DATA = $D033

romdisk_load:

    ; RD_BYTES_LOW / RD_BYTES_HIGH - are the number of bytes to read from the ROM disk
    ; RD_SOURCE_LOW / RD_SOURCE_HIGH - are the high and low starting address in ROM disk
    ; RD_DEST_LOW / RD_DEST_HIGH - are the starting destination address point for the copied data
    ; ramdisk copy will set RAMDISK address based on RD_SOURCE and start copying to RD_DEST from whatever RD_BANK is set

    pha
@loop:
    ldx RD_BYTES_LOW
    cpx #$00
    beq @decrement_high
    bra @copy

@decrement_high:
    ldx RD_BYTES_HIGH
    cpx #$00
    beq @done
    dec RD_BYTES_HIGH

@copy:
    dec RD_BYTES_LOW

    ; load ramdisk with source address and read, and then copy to dest address - increment both addresses
    lda SOURCE_LOW
    sta RD_LOW
    lda SOURCE_HIGH
    sta RD_HIGH
    
    lda RD_DATA        ;  data from romdisk
    sta (DEST_LOW)     ; write to destination address in RAM

    ; increment both source and dest addresses

    inc SOURCE_LOW
    bne @increment_dest
    inc SOURCE_HIGH    

@increment_dest:   
    inc DEST_LOW
    bne @loop
    inc DEST_HIGH
    bra @loop

@done:
    ldy RD_BYTES_HIGH
    cpx #$00
    
    pla
    rts

.segment "OS"
; ============================================================================================
; interrupts
; ============================================================================================

irq:
    rti

nmi:
    pha
    phx
    phy
    
    ; check the ACIA status register to see if we've received data
    ; reading the status register clears the irq bit
    lda A_STS
    and #%00001000   ; check receive bit
    bne @irq_receive

    ; check the IFR to see if it's the VIA - aka the keyboard
    lda IFR
    ror                        ; CA2
    bcs @ps2_keyboard_decode_short
    ror                        ; CA1
    bcs @CA1
    ror                        ;Shift Register
    bcs @shift
    ror                        ; CB2
    bcs @joystick_short          
    ror                        ; CB1
    bcs @kbstrobe
    ror                        ; T2
    bcs @T2
    ror                        ; T1
    bcs @T1
    
@unknown_irq:
    lda #$41
    jsr tx_char_sync
    jmp @exit

@ps2_keyboard_decode_short:
    jmp @ps2_keyboard_decode

@joystick_short:
    jmp @joystick

@irq_receive:
    ; we now have the byte, we need to add it to the keyboard buffer
    lda A_RXD
    ldx KBCURR
    ;sta KBBUF, x
    ora #$80
    sta KEYRAM
    inc KBCURR
    jmp @exit

@kbstrobe:
    lda $C000     ; strip off the high bit
    and #$7F
    sta $C000
    jmp @exit

@T2:
    lda T2L  ; clear the interrupt

    ; if neither left or right are down, we're at midpoint, discharge virtual capacitor now
    ; cap is discharged by setting to 0

    lda KEYSTATE + $6B ; left
    ora KEYSTATE + $74 ; right
    ror
    ror
    sta $C064
    sta $C066

    ; same for up/down
    lda KEYSTATE + $75 ; up
    ora KEYSTATE + $73 ; down
    ror
    ror
    sta $C065
    sta $C067
    jmp @exit

@CA1:
    lda #$47
    jsr tx_char_sync
    jmp @exit
    
@shift:
    lda #$46
    jsr tx_char_sync
    jmp @exit


@T1:
    lda T1CL ; clear the interrupt flag

    lda $C071
    beq @end_discharge

@early_discharge:    
    lda #$00
    sta $C071

    lda KEYSTATE + $6B ; left
    ror
    ror
    eor #$80
    sta $C064
    sta $C066
    lda KEYSTATE + $75 ; up
    ror
    ror
    eor #$80
    sta $C065
    sta $C067

    lda #$00
    sta T1CL
    lda #$0B
    sta T1CH  ; Set T1 for end discharge check

    jmp @exit

@end_discharge:
    ; clear bit 8 on both, we're done - our virtual capacitors have discharged
    lda #$00
    sta $C064
    sta $C065
    sta $C066
    sta $C067

    jmp @exit

;OPNAPPLE = $C061 ;open apple (command) key data (read)
;CLSAPPLE = $C062 ;closed apple (option) key data (read)
;These are actually the first two game Pushbutton inputs (PB0
;and PB1) which are borrowed by the Open Apple and Closed Apple
;keys. Bit 7 is set (=1) in these locations if the game switch or
;corresponding key is pressed.

;PB2 =      $C063 ;game Pushbutton 2 (read)
;This input has an option to be connected to the shift key on
;the keyboard. (See info on the 'shift key mod'.)

;PADDLE0 =  $C064 ;bit 7 = status of pdl-0 timer (read)
;PADDLE1 =  $C065 ;bit 7 = status of pdl-1 timer (read)
;PADDLE2 =  $C066 ;bit 7 = status of pdl-2 timer (read)
;PADDLE3 =  $C067 ;bit 7 = status of pdl-3 timer (read)
;PDLTRIG =  $C070 ;trigger paddles
;Read this to start paddle countdown, then time the period until
;$C064-$C067 bit 7 becomes set to determine the paddle position.
;This takes up to three milliseconds if the paddle is at its maximum
;extreme (reading of 255 via the standard firmware routine).

@joystick:
    lda #$80
    sta $C071
    sta $C064
    sta $C065
    sta $C066
    sta $C067

    lda #$58
    sta T2L
    lda #$6
    sta T2H  ; set T2 for half way

    lda #$40
    sta T1CL
    lda #$00
    sta T1CH  ; Set T1 for early discharge check

    jmp @exit


@ps2_keyboard_decode:
    lda PORTA
    ror
    ror       ; rotate into high order bit
    and #$80

    ldx KBSTATE
    
    cpx #PS2_KEYS
    beq @keys

    cpx #PS2_START
    beq @start 
      
    cpx #PS2_PARITY
    beq @parity

    cpx #PS2_STOP
    beq @stop

    lda #$42
    jsr tx_char_sync

    ; should never get here
    jmp @exit

    
@start:
    ; should be zero - maybe check later
    lda #PS2_KEYS
    sta KBSTATE
    lda #00
    sta KBBIT   ; reset to bit zero
    sta KBTEMP  ; clear the temp key
    inc KBDBG
    jmp @exit

@keys:
    clc
    ror KBTEMP
    ora KBTEMP
    sta KBTEMP
    inc KBDBG
    inc KBBIT
    lda KBBIT
    cmp #$08
    beq @toparity
    jmp @exit

@toparity:
    lda #PS2_PARITY
    sta KBSTATE
    jmp @exit

@parity:
    ; should probably check the parity bit - all 1 data bits + parity bit should be odd #
    lda #PS2_STOP
    sta KBSTATE
    inc KBDBG
    jmp @exit

@stop:
    ; write our temp kb to kbbuf
    inc KBDBG
    lda #PS2_START
    sta KBSTATE
    
@process_key:
    lda KBTEMP    
    
    cmp #$E0           ; set the extended bit if it's an extended character
    bne @notextended
    sta KBEXTEND
    jmp @exit          ; updated the state as extended, we're done here

@notextended:
    cmp #$F0           ; set the key up bit if it's a key up
    bne @notkeyup
    sta KBKEYUP
    lda #$00
    sta KEYRAM
    jmp @exit          ; updated key up state, we're done here
 
@notkeyup:
    lda KBKEYUP        ; check the key up flag
    cmp #$00
    beq @setkeystate

@clearkeystate:        ; this is the key up path TODO: need to update key state to use ascii code instead of scan code
    ldx KBTEMP
    lda #$00
    ; clear flags
    sta KBEXTEND
    sta KBKEYUP
    sta KEYRAM

    cpx #$58
    bne @clear
    jmp @exit

@clear:
    sta KEYSTATE,x
    jmp @exit

@setkeystate:          ; set the key state - this is key down path
    ldx KBTEMP
    cpx #$58
    beq @capstoggle

    lda #$01
    ora KBEXTEND
    sta KEYSTATE, x
    stx KEYLAST

    ; check for non printable 
    ldx KBTEMP         ; store in buffer only if it's a key down for now
    
    cpx #$12          ; left shfit
    beq @nonprint
    cpx #$59          ; right shift
    beq @nonprint
    cpx #$11           ; alt
    beq @nonprint
    cpx #$14           ; ctrl
    beq @nonprint 

    ; check for shift state
    ldx #$12
    lda KEYSTATE,x
    bne @shifted
    
    ldx #$59
    lda KEYSTATE,x
    bne @shifted


    ; check for control state
    ldx #$14
    lda KEYSTATE,x
    bne @control

    ldx KBTEMP
    lda ps2_ascii, x
    ldx KBCURR
    ;sta KBBUF, x

; check for caps lock, if it's on, send low version
    ldx KEYSTATE + $58

    beq @caps
    and #$7F
    @caps:
    sta KEYRAM
    inc KBCURR
    jmp @exit    
    
@shifted:
    ldx KBTEMP
    lda ps2_ascii_shifted, x    
    ldx KBCURR
    ;sta KBBUF, x
    ora #$80
    sta KEYRAM
    inc KBCURR
    jmp @exit

@control:
    ldx KBTEMP
    lda ps2_ascii_control, x
    ldx KBCURR
    ;sta KBBUF, x
    sta KEYRAM
    inc KBCURR
    jmp @exit

@capstoggle:
    lda KEYSTATE,X
    beq @turnon
    lda #$0
    sta KEYSTATE,x
    bra @nonprint

@turnon:
    lda #$1
    sta KEYSTATE,x

@nonprint:
@exit:

    lda #ICLR 
    sta IFR   ; clear all VIA interrupts

    lda #$00
    sta $C061
    sta $C062

    lda KEYSTATE + $6C  ; button 1
    beq @checkbutton2
    ; button pressed
    lda #$80
    sta $C061
@checkbutton2:
    lda KEYSTATE + $7D  ; button 2
    beq @btndone
    lda #$80
    sta $C062
@btndone:

    ply
    plx
    pla

    rti

do_nothing:
                RTS

.segment "DATASEG"
; ============================================================================================
; data
; ============================================================================================
ps2_ascii:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  ;.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "`", $00; 0
  ;.byte $00, $00, $00, $00, $00, "Q", "1", $00, $00, $00, "Z", "S", "A", "W", "2", $00; 1
  ;.byte $00, "C", "X", "D", "E", "4", "3", $00, $00, " ", "V", "F", "T", "R", "5", $00; 2
  ;.byte $00, "N", "B", "H", "G", "Y", "6", $00, $00, $00, "M", "J", "U", "7", "8", $00; 3
  ;.byte $00, ",", "K", "I", "O", "0", "9", $00, $00, ".", "/", "L", ";", "P", "-", $00; 4 
  ;.byte $00, $00, "'", $00, "[", "=", $00, $00, $00, $00, $0D, "]", $00, "\", $00, $00; 5
  ;.byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $88, $00, $00, $00, $00; 6
  ;.byte $00, $00, $8A, $00, $95, $8B, $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7

  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte "~", $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, $D1, $B1, $00, $00, $00, $DA, $D3, $C1, $D7, $B2, $00; 1
  .byte $00, $C3, $D8, $C4, $C5, $B4, $B3, $00, $00, $A0, $D6, $C6, $D4, $D2, $B5, $00; 2
  .byte $00, $CE, $C2, $C8, $C7, $D9, $B6, $00, $00, $00, $CD, $CA, $D5, $B7, $B8, $00; 3
  .byte $00, $AC, $CB, $C9, $CF, $B0, $B9, $00, $00, $AE, $AF, $CC, $BB, $D0, $AD, $00; 4  
  .byte $00, $00, $22, $00, $D8, $8D, $00, $00, $00, $00, $8D, $DD, $00, $DC, $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $88, $00, $00, $00, $00; 6
  .byte $00, $00, $8A, $00, $95, $8B, $9B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7

ps2_ascii_shifted:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte "`", $9B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, $D1, $A1, $00, $00, $00, $DA, $D3, $C1, $D7, $C0, $00; 1
  .byte $00, $C3, $D8, $C4, $C5, $A4, $A3, $00, $00, $A0, $D6, $C6, $D4, $D2, $A5, $00; 2
  .byte $00, $CE, $C2, $C8, $C7, $D9, $DE, $00, $00, $00, $CE, $CA, $D5, $A6, $AA, $00; 3
  .byte $00, $BC, $CB, $C9, $CF, $A9, $A8, $00, $00, $BE, $BF, $CC, ":", $D0, $DF, $00; 4  
  .byte $00, $00, $22, $00, $D8, $8D, $00, $00, $00, $00, $8D, $DD, $00, "|", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $88, $00, $00, $00, $00; 6
  .byte $00, $00, $8A, $00, $95, $8B, $9B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7
 

ps2_ascii_control:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte "~", $9B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, $91, "!", $00, $00, $00, $9A, $93, $81, $97, "@", $00; 1
  .byte $00, $83, $98, $84, $85, "$", "#", $00, $00, $A0, $96, $86, $94, $92, "%", $00; 2
  .byte $00, $8E, $82, $88, $87, $99, "^", $00, $00, $00, $8D, $0A, $95, "&", "*", $00; 3
  .byte $00, "<", $8B, $89, $8F, ")", "(", $00, $00, ">", "?", $8C, ":", $10, "_", $00; 4
  .byte $00, $00, $A2, $00, "{", "+", $00, $00, $00, $00, $8D, "}", $00, "|", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  .byte $00, $00, $00, $00, $00, $00, $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7


; ebadger6502 high res lookup tables
;eb_hires1_msb:
;        .byte $20,$20,$20,$20,$20,$20,$20,$21
;        .byte $21,$21,$21,$21,$21,$22,$22,$22
;        .byte $22,$22,$22,$22,$23,$23,$23,$23
;        .byte $23,$23,$24,$24,$24,$24,$24,$24
;        .byte $25,$25,$25,$25,$25,$25,$25,$26
;        .byte $26,$26,$26,$26,$26,$27,$27,$27
;        .byte $27,$27,$27,$27,$28,$28,$28,$28
;        .byte $28,$28,$29,$29,$29,$29,$29,$29
;        .byte $2A,$2A,$2A,$2A,$2A,$2A,$2A,$2B
;        .byte $2B,$2B,$2B,$2B,$2B,$2C,$2C,$2C
;        .byte $2C,$2C,$2C,$2C,$2D,$2D,$2D,$2D
;        .byte $2D,$2D,$2E,$2E,$2E,$2E,$2E,$2E
;        .byte $2F,$2F,$2F,$2F,$2F,$2F,$2F,$30
;        .byte $30,$30,$30,$30,$30,$31,$31,$31
;        .byte $31,$31,$31,$31,$32,$32,$32,$32
;        .byte $32,$32,$33,$33,$33,$33,$33,$33
;        .byte $34,$34,$34,$34,$34,$34,$34,$35
;        .byte $35,$35,$35,$35,$35,$36,$36,$36
;        .byte $36,$36,$36,$36,$37,$37,$37,$37
;        .byte $37,$37,$38,$38,$38,$38,$38,$38
;        .byte $39,$39,$39,$39,$39,$39,$39,$3A
;        .byte $3A,$3A,$3A,$3A,$3A,$3B,$3B,$3B
;        .byte $3B,$3B,$3B,$3B,$3C,$3C,$3C,$3C
;        .byte $3C,$3C,$3D,$3D,$3D,$3D,$3D,$3D
;        .byte $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3F
;eb_hires_lsb:
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
;        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
;        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
;        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
;        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
;        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
;        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
;        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
;        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
;        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
;        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
;        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
;        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
;        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
;eb_hires2_msb:
;        .byte $40,$40,$40,$40,$40,$40,$40,$41
;        .byte $41,$41,$41,$41,$41,$42,$42,$42
;        .byte $42,$42,$42,$42,$43,$43,$43,$43
;        .byte $43,$43,$44,$44,$44,$44,$44,$44
;        .byte $45,$45,$45,$45,$45,$45,$45,$46
;        .byte $46,$46,$46,$46,$46,$47,$47,$47
;        .byte $47,$47,$47,$47,$48,$48,$48,$48
;        .byte $48,$48,$49,$49,$49,$49,$49,$49
;        .byte $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4B
;        .byte $4B,$4B,$4B,$4B,$4B,$4C,$4C,$4C
;        .byte $4C,$4C,$4C,$4C,$4D,$4D,$4D,$4D
;        .byte $4D,$4D,$4E,$4E,$4E,$4E,$4E,$4E
;        .byte $4F,$4F,$4F,$4F,$4F,$4F,$4F,$50
;        .byte $50,$50,$50,$50,$50,$51,$51,$51
;        .byte $51,$51,$51,$51,$52,$52,$52,$52
;        .byte $52,$52,$53,$53,$53,$53,$53,$53
;        .byte $54,$54,$54,$54,$54,$54,$54,$55
;        .byte $55,$55,$55,$55,$55,$56,$56,$56
;        .byte $56,$56,$56,$56,$57,$57,$57,$57
;        .byte $57,$57,$58,$58,$58,$58,$58,$58
;        .byte $59,$59,$59,$59,$59,$59,$59,$5A
;        .byte $5A,$5A,$5A,$5A,$5A,$5B,$5B,$5B
;        .byte $5B,$5B,$5B,$5B,$5C,$5C,$5C,$5C
;        .byte $5C,$5C,$5D,$5D,$5D,$5D,$5D,$5D
;        .byte $5E,$5E,$5E,$5E,$5E,$5E,$5E,$5F

;eb_text1_msb:
;        .byte $04,$04,$05,$05
;        .byte $06,$06,$07,$07
;        .byte $04,$04,$05,$05
;        .byte $06,$06,$07,$07
;        .byte $04,$04,$05,$05
;        .byte $06,$06,$07,$07

;eb_text2_msb:
;        .byte $08,$08,$09,$09
;        .byte $0A,$0A,$0B,$0B
;        .byte $08,$08,$09,$09
;        .byte $0A,$0A,$0B,$0B
;        .byte $08,$08,$09,$09
;        .byte $0A,$0A,$0B,$0B

;eb_text_lsb:
;        .byte $00,$80,$00,$80
;        .byte $00,$80,$00,$80
;        .byte $28,$A8,$28,$A8
;        .byte $28,$A8,$28,$A8
;        .byte $50,$D0,$50,$D0
;        .byte $50,$D0,$50,$D0



; ****************************************************************************************************************
;  The WOZ Monitor for the Apple 1
;  Written by Steve Wozniak in 1976


; Page 0 Variables

;XAML            = $24           ;  Last "opened" location Low
;XAMH            = $25           ;  Last "opened" location High
;STL             = $26           ;  Store address Low
;STH             = $27           ;  Store address High
;L               = $28           ;  Hex value parsing Low
;H               = $29           ;  Hex value parsing High
;YSAV            = $2A           ;  Used to see if hex value is given
;MODE            = $2B           ;  $00=XAM, $7F=STOR, $AE=BLOCK XAM


; Other Variables

; IN              = $CF00    ;  Input buffer to +$7F

.segment "WOZ"

;WOZMON:         CLD             ; Clear decimal arithmetic mode.
;                CLI
;NOTCR:          CMP #$08 + $80  ; "\B"?
;                BEQ BACKSPACE   ; Yes.
;                CMP #$9B        ; ESC?
;                BEQ ESCAPE      ; Yes.
;                INY             ; Advance text index.
;                BPL NEXTCHAR    ; Auto ESC if > 127.
;ESCAPE:         LDA #'\'        ; "\".
;                JSR ECHO        ; Output it.
;GETLINE:        LDA #$0D        ; CR.
;                JSR ECHO        ; Output it.
;                LDA #$0A        ; LF.
;                JSR ECHO        ; Output it.
;                LDY #$01        ; Initialize text index.
;BACKSPACE:      DEY             ; Back up text index.
;                BMI GETLINE     ; Beyond start of line, reinitialize.
;NEXTCHAR:       jsr read_char_upper   ; Key ready?
;                STA IN,Y        ; Add to text buffer.
;                JSR ECHO        ; Display character.
;                CMP #$8D        ; CR?
;                BNE NOTCR       ; No.
;                LDY #$FF        ; Reset text index.
;                LDA #$00        ; For XAM mode.
;                TAX             ; 0->X.
;SETSTOR:        ASL             ; Leaves $7B if setting STOR mode.
;SETMODE:        STA MODE        ; $00=XAM $7B=STOR $AE=BLOK XAM
;BLSKIP:         INY             ; Advance text index.
;NEXTITEM:       LDA IN,Y        ; Get character.
;                CMP #$8D        ; CR?
;                BEQ GETLINE     ; Yes, done this line.
;                CMP #'.' + $80  ; "."?
;                BCC BLSKIP      ; Skip delimiter.
;                BEQ SETMODE     ; Yes. Set STOR mode.
;                CMP #':' + $80  ; ":"?
;                BEQ SETSTOR     ; Yes. Set STOR mode.
;                CMP #'R' + $80  ; "R"?
;                BEQ WOZRUN      ; Yes. Run user program.
;                STX L           ; $00-> L.
;                STX H           ; and H.
;                STY YSAV        ; Save Y for comparison.
;NEXTHEX:        LDA IN,Y        ; Get character for hex test.
;                EOR #$B0        ; Map digits to $0-9.
;                CMP #$0A        ; Digit?
;                BCC DIG         ; Yes.
;                ADC #$88        ; Map letter "A"-"F" to $FA-FF.
;                CMP #$FA        ; Hex letter?
;                BCC NOTHEX      ; No, character not hex.
;DIG:            ASL
;                ASL             ; Hex digit to MSD of A.
;                ASL
;                ASL
;                LDX #$04        ; Shift count.
;HEXSHIFT:       ASL             ; Hex digit left, MSB to carry.
;                ROL L           ; Rotate into LSD.
;                ROL H           ;  Rotate into MSD’s.
;                DEX             ; Done 4 shifts?
;                BNE HEXSHIFT    ; No, loop.
;                INY             ; Advance text index.
;                BNE NEXTHEX     ; Always taken. Check next char for hex.
;NOTHEX:         CPY YSAV        ; Check if L, H empty (no hex digits).
;                BEQ ESCAPE      ; Yes, generate ESC sequence.
;                BIT MODE        ; Test MODE byte.
;                BVC NOTSTOR     ;  B6=0 STOR 1 for XAM & BLOCK XAM
;                LDA L           ; LSD’s of hex data.
;                STA (STL,X)     ; Store at current ‘store index’.
;                INC STL         ; Increment store index.
;                BNE NEXTITEM    ; Get next item. (no carry).
;                INC STH         ; Add carry to ‘store index’ high order.
;TONEXTITEM:     JMP NEXTITEM    ; Get next command item.
;WOZRUN:         JMP (XAML)      ; Run at current XAM index.
;NOTSTOR:        BMI XAMNEXT     ; B7=0 for XAM, 1 for BLOCK XAM.
;                LDX #$02        ; Byte count.
;SETADR:         LDA L-1,X       ; Copy hex data to
;                STA STL-1,X     ; ‘store index’.
;                STA XAML-1,X    ; And to ‘XAM index’.
;                DEX             ; Next of 2 bytes.
;                BNE SETADR      ; Loop unless X=0.
;NXTPRNT:        BNE PRDATA      ; NE means no address to print.
;                LDA #$8D        ; CR.
;                JSR ECHO        ; Output it.
;                LDA #$8A        ; LF
;                JSR ECHO        ; Output it.
;                LDA XAMH        ; ‘Examine index’ high-order byte.
;                JSR PRBYTE      ; Output it in hex format.
;                LDA XAML        ; Low-order ‘examine index’ byte.
;                JSR PRBYTE      ; Output it in hex format.
;                LDA #':' + $80  ; ":".
;                JSR ECHO        ; Output it.
;PRDATA:         LDA #$A0        ; Blank.
;                JSR ECHO        ; Output it.
;                LDA (XAML,X)    ; Get data byte at ‘examine index’.
;                JSR PRBYTE      ; Output it in hex format.
;XAMNEXT:        STX MODE        ; 0->MODE (XAM mode).
;                LDA XAML
;                CMP L           ; Compare ‘examine index’ to hex data.
;                LDA XAMH
;                SBC H
;                BCS TONEXTITEM  ; Not less, so no more data to output.
;                INC XAML
;                BNE MOD8CHK     ; Increment ‘examine index’.
;                INC XAMH
;MOD8CHK:        LDA XAML        ; Check low-order ‘examine index’ byte
;                AND #$07        ; For MOD 8=0
;                BPL NXTPRNT     ; Always taken.
;PRBYTE:         PHA             ; Save A for LSD.
;                LSR
;                LSR
;                LSR             ; MSD to LSD position.
;                LSR
;                JSR PRHEX       ; Output hex digit.
;                PLA             ; Restore A.
;PRHEX:          AND #$0F        ; Mask LSD for hex print.
;                ORA #'0' + $80  ; Add "0".
;                CMP #$BA        ; Digit?
;                BCC ECHO        ; Yes, output it.
;                ADC #$06        ; Add offset for letter.
;ECHO:           pha
;                and #$7F
;                jsr display_char
;                pla
;                RTS             ; Return.


.segment "BOOTVECTORS"
    .word nmi
    .word init
    .word IRQ 
