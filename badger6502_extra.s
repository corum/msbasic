.segment "A2MON"
.include "apple2rom.s"

.segment "BANKROM"

;Keyboard
KEYRAM         = $C000

;ACIA C0
A_RXD          = $C100
A_TXD          = $C100
A_STS          = $C101
A_RES          = $C101
A_CMD          = $C102
A_CTL          = $C103

; devices
;VIA D1
PORTB          = $C200
PORTA          = $C201
DDRB           = $C202
DDRA           = $C203
T1CL           = $C204
T1CH           = $C205
T1LL           = $C206
T1LH           = $C207
T2L            = $C208
T2H            = $C209
SHCTL          = $C20A
ACR            = $C20B     ; auxiliary control register
PCR            = $C20C     ; peripheral control register
IFR            = $C20D 
IER            = $C20E     ; interrupt enable register

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
ICLR           = %01111111  ; clear all VIA interrupts

;SD card pins
SD_CS          = %00010000
SD_SCK         = %00001000
SD_MOSI        = %00000100
SD_MISO        = %00000010

PORTA_OUTPUTPINS = %11100000 | SD_CS | SD_SCK | SD_MOSI

;GAMEPAD pins
GC_CLOCK       = %10000000
GC_LATCH       = %01000000
GC_DATA1       = %00100000
GC_DATA2       = %00010000

PS2_MOUSE_CLK  = %00000010
PS2_MOUSE_DATA = %00000001

PORTB_OUTPUTPINS = GC_CLOCK | GC_LATCH

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
MOUSE_SEND     = $CE22
MOUSE_FLAGS    = $CE23
MOUSE_X_POS    = $CE24
MOUSE_Y_POS    = $CE25
MOUSE_BYTE     = $CE26
MOUSE_REPORT   = $CE27
JOYSTICK_MODE  = $CE28
MOUSE_STATE    = $CE29
MOUSE_BIT      = $CE2A


; Joystick modes
JOY_MODE_PADS  = 0
JOY_MODE_KEYS  = 1
JOY_MODE_MOUSE = 2

; keyboard processing states
PS2_START      = $00
PS2_KEYS       = $01
PS2_PARITY     = $02
PS2_STOP       = $03

; mouse processing states
PS2_M_START    = $00
PS2_M_BITS     = $01
PS2_M_PARITY   = $02
PS2_M_STOP     = $03

; Mouse states
MOUSE_REPORT_A  = 0
MOUSE_REPORT_X  = 1
MOUSE_REPORT_Y  = 2

; starting of 512 byte buffer used by fat32
fat32_workspace= $C800  ; $C800 - $C9FF

KBBUF           = $0200 ; $CA00
KEYSTATE        = $CB00
fat32_variables = $CC00
GAMEPAD1        = $CEE0
GAMEPAD2        = $CEF0

;GAMEPAD INDICES

GAMEPAD_B       = $0
GAMEPAD_Y       = $1
GAMEPAD_SELECT  = $2
GAMEPAD_START   = $3
GAMEPAD_UP      = $4
GAMEPAD_DOWN    = $5
GAMEPAD_LEFT    = $6
GAMEPAD_RIGHT   = $7
GAMEPAD_A       = $8
GAMEPAD_X       = $9
GAMEPAD_L       = $A
GAMEPAD_R       = $B

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

mouse_on:
    lda #$F3
    sta MOUSE_SEND
    jsr mouse_message   ; set sampling rate
    jsr ps2_read_packet

    lda #$0A
    sta MOUSE_SEND
    jsr mouse_message  ; set sampling rate to 10 reports per second
    jsr ps2_read_packet

    lda #$E8 ; set resolution
    sta MOUSE_SEND
    jsr mouse_message
    jsr ps2_read_packet

    lda #$00 ; set resolution to 1 count/mm
    sta MOUSE_SEND
    jsr mouse_message
    jsr ps2_read_packet

    lda #$F4
    sta MOUSE_SEND
    jsr mouse_message
    jsr ps2_read_packet

    jsr via_init ; turn interrupts back on

    rts

mouse_off:
    lda #$F5
    sta MOUSE_SEND
    jsr mouse_message
    rts

mouse_message:
    lda #%01111111 
    sta IER        ; disable VIA interrupts for now

    lda #PS2_MOUSE_CLK
    sta DDRB       ; make clock output pin

    lda #$00
    sta PORTB      ; pull clock low
    
    ; wait for 100 microseconds+
    ldx #$30
@pause:
    dex
    bne @pause

    lda #PS2_MOUSE_DATA | PS2_MOUSE_CLK
    sta DDRB       ; take the data as output

    lda #$00
    sta PORTB      ; pull data low

    lda #PS2_MOUSE_DATA  
    sta DDRB       ; release the clock line

    ldy #$07 ; 8 bits
    lda MOUSE_SEND ; ready device code

    clc
    jsr mouse_send_bit   ; start bit is a zero

    ldx #$1 ; odd parity
@sendbyte:
    ror                  ; bit 0 -> carry
    bcc @send
    inx                  ; increment x for parity, if carry is set, it's a 1
@send:
    jsr mouse_send_bit
    dey
    bpl @sendbyte

    txa ; parity stored in X
    ror ; move bit 0->Carry
    jsr mouse_send_bit

    sec ; set carry for stop bit
    jsr mouse_send_bit
    rts

mouse_send_bit:
    pha
;set data
    rol                       ; PS2_MOUSE_DATA
    sta PORTB

@waithigh:
    lda PORTB
    and #PS2_MOUSE_CLK
    beq @waithigh

; wait for clock to drops
@waitlow:
    lda PORTB
    and #PS2_MOUSE_CLK
    bne @waitlow

    pla
    rts

ps2_read_packet:
    lda #$00
    sta MOUSE_BYTE

    jsr ps2_readbit ; start bit
    
    ldx #$8
@loop:
    jsr ps2_readbit ; bit
    rol MOUSE_BYTE
    dex
    bne @loop

    jsr ps2_readbit ; parity
    jsr ps2_readbit ; stop bit

    lda MOUSE_BYTE
    jsr print_hex
    jsr print_crlf

    rts

; ps2_readbit waits on ps/2 clock
; populates carry flag with data bit
ps2_readbit:
    jsr ps2_waitlow
    lda PORTB          ; read a bit
    ror                ; populate the carry bit
    jsr ps2_waithigh
    rts

ps2_waitlow:
    ; wait for mouse clock to go low
    lda PORTB
    and #PS2_MOUSE_CLK
    bne ps2_waitlow
    rts

ps2_waithigh:
    ; wait for mouse clock to go high
    lda PORTB
    and #PS2_MOUSE_CLK
    beq ps2_waithigh
    rts

; =================================================================================

via_init:
    lda #PORTA_OUTPUTPINS   ; Set various pins on port A to output
    sta DDRA

    lda #PORTB_OUTPUTPINS
    sta DDRB

    lda #%00100010  ; configure CA1 and CA2 for negative active edge for PS/2 clocks
    sta PCR        ; configure CA2 for negative edge independent interrupt, for PS/2, CB2 for negative interrupt for keyboard strobe

    lda #$0
    sta ACR

    lda #%11111011 
    sta IER        ; enable interrupts for CA2, CA1, CB1, CB2 and Timer1, Timer2

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

    ; init joystick
    sta $C064
    sta $C065
    sta $C066
    sta $C067

    sta MUTE_OUTPUT
    sta JOYSTICK_MODE  ; init to gamepads
    sta MOUSE_STATE
    sta MOUSE_REPORT
    sta MOUSE_BIT

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

    ; mouse to center
    lda #$80
    sta MOUSE_X_POS
    sta MOUSE_Y_POS

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
    .byte "NOFOUND"
    .byte $8D,0

MSG_FILE_ERROR:
    .byte "ERR"
    .byte $8D,0


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

;read_char_upper_echo:
;    jsr read_char_upper
;    and #$7F
;    jsr display_char
;    rts

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
    jsr tx_char_sync
    ldx MUTE_OUTPUT
    bne @done
    ora #$80    
    jsr COUT1
@done: 
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

;wozlong:
;    jmp $FF00

;==========================================================================
; drawing routines
;==========================================================================

cls:
_cls:
    ;pha

    jmp HOME
 ;   lda #$00
 ;   sta DRAW_COLOR
 ;   sta CURSOR_X
 ;   sta CURSOR_Y

    ;jsr fillscreen
;    jsr clear_text_region

    ;pla
    ;rts

.segment "BANKROM"

; some test routines

keytest:
    lda KEYRAM
    jsr print_hex
    jsr print_space
    lda KBTEMP
    jsr print_hex
    jsr print_crlf
    jmp keytest

mousetest:
    jsr _cls
@loop:
    lda     WNDTOP
    sta     CV
    ldy     #$00
    sty     CH
    
    jsr display_message
    .byte $8D,"X     =",0
    lda MOUSE_X_POS
    jsr print_hex
    jsr display_message
    .byte $8D, "Y     =",0
    lda MOUSE_Y_POS
    jsr print_hex

    jsr display_message
    .byte $8D,"BUTTON=", 0
    lda MOUSE_FLAGS
    and #$7
    jsr print_nybble

    jsr display_message
    .byte $8D, "FLAGS =",0
    lda MOUSE_FLAGS
    jsr print_hex

    jsr display_message
    .byte $8D, "STATE =",0
    lda MOUSE_STATE
    jsr print_hex

    jsr display_message
    .byte $8D, "REPORT=",0
    lda MOUSE_REPORT
    jsr print_hex

    jsr display_message
    .byte $8D, "BYTE  =",0
    lda MOUSE_BYTE
    jsr print_hex

    jsr display_message
    .byte $8D, "BIT   =",0
    lda MOUSE_BIT
    jsr print_hex

    jmp @loop

    
joytest:
    jsr _cls
@loop:
    ; move cursor to top right of screen
    lda     WNDTOP
    sta     CV
    ldy     #$00
    sty     CH

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

    lda $C061
    jsr print_space
    jsr print_hex
    
    jsr print_space
    lda $C062
    jsr print_hex

    jsr print_space
    lda $C063
    jsr print_hex

    jsr print_crlf

    jsr display_message
    .byte "G1: ",0

    ldx #$0
@dump_gamepad_1:
    lda GAMEPAD1,x
    bne @actuated1
    lda #'0'
    bra @notactuated1
@actuated1:
    lda #'1'
@notactuated1:
    jsr print_char
    inx
    cpx #$10
    bne @dump_gamepad_1  

    jsr print_crlf

    jsr display_message
    .byte "G2: ",0

    ldx #$0
@dump_gamepad_2:
    lda GAMEPAD2,x
    bne @actuated2
    lda #'0'
    bra @notactuated2
@actuated2:
    lda #'1'
@notactuated2:
    jsr print_char
    inx
    cpx #$10
    bne @dump_gamepad_2

    jsr print_crlf

    jmp @loop


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
    ; check the ACIA status register to see if we've received data
    ; reading the status register clears the irq bit
    lda A_STS
    and #%00001000   ; check receive bit
    bne @irq_receive
    beq @check_via_interrupts

@irq_receive:
    ; we now have the byte, we need to add it to the keyboard buffer
    lda A_RXD
    ldx KBCURR
    ;sta KBBUF, x
    ora #$80
    sta KEYRAM
    inc KBCURR
    bra @exit_long

@ps2_keyboard_decode_long:
    jmp @ps2_keyboard_decode

@joystick_long:
    jmp @joystick

@check_via_interrupts:
    ; check the IFR to see if it's the VIA - aka the keyboard
    lda IFR
    ror
    bcs @ps2_keyboard_decode_long   ; bit 0
    ror
    bcs @ps2_mouse_decode           ; bit 1
    ror
    bcs @shift_long                 ; bit 2
    ror
    bcs @joystick_long              ; bit 3
    ror
    bcs @kbstrobe                   ; bit 4
    ror
    bcs @T2_long                    ; bit 5
    ror
    bcs @T1_long                    ; bit 6
    
@unknown_irq:
    ;lda #$41
    ;jsr tx_char_sync
    bra @exit_long

@T1_long:
    jmp @T1

@T2_long:
    jmp @T2

@shift_long:
    jmp @shift

@kbstrobe:
    lda $C000     ; strip off the high bit
    and #$7F
    sta $C000  
    lda PORTB
@exit_long:
    bra @exit_long_5

@ps2_mouse_decode:      ; decode 11 bits from the PS/2 mouse
    ldx MOUSE_STATE    

    cpx #PS2_M_START
    beq @m_start 

    cpx #PS2_M_BITS
    beq @m_bits
      
    cpx #PS2_M_PARITY
    beq @m_parity

    cpx #PS2_M_STOP
    beq @m_stop

    ; should never get here
    bra @exit_long_5

@m_start:
    ; should be zero - maybe check later
    lda #PS2_M_BITS
    sta MOUSE_STATE
    lda #$00
    sta MOUSE_BIT
    sta MOUSE_BYTE
@exit_long_5:
    jmp @exit

@m_bits:
    lda PORTB
    ror             ; move PS2_MOUSE_DATA into carry bit

    rol MOUSE_BYTE
    inc MOUSE_BIT
    lda MOUSE_BIT
    cmp #$08        ; 8th bit in the byte
    beq @m_toparity
    bra @exit_long_5

@m_toparity:
    lda #PS2_M_PARITY
    sta MOUSE_STATE
    bra @exit_long_5

@m_parity:
    ; should probably check the parity bit - all 1 data bits + parity bit should be odd #
    lda #PS2_M_STOP
    sta MOUSE_STATE
    bra @exit_long_5

@m_stop:
    lda #PS2_M_START
    sta MOUSE_STATE
    
@process_mouse_report:
    lda MOUSE_REPORT
    cmp #MOUSE_REPORT_A
    bne @report_x 
    lda MOUSE_BYTE
    and #$8                ; in the flag report, bit 3 is always 1
    beq @exit_long_5       ; zero flag enabled means bit not set
    lda MOUSE_BYTE
    sta MOUSE_FLAGS
    inc MOUSE_REPORT       ; #MOUSE_REPORT_X
    bra @exit_long_5

@report_x:       
    cmp #MOUSE_REPORT_X
    bne @report_y
    inc MOUSE_REPORT       ; #MOUSE_REPORT_Y

    lda MOUSE_FLAGS
    and #$10          ; x sign bit - negative if its a 1
    beq @addx
    lda MOUSE_X_POS
    sec 
    sbc MOUSE_BYTE
    bcc @exit_long_5  ; if < 0 , just don't add
    sta MOUSE_X_POS
    bra @exit_long_5
@addx:
    clc
    lda MOUSE_X_POS
    adc MOUSE_BYTE
    bcs @exit_long_5    ; in > 255, don't add
    sta MOUSE_X_POS
    bra @exit_long_5

@report_y:
    stz MOUSE_REPORT  ; reset expected report
    lda MOUSE_FLAGS
    and #$20          ; negative bit for y
    beq @addy
    lda MOUSE_Y_POS 
    sec
    sbc MOUSE_BYTE
    bcc @exit_long_3  ; in overflow, don't add
    sta MOUSE_Y_POS
    bra @exit_long_3
    
@addy:
    clc
    lda MOUSE_Y_POS
    adc MOUSE_BYTE
    bcs @exit_long_3  ; in overflow, don't add
    sta MOUSE_Y_POS
    bra @exit_long_3

@shift:
    lda #$4
    sta IFR
@exit_long_3:
    jmp @exit_long

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

@T2:
    ; if neither left or right are down, we're at midpoint, discharge virtual capacitor now
    ; cap is discharged by setting to 0

    lda T2L  ; clear the interrupt

    ; gamepad 1
    clc
    lda KEYSTATE + $74 ; right
    ora KEYSTATE + $8D ; right/up
    ora KEYSTATE + $7A ; right/down
    ora GAMEPAD1 + GAMEPAD_RIGHT
    ror
    ror
    ora #$7F
    sta $C064


    ; same for up/down
    clc
    lda KEYSTATE + $73 ; 5 on numpad - treat as down for convenience
    ora KEYSTATE + $72 ; down arrow
    ora KEYSTATE + $69 ; down/left
    ora KEYSTATE + $7A ; down/right
    ora GAMEPAD1 + GAMEPAD_DOWN
    ror
    ror
    ora #$7F
    sta $C065


    ; gamepad 2
    clc
    lda GAMEPAD2 + GAMEPAD_RIGHT
    ror
    ror
    ora #$7F
    sta $C066


    ; same for up/down
    clc
    lda GAMEPAD2 + GAMEPAD_DOWN
    ror
    ror
    ora #$7F
    sta $C067

    bra @exit_long_3

@T1:
    lda T1CL ; clear the interrupt flag

    ; clear bit 7 on both, we're done - our virtual capacitors have discharged
    lda #$7F
    sta $C064
    sta $C065
    sta $C066
    sta $C067

    bra @exit_long_3

@joystick:
    lda #$FF
    sta $C064
    sta $C065
    sta $C066
    sta $C067

    ; read inputs from gamepads
    ; first, latch the input
    lda #$00
    sta PORTB
    lda #GC_LATCH
    sta PORTB

    phy
    ldy #$1
    ldx #$00
@read_controllers:    
    lda #$00          ; first time through, this drops the latch pulse
    sta PORTB
    sta GAMEPAD1,x
    sta GAMEPAD2,x

    lda PORTB
    eor #$FF
    and #GC_DATA1
    beq @check2
    tya
    sta GAMEPAD1,x

@check2:
    lda PORTB
    eor #$FF
    and #GC_DATA2
    beq @next_clock
    tya
    sta GAMEPAD2,x

@next_clock:
    lda #GC_CLOCK  ; clock rise
    sta PORTB
    inx
    cpx #$10
    bne @read_controllers
    
    ; we're done, bring the clock back down
    lda #$00          ; first time through, this drops the latch pulse
    sta PORTB         
    ply

    ;   set the buttons
    ; button pressed
    lda KEYSTATE + $12  ; shift key
    ora GAMEPAD1 + GAMEPAD_Y
    ora GAMEPAD1 + GAMEPAD_B
    ror
    ror
    sta $C061
     
    lda KEYSTATE + $14  ; ctrl key
    ora GAMEPAD1 + GAMEPAD_X
    ora GAMEPAD1 + GAMEPAD_A
    ora GAMEPAD2 + GAMEPAD_Y
    ora GAMEPAD2 + GAMEPAD_B
    ror
    ror
    sta $C062

    lda GAMEPAD2 + GAMEPAD_X
    ora GAMEPAD2 + GAMEPAD_A
    ror
    ror
    sta $C063

    lda #$00
    sta T2L
    lda #$6
    sta T2H  ; set T2 for half way

    lda #$90
    sta T1CL
    lda #$0B
    sta T1CH  ; Set T1 for end discharge check

    clc
    lda KEYSTATE + $6B ; left
    ora KEYSTATE + $6C ; left/up
    ora KEYSTATE + $69 ; left/down
    ora GAMEPAD1 + GAMEPAD_LEFT
    eor #$01
    ror
    ror
    ora #$7F
    sta $C064

    clc
    lda KEYSTATE + $75 ; up
    ora KEYSTATE + $6C ; up/left
    ora KEYSTATE + $7D ; up/right
    ora GAMEPAD1 + GAMEPAD_UP
    eor #$01
    ror
    ror
    ora #$7F
    sta $C065

    clc
    lda GAMEPAD2 + GAMEPAD_LEFT
    eor #$01
    ror
    ror
    ora #$7F
    sta $C066

    clc
    lda GAMEPAD2 + GAMEPAD_UP
    eor #$01
    ror
    ror
    ora #$7F
    sta $C067

    lda PORTB  ; clear the interrupt

@exit_long_4:
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

    ;lda #$42
    ;jsr tx_char_sync

    ; should never get here
    bra @exit_long_2

@start:
    ; should be zero - maybe check later
    lda #PS2_KEYS
    sta KBSTATE
    lda #00
    sta KBBIT   ; reset to bit zero
    sta KBTEMP  ; clear the temp key
    ;inc KBDBG
@exit_long_2:
    jmp @exit

@keys:
    clc
    ror KBTEMP
    ora KBTEMP
    sta KBTEMP
    ;inc KBDBG
    inc KBBIT
    lda KBBIT
    cmp #$08
    beq @toparity
    bra @exit_long_2

@toparity:
    lda #PS2_PARITY
    sta KBSTATE
    bra @exit_long_2

@parity:
    ; should probably check the parity bit - all 1 data bits + parity bit should be odd #
    lda #PS2_STOP
    sta KBSTATE
    ;inc KBDBG
    bra @exit_long_2

@stop:
    ; write our temp kb to kbbuf
    ;inc KBDBG
    lda #PS2_START
    sta KBSTATE
    
@process_key:
    lda KBTEMP    
    
    cmp #$E0           ; set the extended bit if it's an extended character
    bne @notextended
    sta KBEXTEND
    bra @exit_long_2   ; updated the state as extended, we're done here

@notextended:
    cmp #$F0           ; set the key up bit if it's a key up
    bne @notkeyup
    sta KBKEYUP
    jmp @checkbuttons          ; updated key up state, we're done here
 
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
    ;sta KEYRAM

    cpx #$58
    bne @clear
    bra @checkbuttons

@clear:
    sta KEYSTATE,x
    bra @checkbuttons

@setkeystate:          ; set the key state - this is key down path
    ldx KBTEMP
    cpx #$58
    beq @capstoggle

    lda #$01
    ora KBEXTEND
    sta KEYSTATE, x
    stx KEYLAST

    ; check for non printable 
    ; x already contains KBTEMP
    ;ldx KBTEMP         ; store in buffer only if it's a key down for now
    
    cpx #$12          ; left shfit
    beq @nonprint
    cpx #$59          ; right shift
    beq @nonprint
    cpx #$11           ; alt
    beq @nonprint
    cpx #$14           ; ctrl
    beq @nonprint 

    ; check for shift state
    lda KEYSTATE + $12
    ora KEYSTATE + $59
    bne @shifted
    
    ; check for control state
    lda KEYSTATE + $14
    bne @control

    ldx KBTEMP
    lda ps2_ascii, x
    ldx KBCURR
    ;sta KBBUF, x

; check for caps lock, if it's on, send low version
    ldx KEYSTATE + $58
    beq @caps
    ;and #$7F
    @caps:
    sta KEYRAM
    inc KBCURR
    bra @checkbuttons
    
@shifted:
    ldx KBTEMP
    lda ps2_ascii_shifted, x    
    ldx KBCURR
    ;sta KBBUF, x
    ora #$80
    sta KEYRAM
    inc KBCURR
    bra @checkbuttons

@control:
    ldx KBTEMP
    lda ps2_ascii_control, x
    ldx KBCURR
    ;sta KBBUF, x
    sta KEYRAM
    inc KBCURR
    bra @checkbuttons

@capstoggle:
    lda KEYSTATE,X
    beq @turnon
    lda #$0
    sta KEYSTATE,x
    bra @nonprint

@turnon:
    lda #$81
    sta KEYSTATE,x

@nonprint:
@checkbuttons:
    

@exit:
    lda #$7F
    STA IFR

    ;ply
    plx
    pla
    rti

do_nothing:
    RTS

.segment "RETURN"
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    rts
    
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
  .byte $00, $00, $22, $00, $D8, $BD, $00, $00, $00, $00, $8D, $DD, $00, $DC, $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $88, $00, $00, $00, $00; 6
  .byte $00, $00, $8A, $00, $95, $8B, $9B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7

ps2_ascii_shifted:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte "`", $9B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, $D1, $A1, $00, $00, $00, $DA, $D3, $C1, $D7, $C0, $00; 1
  .byte $00, $C3, $D8, $C4, $C5, $A4, $A3, $00, $00, $A0, $D6, $C6, $D4, $D2, $A5, $00; 2
  .byte $00, $CE, $C2, $C8, $C7, $D9, $DE, $00, $00, $00, $CE, $CA, $D5, $A6, $AA, $00; 3
  .byte $00, $BC, $CB, $C9, $CF, $A9, $A8, $00, $00, $BE, $BF, $CC, ":", $D0, $DF, $00; 4  
  .byte $00, $00, $22, $00, $D8, $BD, $00, $00, $00, $00, $8D, $DD, $00, "|", $00, $00; 5
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



.segment "BOOTVECTORS"
    .word nmi
    .word init
    .word IRQ 
