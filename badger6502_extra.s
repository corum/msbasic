; memory map
; 0x0000 - 0x7EFF  RAM (31K)
; 0x7F00 - 0x7FFF  Devices (256 bytes) 16 devices, 16 bytes each, D1 = 0x7F00-0x7F0F, D2= 0x7F10-0x7F1F ...
; 0x8000 - 0xFFFF  ROM (32KB)
; Write to video memory by writing to ROM address space
; Write to text video memory by writing to ROM address space in 8000-BFFF , top nibble is row, bottom nibble is column - for row only 6 bits are used
; Text buffer is shadowed in RAM to enable scroling.  Shadow buffer is calculated by subtracting $25 from the high byte
; Text buffer only has $24 lines, so translates to the range of $5B00-$7F00 
 

.segment "CODE"

;ACIA D0
A_RXD   = $7F00
A_TXD   = $7F00
A_STS   = $7F01
A_RES   = $7F01
A_CMD   = $7F02
A_CTL   = $7F03

; devices
;VIA D1
PORTB   = $7F20
PORTA   = $7F2F     ; PORTA is register 1, this is PORTA with no handshake
DDRB    = $7F22
DDRA    = $7F23
SHCTL   = $7F2A
ACR     = $7F2B     ; auxiliary control register
PCR     = $7F2C     ; peripheral control register
IFR     = $7F2D 
IER     = $7F2E     ; interrupt enable register

; devices
;VIA2 D2
PORTB2   = $7F10
PORTA2   = $7F1F     ; PORTA is register 1, this is PORTA with no handshake
DDRB2    = $7F12
DDRA2    = $7F13
SHCTL2   = $7F1A
ACR2     = $7F1B     ; auxiliary control register
PCR2     = $7F1C     ; peripheral control register
IFR2     = $7F1D 
IER2     = $7F1E     ; interrupt enable register


;VIA config flags 
ICLR   = %01111111  ; clear all VIA interrupts
IMASK  = %10000011 ; enable interrupt for CA1
CFGCA  = %00000010  ; configure CA2 for negative active edge for PS/2 clock
ACRCFG = %00000011  ; enable latching


;LCD bits
E      = %10000000
RW     = %01000000
RS     = %00100000

;DISPLAY RAM LOCATIONS
TEXT      = $80
STEXT     = $82

COLVAL    = $80
ROWVAL    = $81

SCOLVAL   = $82
SROWVAL   = $83

GRAPHMODE = $84

; CONSTANT
ROWCOUNT  = $26
MAXROW    = $A4
MAXROWM1  = $A3
MAXCOL    = $65

; PS/2 keyboard memory locations
KBSTATE   = $A0
KBTEMP    = $A1
KBCURR    = $A2 
KBBIT     = $A3
KBEXTEND  = $A4
KBKEYUP   = $A5
KBDBG     = $A6
KBDBG2    = $A7
KEYTEMP   = $A8
KEYLAST   = $A9

KBBUF     = $7D00
KEYSTATE  = $7E00

ORIGIN         = $B0 
ORIGIN_H       = $B1
DRAW_WIDTH     = $B2
DRAW_HEIGHT    = $B3
DRAW_COLOR     = $B4
TEMP           = $B5
X1             = $B6
X2             = $B7
Y1             = $B8
Y2             = $B9

;INPUTBUF = $300

; keyboard processing states
PS2_START   = $00
PS2_KEYS    = $01
PS2_PARITY  = $02
PS2_STOP    = $03

;CODE
init:
    sei
    cld

    ldx #STACK_TOP
    txs

 ;   stz INPUTBUF
    stz INPUTBUFFER
    stz GRAPHMODE

; initialize the ACIA
    sta A_RES      ; soft reset (value not important)

                   ; set specific modes and functions
                   ; no parity, no echo, no Tx interrupt, Rx interrupt, enable Tx/Rx
    lda #%00001001
    sta A_CMD      ; store to the command register

    ;lda #$00      ; 1 stop bits, 8 bit word length, external clock, 16x baud rate
    lda #$1F       ; 1 stop bits, 8 bit word length, internal clock, 19.2k baud rate
    sta A_CTL      ; program the ctl register

; initialize the LCD via the VIA
 ;   lda #%11111111 ; Set all pins on port B for output
 ;   sta DDRB

 ;   lda #%00000000 ; Set 3 pins on port A for output
 ;   sta DDRA

;    lda #%00111000 ; set 8-bit mode, 2-line display, 5x8 font
;    jsr lcd_instruction
;    lda #%00001110 ; display on cursor on blink off
;    jsr lcd_instruction
;    lda #%00000110 ; increment and shift cursor; don't shift entire display
;    jsr lcd_instruction
;    lda #%00000001 ; clear the display
;    jsr lcd_instruction

; init display variables, column and row, shadow column and shadow row
    lda #$00
    sta COLVAL
    sta SCOLVAL

    lda #$80
    sta ROWVAL
    sbc #ROWCOUNT        ; subtract $26 to get shadow row
    sta SROWVAL


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

    ldx #$00           ; clear the key state and input buffers
@clrbufx:
    sta KEYSTATE, x
    sta KBBUF, x
    inx
    cpx #$00
    bne @clrbufx



  ;  jsr print_message

    lda #%00000000 ; configure all VIA1 A pins for input
    sta DDRA
    ;sta DDRA2

    lda #CFGCA
    sta PCR        ; configure CA2 for negative edge independent interrupt
    ;sta PCR2

    ;lda #ACRCFG
    ;sta ACR        ; enable latching

    lda #$83
    sta IER        ; enable interrupts for CA1 and CA2
    
    ;lda #$80
    ;sta IER2

    jsr cls
    
    cli

    lda #$9B
@loop:
    jsr WOZMON
    jmp @loop

message: .asciiz "Badger6502"


; Display startup message
ShowStartMsg:
    jsr tx_startup_message


; Wait for a cold/warm start selection
WaitForKeypress:
	jsr	MONRDKEY
	and	#$DF			; Make upper case
	cmp	#'W'			; compare with [W]arm start
	beq	WarmStart

	cmp	#'C'			; compare with [C]old start
	bne	ShowStartMsg

	jmp	COLD_START	; BASIC cold start

WarmStart:
	jmp	RESTART		; BASIC warm start

StartupMessage:
	.byte	$0C,"Cold [C] or warm [W] start?",$0D,$0A,$00

Backspace:
  .byte $1B,"[D ",$1B,"[D",$00

ms_basic:
  .asciiz "Microsoft BASIC"

;LOAD:
;	RTS

;SAVE:
;	RTS

tx_startup_message:
    ldx #0
@loop:
    lda StartupMessage, x
    beq @return
    inx
    jsr MONCOUT
    jmp @loop
@return:
    rts

tx_message:
    ldx #0
@loop:
    lda message, x
    beq @exit
    inx
    jsr MONCOUT
    jmp @loop
@exit:
    rts

wdc_pause:
    phx
    ldx #0

@wdc_pause_loop:
    inx
    cpx #$80
    bne @wdc_pause_loop

    plx
    rts

tx_char_sync:
    pha
@wait:
    lda A_STS              ; get status byte
    and #$10               ; mask transmit buffer status flag
    beq @wait              ; loop if tx buffer full

    pla
    sta A_TXD

    ; workaround for WDC chip
    jsr wdc_pause

    rts

rx_char_sync:
    lda A_STS              ; get status byte
    and #$08               ; max rx buffer status flag
    beq rx_char_sync       ; loop if rx buffer is empty   
    lda A_RXD              ; get byte from ACIA
    rts

rx_char_sync_nowait:
    lda A_STS              ; get status byte
    and #$08               ; max rx buffer status flag
    beq @nochar            ; exit with a null if the buffer is empty  
    lda A_RXD              ; get byte from ACIA
    bra @exit
@nochar:
    lda #$00
    bra @exit

@exit:
    rts

;rx_line_sync:
;    pha
;    phx
;    phy
;    ldy #$00               ;  init counter to 0
;@loop:
;    jsr rx_char_sync       ;  get a character, stored in A
;    jsr tx_char_sync       ;  echo
;    cmp	#$0D			         ;  compare with \r
;    beq @eol               ;  end of line
;    sta INPUTBUF, y     ;  store in the input buffer
;    iny                    ;  next character
;    cpy #$80               ;  80'th character? force end of line
;    beq @eol               ;  end of line;

;    jmp @loop              ;  repeat to read next character

;@eol:
;    lda #$00
;    sta INPUTBUF, y     ; null terminate
;    ply
;    plx
;    pla
;    rts

;tx_input_buffer:
;    pha
;    phx
;    phy
;    ldx #0
;@loop:
;    lda INPUTBUF, x
;    beq @exit
;    inx
;    jsr tx_char_sync
;    jmp @loop
;@exit:
;    pla
;    plx
;    ply
;    rts

lcd_wait:
    pha
    lda #%00000000  ; PortB is input
    sta DDRB

lcd_wait_loop:
    lda #RW
    sta PORTA

    lda #(RW|E)
    sta PORTA

    lda PORTB
    and #%10000000
    bne lcd_wait_loop

    lda #RW
    sta PORTA

    beq lcd_wait_loop

    lda #%11111111  ; PortB is output
    sta DDRB
    pla
    rts

lcd_instruction:
    jsr lcd_wait
    sta PORTB

    lda #0         ; clear RS/RW/E bits
    sta PORTA

    lda #E         ; flip enable bit on port a to send command via portb
    sta PORTA

    lda #0         ; clear RS/RW/E bits
    sta PORTA
    rts

print_message:
    ldx #0
@loop:
    lda message, x
    beq @return
    inx
    
    jsr print_char

    ;jsr tx_char
    jmp @loop
@return:
    rts

print_char:
    jsr lcd_wait
    sta PORTB
    pha
    lda #RS        ; register select bit on
    sta PORTA
    lda #(RS | E)  ; toggle E bit while leaving RS bit on
    sta PORTA
    lda #RS        ; clear enable bit
    sta PORTA
    pla
    rts

tx_backspace:
    pha
    phx
    ldx #$FF
@loop:
    inx
    lda Backspace,X
    beq @exit
    jsr MONCOUT
    bne @loop
@exit:
    plx
    pla
    rts

; KEYBOARD

read_char_async:
    lda KBCURR
    cmp #$00
    beq @exit
    jsr read_char
@exit:
    rts

read_char_echo:
    jsr read_char
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
display_char:
    ;jsr output_char
    jsr tx_char_sync
    rts

new_line:
    pha
    lda GRAPHMODE
    cmp #$00
    bne @exit

    lda #$00
    sta COLVAL    ; column goes to 0
    inc ROWVAL
    lda ROWVAL
    cmp #MAXROW      ; if row 23
    bne @exit
    jsr scroll    ; clear screen
@exit:
    pla
    rts

inc_col:
    pha
    inc COLVAL    
    lda COLVAL
    cmp #MAXCOL
    bne @exit
    jsr new_line
@exit:
    pla
    rts

backspace:
    pha
    lda COLVAL
    cmp #$00
    beq @exit
    
    lda #$00
    jsr write_char
    dec COLVAL
@exit:
    pla
    rts

output_char:
    ;phx
    ;ldx COLVAL
    ;stx $A400
    ;ldx ROWVAL
    ;stx $A402
    ;plx

    cmp #$03
    beq wozlong

    cmp #$0D
    beq @crlf

    cmp #$0A
    beq @exit

    cmp #$1B
    beq @clear

    cmp #$7F
    beq @backspace
    cmp #$08
    beq @backspace

    bra @char_out

@crlf:
    jsr new_line
    bra @exit

@clear:
    lda #$00
    sta COLVAL
    lda #$C0
    sta ROWVAL
    jsr cls
    bra @exit

@backspace:
    jsr backspace
    bra @exit

@char_out:
    jsr inc_col
    jsr write_char

@exit:
    rts

wozlong:
    jmp $FF00
  
write_char:
    phy

    ldy GRAPHMODE
    cpy #$00
    bne @exit

    ldy #$00 
    sta (TEXT),y    

    pha
    lda ROWVAL
    sbc #ROWCOUNT
    sta SROWVAL
    
    lda COLVAL
    sta SCOLVAL
    pla

    sta (STEXT),y
@exit:
    ply
    rts

cls:
_cls:
    pha
    phy
    lda #$80
    ldy #$00

    sty $00
    sta $01       ; address containing address to write to
    
    lda #$00
    clc
@clsloop:
    sta ($00),y
    iny
    bne @clsloop

    inc $01
    bne @clsloop  
    ply
    pla
    rts

;draw a rectangle

draw_rect:
    pha
    phx
    phy

    lda X2
    clc
    sbc X1
    sta DRAW_WIDTH

    lda Y2
    clc
    sbc Y1
    sta DRAW_HEIGHT

    lda X1
    sta ORIGIN
    lda Y1
    ora #$80
    sta ORIGIN_H

    lda DRAW_COLOR

    ldx #$00
@dr_looprow:
    ldy #$00
@dr_loopcol:
	sta (ORIGIN),Y
    iny
	cpy DRAW_WIDTH
	bne @dr_loopcol

    inc ORIGIN_H
    inx
	cpx DRAW_HEIGHT
	bne @dr_looprow

    ply
    plx
    pla
    rts

scroll:
    pha
    phy

    lda #$00
    sta COLVAL
    sta SCOLVAL

    lda #$80
    sta ROWVAL
    sbc #ROWCOUNT 
    sta SROWVAL

@looprow:
    ldy #$00
    inc ROWVAL
    inc SROWVAL

@loopcol:
    lda (STEXT),y
    dec ROWVAL
    dec SROWVAL

    sta (TEXT),y
    sta (STEXT),y

    inc ROWVAL
    inc SROWVAL

    iny
    cpy #MAXCOL
    bne @loopcol

    lda ROWVAL
    cmp #MAXROWM1
    bne @looprow

    lda #$A2
    sta ROWVAL    
    sbc #ROWCOUNT    
    sta SROWVAL

    ply
    pla
    rts



nmi:
    rti

irq:
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
    bne @ps2_keyboard_decode
    

    lda #$41
    jsr tx_char_sync

    jmp @exit


@irq_receive:
    ; we now have the byte, we need to add it to the keyboard buffer
    lda A_RXD
    ldx KBCURR
    sta KBBUF, x
    inc KBCURR
    jmp @exit


@ps2_keyboard_decode:
    lda PORTA
    ror
    ror       ; rotate into high order bit
    and #$80

    ldx KBSTATE
    cpx #PS2_START
    beq @start 
    
    cpx #PS2_KEYS
    beq @keys
  
    cpx #PS2_PARITY
    beq @parity

    cpx #PS2_STOP
    beq @stop
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
    jmp @exit          ; updated key up state, we're done here
 
@notkeyup:
    lda KBKEYUP        ; check the key up flag
    cmp #$00
    beq @setkeystate

@clearkeystate:        ; this is the key up path TODO: need to update key state to use ascii code instead of scan code
    ldx KBTEMP
    lda #$00
    sta KEYSTATE,x
    ; clear flags
    sta KBEXTEND
    sta KBKEYUP
    jmp @exit

@setkeystate:          ; set the key state - this is key down path
    ldx KBTEMP
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
    cpx #$58           ; caps lock
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
    sta KBBUF, x
    inc KBCURR
    jmp @exit    
    
@shifted:
    ldx KBTEMP
    lda ps2_ascii_shifted, x    
    ldx KBCURR
    sta KBBUF, x
    inc KBCURR
    jmp @exit

@control:
    ldx KBTEMP
    lda ps2_ascii_control, x
    ldx KBCURR
    sta KBBUF, x
    inc KBCURR
    jmp @exit

@nonprint:
@exit:

    lda #ICLR 
    sta IFR   ; clear all VIA interrupts

    ply
    plx
    pla

    rti

ps2_ascii:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "`", $00; 0
  .byte $00, $00, $00, $00, $00, "q", "1", $00, $00, $00, "z", "s", "a", "w", "2", $00; 1
  .byte $00, "c", "x", "d", "e", "4", "3", $00, $00, " ", "v", "f", "t", "r", "5", $00; 2
  .byte $00, "n", "b", "h", "g", "y", "6", $00, $00, $00, "m", "j", "u", "7", "8", $00; 3
  .byte $00, ",", "k", "i", "o", "0", "9", $00, $00, ".", "/", "l", ";", "p", "-", $00; 4 
  .byte $00, $00, "'", $00, "[", "=", $00, $00, $00, $00, $0D, "]", $00, "\", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  .byte $00, $00, $00, $00, $00, $00, $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7

ps2_ascii_shifted:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, "Q", "!", $00, $00, $00, "Z", "S", "A", "W", "@", $00; 1
  .byte $00, "C", "X", "D", "E", "$", "#", $00, $00, " ", "V", "F", "T", "R", "%", $00; 2
  .byte $00, "N", "B", "H", "G", "Y", "^", $00, $00, $00, "M", "J", "U", "&", "*", $00; 3
  .byte $00, "<", "K", "I", "O", ")", "(", $00, $00, ">", "?", "L", ":", "P", "_", $00; 4
  .byte $00, $00, $22, $00, "{", "+", $00, $00, $00, $00, $0D, "}", $00, "|", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  .byte $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7
 

ps2_ascii_control:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, $11, "!", $00, $00, $00, $1A, $13, $01, $17, "@", $00; 1
  .byte $00, $03, $18, $04, $05, "$", "#", $00, $00, " ", $16, $06, $14, $12, "%", $00; 2
  .byte $00, $0E, $02, $08, $07, $19, "^", $00, $00, $00, $0D, $0A, $15, "&", "*", $00; 3
  .byte $00, "<", $0B, $09, $0F, ")", "(", $00, $00, ">", "?", $0C, ":", $10, "_", $00; 4
  .byte $00, $00, $22, $00, "{", "+", $00, $00, $00, $00, $0D, "}", $00, "|", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  .byte $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7

 

; ****************************************************************************************************************
;  The WOZ Monitor for the Apple 1
;  Written by Steve Wozniak in 1976


; Page 0 Variables

XAML            = $24           ;  Last "opened" location Low
XAMH            = $25           ;  Last "opened" location High
STL             = $26           ;  Store address Low
STH             = $27           ;  Store address High
L               = $28           ;  Hex value parsing Low
H               = $29           ;  Hex value parsing High
YSAV            = $2A           ;  Used to see if hex value is given
MODE            = $2B           ;  $00=XAM, $7F=STOR, $AE=BLOCK XAM


; Other Variables

IN              = $300   ;  Input buffer to +$7F

.segment "WOZ"

WOZMON:         CLD             ; Clear decimal arithmetic mode.
                CLI
NOTCR:          CMP #$08 + $80  ; "\B"?
                BEQ BACKSPACE   ; Yes.
                CMP #$9B        ; ESC?
                BEQ ESCAPE      ; Yes.
                INY             ; Advance text index.
                BPL NEXTCHAR    ; Auto ESC if > 127.
ESCAPE:         LDA #'\'        ; "\".
                JSR ECHO        ; Output it.
GETLINE:        LDA #$8D        ; CR.
                JSR ECHO        ; Output it.
                LDA #$8A        ; LF.
                JSR ECHO        ; Output it.
                LDY #$01        ; Initialize text index.
BACKSPACE:      DEY             ; Back up text index.
                BMI GETLINE     ; Beyond start of line, reinitialize.
NEXTCHAR:       jsr read_char   ; Key ready?
                ora #$80
                STA IN,Y        ; Add to text buffer.
                JSR ECHO        ; Display character.
                CMP #$8D        ; CR?
                BNE NOTCR       ; No.
                LDY #$FF        ; Reset text index.
                LDA #$00        ; For XAM mode.
                TAX             ; 0->X.
SETSTOR:        ASL             ; Leaves $7B if setting STOR mode.
SETMODE:        STA MODE        ; $00=XAM $7B=STOR $AE=BLOK XAM
BLSKIP:         INY             ; Advance text index.
NEXTITEM:       LDA IN,Y        ; Get character.
                CMP #$8D        ; CR?
                BEQ GETLINE     ; Yes, done this line.
                CMP #'.' + $80  ; "."?
                BCC BLSKIP      ; Skip delimiter.
                BEQ SETMODE     ; Yes. Set STOR mode.
                CMP #':' + $80  ; ":"?
                BEQ SETSTOR     ; Yes. Set STOR mode.
                CMP #'R' + $80  ; "R"?
                BEQ WOZRUN      ; Yes. Run user program.
                STX L           ; $00-> L.
                STX H           ; and H.
                STY YSAV        ; Save Y for comparison.
NEXTHEX:        LDA IN,Y        ; Get character for hex test.
                EOR #$B0        ; Map digits to $0-9.
                CMP #$0A        ; Digit?
                BCC DIG         ; Yes.
                ADC #$88        ; Map letter "A"-"F" to $FA-FF.
                CMP #$FA        ; Hex letter?
                BCC NOTHEX      ; No, character not hex.
DIG:            ASL
                ASL             ; Hex digit to MSD of A.
                ASL
                ASL
                LDX #$04        ; Shift count.
HEXSHIFT:       ASL             ; Hex digit left, MSB to carry.
                ROL L           ; Rotate into LSD.
                ROL H           ;  Rotate into MSD’s.
                DEX             ; Done 4 shifts?
                BNE HEXSHIFT    ; No, loop.
                INY             ; Advance text index.
                BNE NEXTHEX     ; Always taken. Check next char for hex.
NOTHEX:         CPY YSAV        ; Check if L, H empty (no hex digits).
                BEQ ESCAPE      ; Yes, generate ESC sequence.
                BIT MODE        ; Test MODE byte.
                BVC NOTSTOR     ;  B6=0 STOR 1 for XAM & BLOCK XAM
                LDA L           ; LSD’s of hex data.
                STA (STL,X)     ; Store at current ‘store index’.
                INC STL         ; Increment store index.
                BNE NEXTITEM    ; Get next item. (no carry).
                INC STH         ; Add carry to ‘store index’ high order.
TONEXTITEM:     JMP NEXTITEM    ; Get next command item.
WOZRUN:         JMP (XAML)      ; Run at current XAM index.
NOTSTOR:        BMI XAMNEXT     ; B7=0 for XAM, 1 for BLOCK XAM.
                LDX #$02        ; Byte count.
SETADR:         LDA L-1,X       ; Copy hex data to
                STA STL-1,X     ; ‘store index’.
                STA XAML-1,X    ; And to ‘XAM index’.
                DEX             ; Next of 2 bytes.
                BNE SETADR      ; Loop unless X=0.
NXTPRNT:        BNE PRDATA      ; NE means no address to print.
                LDA #$8D        ; CR.
                JSR ECHO        ; Output it.
                LDA #$8A        ; LF
                JSR ECHO        ; Output it.
                LDA XAMH        ; ‘Examine index’ high-order byte.
                JSR PRBYTE      ; Output it in hex format.
                LDA XAML        ; Low-order ‘examine index’ byte.
                JSR PRBYTE      ; Output it in hex format.
                LDA #':' + $80  ; ":".
                JSR ECHO        ; Output it.
PRDATA:         LDA #$A0        ; Blank.
                JSR ECHO        ; Output it.
                LDA (XAML,X)    ; Get data byte at ‘examine index’.
                JSR PRBYTE      ; Output it in hex format.
XAMNEXT:        STX MODE        ; 0->MODE (XAM mode).
                LDA XAML
                CMP L           ; Compare ‘examine index’ to hex data.
                LDA XAMH
                SBC H
                BCS TONEXTITEM  ; Not less, so no more data to output.
                INC XAML
                BNE MOD8CHK     ; Increment ‘examine index’.
                INC XAMH
MOD8CHK:        LDA XAML        ; Check low-order ‘examine index’ byte
                AND #$0F        ; For MOD 16=0
                BPL NXTPRNT     ; Always taken.
PRBYTE:         PHA             ; Save A for LSD.
                LSR
                LSR
                LSR             ; MSD to LSD position.
                LSR
                JSR PRHEX       ; Output hex digit.
                PLA             ; Restore A.
PRHEX:          AND #$0F        ; Mask LSD for hex print.
                ORA #'0' + $80  ; Add "0".
                CMP #$BA        ; Digit?
                BCC ECHO        ; Yes, output it.
                ADC #$06        ; Add offset for letter.
ECHO:           pha
                and #$7F
                jsr display_char
                pla
                RTS             ; Return.

do_nothing:
                RTS

.segment "BOOTVECTORS"
    .word nmi
    .word init
    .word irq 
