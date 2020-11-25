; memory map
; 0x0000 - 0x7FFF  RAM (32KB)
; 0x8000 - 0x8FFF  Devices (4KB) 16 devices, 256 bytes each, D1 = 0x8000-0x80FF, D2= 0x8100-0x81FF ...
; 0x9000 - 0xFFFF  ROM (28KB)

.segment "CODE"

PORTB   = $8000
PORTA   = $8001
DDRB    = $8002
DDRA    = $8003
SHCTL   = $800A
ACR     = $800B     ; auxiliary control register
PCR     = $800C     ; peripheral control register
IFR     = $800D 
IER     = $800E     ; interrupt enable register

;VIA config flags 
ICLR   = %01111111  ; clear all VIA interrupts
IMASK  = %10000001  ; enable interrupt for CA1
CFGCA  = %00000010  ; configure CA1 for negative active edge for PS/2 clock
ACRCFG = %00000011  ; enable latching

;ACIA
A_RXD = $8100
A_TXD = $8100
A_STS = $8101
A_RES = $8101
A_CMD = $8102
A_CTL = $8103

;LCD bits
E     = %10000000
RW    = %01000000
RS    = %00100000

;DISPLAY RAM LOCATIONS
COORD  = $80
SHADOW = $90

COLVAL = $80
ROWVAL = $81

SHADOWCOL = $90
SHADOWROW = $91

; PS/2 keyboard state
KBSTATE   = $A0
KBTEMP    = $A1
KBCURR    = $A2
KBBIT     = $A3
KBEXTEND  = $A4
KBKEYUP   = $A5
KBDBG     = $A6
KBDBG2    = $A7

KBBUF    = $100
KEYSTATE = $200

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

    stz INPUTBUFFER

; init display variables, column and row, shadow column and shadow row
    lda #$00
    sta COLVAL
    sta SHADOWCOL

    lda #$C0
    sta ROWVAL
    and #$7F
    sta SHADOWROW


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

; initialize the ACIA
    sta A_RES      ; soft reset (value not important)
    lda #$0B       ; set specific modes and functions
                   ; no parity, no echo, no Tx interrupt, no Rx interrupt, enable Tx/Rx
    sta A_CMD      ; store to the command register

    ;lda #$00      ; 1 stop bits, 8 bit word length, external clock, 16x baud rate
    lda #$1F       ; 1 stop bits, 8 bit word length, internal clock, 19.2k baud rate
    sta A_CTL      ; program the ctl register

; initialize the LCD via the VIA
    lda #%11111111 ; Set all pins on port B for output
    sta DDRB

    lda #%11100000 ; Set 3 pins on port A for output
    sta DDRA

    lda #%00111000 ; set 8-bit mode, 2-line display, 5x8 font
    jsr lcd_instruction
    lda #%00001110 ; display on cursor on blink off
    jsr lcd_instruction
    lda #%00000110 ; increment and shift cursor; don't shift entire display
    jsr lcd_instruction
    lda #%00000001 ; clear the display
    jsr lcd_instruction

    jsr print_message

    jsr cls

; initialize VIA for keyboard processing
    lda #ICLR
    sta IFR

    lda #IMASK
    sta IER

    lda #CFGCA
    sta PCR        ; configure CB21for negative active edge and independent interrupt

    lda #ACRCFG
    sta ACR
    
    cli

    jmp WOZMON


loop:
    jmp loop       ; repeat

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

LOAD:
	RTS

SAVE:
	RTS

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

tx_char_sync:
    pha
@wait:
    lda A_STS              ; get status byte
    and #$10               ; mask transmit buffer status flag
    beq @wait              ; loop if tx buffer full

    pla
    sta A_TXD
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

rx_line_sync:
    pha
    phx
    phy
    ldy #$00               ;  init counter to 0
@loop:
    jsr rx_char_sync       ;  get a character, stored in A
    jsr tx_char_sync       ;  echo
    cmp	#$0D			         ;  compare with \r
    beq @eol               ;  end of line
    sta INPUTBUFFER, y     ;  store in the input buffer
    iny                    ;  next character
    cpy #$80               ;  80'th character? force end of line
    beq @eol               ;  end of line

    jmp @loop              ;  repeat to read next character

@eol:
    lda #$00
    sta INPUTBUFFER, y     ; null terminate
    ply
    plx
    pla
    rts

tx_input_buffer:
    pha
    phx
    phy
    ldx #0
@loop:
    lda INPUTBUFFER, x
    beq @exit
    inx
    jsr tx_char_sync
    jmp @loop
@exit:
    pla
    plx
    ply
    rts

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

; DISPLAY 
display_char:
    jsr output_char
    jsr tx_char_sync
    rts

new_line:
    pha
    lda #$00
    sta COLVAL    ; column goes to 0
    inc ROWVAL
    lda ROWVAL
    cmp #$E3      ; if row 23
    bne @exit
    jsr scroll    ; clear screen
@exit:
    pla
    rts

inc_col:
    pha
    inc COLVAL    
    lda COLVAL
    cmp #$61
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
    phx
    ldx COLVAL
    stx $E300
    ldx ROWVAL
    stx $E302
    plx

    cmp #$03
    beq wozmon

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

wozmon:
    jmp $FF00
  
write_char:
    phy

    ldy #$00 
    sta (COORD),y    

    pha
    lda ROWVAL
    and #$7F
    sta SHADOWROW
    
    lda COLVAL
    sta SHADOWCOL
    pla

    sta (SHADOW),y

    ply
    rts


_cls:
cls:
    pha
    phx
    phy

    ldy #$00
    ldx #$00
    stx COLVAL 
    stx SHADOWCOL
    lda #$BF
    sta ROWVAL
    and #$7F
    sta SHADOWROW

    lda #$00
   
@loopy:
    ldy #$00
    inc ROWVAL
    inc SHADOWROW

@loopx:
    sta (COORD),y
    sta (SHADOW),y
    iny
    cpy #$FF
    bne @loopx    

    ldy #$00
    ldx ROWVAL
    cpx #$E6
    bne @loopy
    
    ldx #$00
    stx COLVAL
    stx SHADOWCOL

    ldx #$C0
    stx ROWVAL
    and #$7F
    stx SHADOWROW

    ply
    plx
    pla

    rts

scroll:
    pha
    phy

    lda #$00
    sta COLVAL
    sta SHADOWCOL

    lda #$C0
    sta ROWVAL
    and #$7F
    sta SHADOWROW

@looprow:
    ldy #$00
    inc ROWVAL
    inc SHADOWROW

@loopcol:
    lda (SHADOW),y
    dec ROWVAL
    dec SHADOWROW

    sta (COORD),y
    sta (SHADOW),y

    inc ROWVAL
    inc SHADOWROW

    iny
    cpy #$64
    bne @loopcol

    lda ROWVAL
    cmp #$E3
    bne @looprow

    lda #$00
    sta COLVAL
    sta SHADOWCOL

    lda #$E2
    sta ROWVAL    
    and #$7F
    sta SHADOWROW

    ply
    pla
    rts



nmi:
    rti

;INTERRUPT
irq:
    pha
    phx
    phy


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
    and KBEXTEND
    sta KEYSTATE, x

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
    cmp #$00
    bne @shifted
    
    ldx #$59
    lda KEYSTATE,x
    cmp #$00
    bne @shifted

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
  .byte $00, $00, "'", $00, "[", "=", $00, $00, $00, $00, $13, "]", $00, "\", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  .byte $00, $00, $00, $00, $00, $00, $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7

ps2_ascii_shifted:
  ;      0   1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, "~", $00; 0
  .byte $00, $00, $00, $00, $00, "Q", "!", $00, $00, $00, "Z", "S", "A", "W", "@", $00; 1
  .byte $00, "C", "X", "D", "E", "$", "#", $00, $00, " ", "V", "F", "T", "R", "%", $00; 2
  .byte $00, "N", "B", "H", "G", "Y", "^", $00, $00, $00, "M", "J", "U", "&", "*", $00; 3
  .byte $00, "<", "K", "I", "O", ")", "(", $00, $00, ">", "?", "L", ":", "P", "_", $00; 4
  .byte $00, $00, $22, $00, "{", "+", $00, $00, $00, $00, $13, "}", $00, "|", $00, $00; 5
  .byte $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  .byte $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00; 7
  
;      0123456789ABCDEF
  ;.byte "??????????????`?" ; 0
  ;.byte "?????Q!???ZSAW@?" ; 1
  ;.byte "?CXDE$#?? VFTR%?" ; 2
  ;.byte "?NBHGY^???MJU&*?" ; 3
  ;.  .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00; 6
  ;.byte "??", $22, "?{+?????}?|??" ; 5


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
NEXTCHAR:       jsr rx_char_sync ; Key ready?
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

.segment "BOOTVECTORS"
    .word nmi
    .word init
    .word irq 
