;memory map
;0x0000	0x7FFF	RAM				32KB main RAM
;0x8000	0xBFFF	Banked RAM		16KB banked RAM
;0xC000	0xCFFF	Video RAM		4KB banked video memory
;0xD000	0xD0FF	Devices
;0xD100	0xF2FF	Basic ROM
;0xF300	0xFFFF	OS
 

.segment "CODE"

;ACIA D0
A_RXD   = $D000
A_TXD   = $D000
A_STS   = $D001
A_RES   = $D001
A_CMD   = $D002
A_CTL   = $D003

; devices
;VIA D1
PORTB   = $D020
PORTA   = $D02F     ; PORTA is register 1, this is PORTA with no handshake
DDRB    = $D022
DDRA    = $D023
SHCTL   = $D02A
ACR     = $D02B     ; auxiliary control register
PCR     = $D02C     ; peripheral control register
IFR     = $D02D 
IER     = $D02E     ; interrupt enable register

; devices
;VIA2 D2
PORTB2   = $D010
VIDBANK  = $D010
PORTA2   = $D01F     ; PORTA is register 1, this is PORTA with no handshake
DDRB2    = $D012
DDRA2    = $D013
SHCTL2   = $D01A
ACR2     = $D01B     ; auxiliary control register
PCR2     = $D01C     ; peripheral control register
IFR2     = $D01D 
IER2     = $D01E     ; interrupt enable register


;VIA config flags 
ICLR   = %01111111  ; clear all VIA interrupts
IMASK  = %10000011 ; enable interrupt for CA1
CFGCA  = %00000010  ; configure CA2 for negative active edge for PS/2 clock
ACRCFG = %00000011  ; enable latching


;LCD bits
E      = %10000000
RW     = %01000000
RS     = %00100000

;GRAPHICS

X1_            = $AD
X2_            = $AE
ORIGIN_H_LAST  = $AF
HIRESPAGE      = $B0
HIRESPAGE_H    = $B1

DRAW_WIDTH     = $B2
DRAW_WIDTH_H   = $B3
DRAW_HEIGHT    = $B4
DRAW_COLOR     = $B5
X1             = $B6
X1_H           = $B7
X2             = $B8
X2_H           = $B9
Y1             = $BA
Y2             = $BB
XD             = $BC   ; xdelta for line drawing
YD             = $BD   ; ydelta for line drawing
XT             = $BE   ; x temp
YT             = $BF   ; y temp
XADD           = $C0
YADD           = $C1
WHICH_PIXEL_X1 = $C2
WHICH_PIXEL_X2 = $C3
CHAR           = $2C2
CHAR_H         = $2C3
CURSOR         = $2C4
CURSOR_H       = $2C5
CURSORCOPY     = $2C6
CURSORCOPY_H   = $2C7
CURSORPOS_X    = $2C8
CURSORPOS_Y    = $2C9
DRAW_WIDTH_IDX = $2CE

VIDMEM         = $C4
VIDMEM_H       = $C5
SOURCE         = $C6
SOURCE_H       = $C7
ORIGIN         = $C8 
ORIGIN_H       = $C9


; PS/2 keyboard memory locations
KBSTATE   = $CA
KBTEMP    = $CB
KBCURR    = $CC 
KBBIT     = $CD
KBEXTEND  = $CE
KBKEYUP   = $CF
KBDBG     = $D0
KBDBG2    = $D1
KEYTEMP   = $D2
KEYLAST   = $D3

TEMP      = $D4

KBBUF     = $500
KEYSTATE  = $600


;INPUTBUF = $300

; keyboard processing states
PS2_START   = $00
PS2_KEYS    = $01
PS2_PARITY  = $02
PS2_STOP    = $03

.segment "OS"
;CODE
init:
    sei
    cld

    ldx #STACK_TOP
    txs

 ;   stz INPUTBUF
    stz INPUTBUFFER

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
    
    lda #%11111111 ; Set all pins on port B for output
    sta DDRB2

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
;    jsr tx_startup_message
     rts

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

;tx_startup_message:
;    ldx #0
;@loop:
;    lda StartupMessage, x
;    beq @return
;    inx
;    jsr MONCOUT
;    jmp @loop
;@return:
;    rts

;tx_message:
;    ldx #0
;@loop:
;    lda message, x
;    beq @exit
;    inx
;    jsr MONCOUT
;    jmp @loop
;@exit:
;    rts

wdc_pause:
    phx
    ldx #0

@wdc_pause_loop1:
    inx
    cpx #$00
    bne @wdc_pause_loop1

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


;lcd_wait:
;    pha
;    lda #%00000000  ; PortB is input
;    sta DDRB

;lcd_wait_loop:
;    lda #RW
;    sta PORTA

;    lda #(RW|E)
;    sta PORTA

;    lda PORTB
;    and #%10000000
;    bne lcd_wait_loop

;    lda #RW
;    sta PORTA

;    beq lcd_wait_loop

;    lda #%11111111  ; PortB is output
;    sta DDRB
;    pla
;    rts

;lcd_instruction:
;    jsr lcd_wait
;    sta PORTB

;    lda #0         ; clear RS/RW/E bits
;    sta PORTA

;    lda #E         ; flip enable bit on port a to send command via portb
;    sta PORTA

;    lda #0         ; clear RS/RW/E bits
;    sta PORTA
;    rts

;print_message:
;    ldx #0
;@loop:
;    lda message, x
;    beq @return
;    inx
    
;    jsr print_char

    ;jsr tx_char
;    jmp @loop
;@return:
;    rts

;print_char:
;    jsr lcd_wait
;    sta PORTB
;    pha
;    lda #RS        ; register select bit on
;    sta PORTA
;    lda #(RS | E)  ; toggle E bit while leaving RS bit on
;    sta PORTA
;    lda #RS        ; clear enable bit
;    sta PORTA
;    pla
;    rts

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

;==========================================================================
; Keyboard
;==========================================================================

read_char_async_apple:
    lda KBCURR
    cmp #$00
    beq @exit
    jsr read_char
    ora #$80
@exit:
    rts

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
    jsr tx_char_sync
    ;jsr char_to_screen
    rts


wozlong:
    jmp $FF00




;==========================================================================
; console routines
;==========================================================================

calc_cursor:  
    pha
    phx

	clc
    lda CURSOR
    ldx CURSORPOS_Y   

@loopmult:
    adc #$40
    bcs @inchigh
@resumeloop:
	dex
	bne @loopmult
    bra @addy

@inchigh:
    clc 
    inc CURSOR_H
    bra @resumeloop

@addy:
    clc
    adc CURSORPOS_X
    sta CURSOR
    bcc @exit
    inc CURSOR_H

@exit:
    plx
    pla
    rts

; based on CURSORPOS_X and CURSORPOS_Y, set drawing point for character
set_cursor_coords:
    pha
    phx

    lda #$00
    sta X1
    sta Y1

    ldx #$00
    lda #$00
@loopx:
    cpx CURSORPOS_X
    beq @setx
    inx
    clc
    adc #$04
    bra @loopx

@setx:
    sta X1
    ldx #$00
    lda #$00

@loopy:
    cpx CURSORPOS_Y
    beq @sety
    inx
    clc
    adc #$06
    bra @loopy

@sety:
    sta Y1

    plx
    pla
    rts

; Add a character to the screen memory at the cursor location and render on the screen
;char_to_screen:
;    pha
;    phx

;    ldx #$02
;    stx DRAW_COLOR

;    jsr calc_cursor
    

;    cmp #$0D  ; CR
;    beq @cr
;    cmp #$0A  ; linefeed
;    beq @afternewrow
;    cmp #$08  ; backspace
;    beq @backspace
;    cmp #$03  ; ESCAPE
;    beq @escape

    ; write to screen memory
    ; screen + (cursorpos_y * $40 + cursorpos_x)
;    sta (CURSOR)

 ;   jsr set_cursor_coords  
;    jsr draw_char

    
 ;   adc CURSORPOS_X

 ;   inc CURSORPOS_X
 ;   lda CURSORPOS_X
 ;   cmp #$40
 ;   bne @afternewrow

;@cr:
;    stz CURSORPOS_X
;    inc CURSORPOS_Y
;    lda CURSORPOS_Y
;    cmp #$14
;    bne @afternewrow
    ; we need to scroll now
;    jsr scroll_screen
;    jsr draw_screen
;    bra @afternewrow

;@backspace:
;    dec CURSORPOS_X
;    cmp #$FF
;    bne @doback
;    stz CURSORPOS_X
;    bra @afternewrow

;@doback:
;   lda #$00
;   dec CURSOR
;   sta (CURSOR)
;   bra @afternewrow

;@escape:
;    jsr cls

;@afternewrow:
;    plx
;    pla
;    rts

;scroll_screen:
;    pha
;    phx
;    phy 

    ; set cursor to beginning of screen memory
;    lda #<SCREEN
;    sta CURSOR
  
;    clc
;    adc #$40
;    sta CURSORCOPY

;    lda #>SCREEN
;    sta CURSOR_H
;    sta CURSORCOPY_H
 
    ; copy 
;    lda #$00
;    ldx #$00
;@loop:
;    lda (CURSORCOPY),Y
;    sta (CURSOR),Y
;    iny
;    bne @loop

;    inc CURSOR_H
;    inc CURSORCOPY_H

;    inx
;    cpx #$04
;    bne @loop

;    stz CURSORPOS_X
;    lda #$13
;    sta CURSORPOS_Y

;    lda #<SCREEN
;    sta CURSOR
;    lda #>SCREEN
;    sta CURSOR_H

;    ply
;    plx
;    pla
;    rts

;==========================================================================
; drawing routines
;==========================================================================

cls:
_cls:
     
    pha

    lda #$00
    sta DRAW_COLOR
    
    sta X1
    sta X1_H
    sta Y1

    ; $140 = 320
    lda #$1
    sta X2_H

    lda #$40
    sta X2
    
    
    lda #$F0  ; 240
    sta Y2
    
    jsr draw_rect

    pla
    rts

set_vid_bank:
    pha

; set video bank
    lda ORIGIN_H   
    and #$F0      ; mask off bottom 4 bits
    sta VIDBANK

    lda ORIGIN_H
    and #$0F      ; mask off top 4 bits
    ora #$C0      ; video memory is banked in from $C000 - $CFFF
    sta VIDMEM_H

    lda ORIGIN
    sta VIDMEM
; end of setting video bank
    pla
    rts

draw_pixel:
    pha
    phy

; set video bank
    lda ORIGIN_H   
    and #$F0      ; mask off bottom 4 bits
    sta VIDBANK

    lda ORIGIN_H
    and #$0F      ; mask off top 4 bits
    ora #$C0      ; video memory is banked in from $C000 - $CFFF
    sta VIDMEM_H

    lda ORIGIN
    sta VIDMEM
; end of setting video bank

    lda (VIDMEM)
    tay

    lda WHICH_PIXEL_X1
    lsr
    bcc @left

@right:
    lda DRAW_COLOR
    clc
    asl
    asl
    asl
    asl
    sta TEMP
    tya
    and #$0F    
    bra @write

@left:
    lda DRAW_COLOR
    and #$0F
    sta TEMP
    tya
    and #$F0

@write:
    ora TEMP
    sta (VIDMEM)

    ply
    pla
    rts

;draw a rectangle
draw_rect:
    pha
    phx
    phy

    lda X2
    sec
    sbc X1
    sta DRAW_WIDTH

    lda X2_H
    sec
    sbc X1_H
    sta DRAW_WIDTH_H
    lsr                  ; if > 255, shift bit 0 into carry
    
    lda DRAW_WIDTH
    ror                  ; divide by 2 and rotate carry into bit 7
                         
    sta DRAW_WIDTH       ; DRAW_WIDTH is now indexed width

    lda Y2
    sec
    sbc Y1
    sta DRAW_HEIGHT


    lda X1_H
    lsr                 ; if X > 255, shift bit 0 into carry

    lda X1
    and #$01        
    sta WHICH_PIXEL_X1  ; which pixel, 0 = left, 1 = right
   
    lda X1
    ror                 ; divide by 2  and rotate carried bit into bit 7

    sta ORIGIN          ; ORIGIN contains the indexed X position

    lda Y1
    sta ORIGIN_H
    jsr set_vid_bank

    lda DRAW_COLOR
    
    ldx #$00
@dr_looprow:
    ldy #$00
@dr_loopcol:

    ; pixels are 4bpp, so we should read the pixel we're planning to write
    ; mask out the pixel we're going to change, or it back in, and then store both pixels
    ; for now, ignore which pixel and just fill them both,  will only draw rects on 2 pixel boundary
    ; but it'll be faster

    sta (VIDMEM),Y
    iny
	cpy DRAW_WIDTH
	bne @dr_loopcol

    inc ORIGIN_H   
    jsr set_vid_bank

    inx
	cpx DRAW_HEIGHT
	bne @dr_looprow

    ply
    plx
    pla
    rts

    
draw_line:
            ; this algorithm loads XT and YT at X1, Y1 points
            ; ORIGIN and ORIGIN_H also are initialized to X1, Y1 and act as a cursor
            ; XD and YD are the deltas (x2-x1) and (y2-y1)
            ; XT and YT are incremented by XD and YD in a loop
            ; when XT or YT overflow, ORIGIN and ORIGIN_H are incremented respectively
            ; on every change of the cursor a point is plotted
            ; drawing terminates when ORIGIN and ORIGIN_H match the end point
    pha
    phx
    phy

@start:

    ; start drawing from x1,y1

    lda X1_H
    lsr                 ; if X > 255, shift bit 0 into carry

    lda X1
    tay
    and #$01        
    sta WHICH_PIXEL_X1  ; which pixel, 0 = left, 1 = right
    tya
    ror                 ; divide by 2  and rotate carried bit into bit 7
    sta ORIGIN          ; ORIGIN contains the indexed X position (X/2)
    sta X1_

    lda X2_H
    lsr
    lda X2
    ror
    sta X2_

    lda Y1
    sta ORIGIN_H

    
    jsr draw_pixel

    ; default increment to +1
    lda #$01 
    sta XADD
    sta YADD

    ; calc xdelta
    
    sec
    lda X2_
    sbc X1_
    sta XD  ; xdelta
    sta XT

    ; calc ydelta
    sec
    lda Y2
    sbc Y1
    sta YD  ; ydelta
    sta YT


    ; if x2 < x1, xadd = -1
    lda X1_
    cmp X2_
    bcc @skipxadd
    lda #$FF  
    sta XADD  ; increment by -1
    lda #$00  
    sec
    sbc XD   
    sta XD    ; 0 - XD, reverse the x delta
    
@skipxadd:
    ; if y2 < y1, yadd = -1
    lda Y1
    cmp Y2
    bcc @skipyadd
    lda #$FF
    sta YADD   ; increment by - 1
    lda #$00
    sec
    sbc YD    
    sta YD     ; 0 - YD, reverse the y delta

@skipyadd:
    
    ; divide YD and YT by 2 due to packing of X pixel
    lda YD
    lsr
    sta YD
    sta YT
     
    ; start draw loop

@xcheck:
    clc    
    lda XT
    adc XD 
    sta XT
    bcs @incX

@ycheck:  
    clc     
    lda YT
    adc YD
    sta YT
    bcs @incY

@testend:
    lda YD
    bne @xcheck

    lda XD
    bne @xcheck  

    bra @endloop


@incX:
    lda X2_
    cmp ORIGIN
    beq @testend

    inc WHICH_PIXEL_X1
    lda WHICH_PIXEL_X1
    lsr
    bcs @pixelskip

    lda ORIGIN
    clc
    adc XADD
    sta ORIGIN
@pixelskip:
    ; plot point (x3,y3)
    jsr draw_pixel

    cmp X2_
    bne @ycheck

    lda #$00
    sta XD
    bra @ycheck

@incY:

    lda ORIGIN_H
    clc
    adc YADD
    sta ORIGIN_H

    ; plot point (x3, y3)
    jsr draw_pixel

    cmp Y2
    bne @xcheck

    lda #$00
    sta YD
    bra @xcheck

@endloop:

    ply
    plx
    pla
    rts

; helper function to decode apple II hires video data and copy to video memory
; to aid in porting

apple_draw_1:
    pha
    phx
    phy
    
    lda #$00
    sta Y1

    ldy #$00
@loopy:
    ldx #$00
    stx X1
    stx X1_H

    lda hires_lsb, y
    sta SOURCE

    lda (HIRESPAGE), y
    sta SOURCE_H

@loopx:

    lda (SOURCE)

    phx
    phy

    ldx #$7
@nextpixel:   
    ror
    bcs @white
@black:
    ; it's black
    ldy #$80
    sty DRAW_COLOR
    bra @draw
@white:
    ldy #$FF
    sty DRAW_COLOR
@draw:
    inc X1
    bne @skipcarry
    inc X1_H

@skipcarry:    
    jsr draw_pixel_2

    dex
    bne @nextpixel

    ply
    plx
  
@next:
    inc SOURCE
    inx
    cpx #$28
    bne @loopx

    iny
    sty Y1
    sty ORIGIN_H

    cpy #$C0
    bne @loopy

    ply
    plx
    pla
    rts

;
;  draw_pixel_2 
;

draw_pixel_2:
    pha
    phy

    lda X1_H
    lsr                 ; if X > 255, shift bit 0 into carry
    lda X1
    tay
    and #$01        
    sta WHICH_PIXEL_X1  ; which pixel, 0 = left, 1 = right
    tya
    ror                 ; divide by 2  and rotate carried bit into bit 7
    sta ORIGIN          ; ORIGIN contains the indexed X position (X/2)

    jsr draw_pixel

    ply
    pla
    rts

set_hires_page1:
    pha
    lda #<hires1_msb
    sta HIRESPAGE
    lda #>hires1_msb
    sta HIRESPAGE_H
    pla
    rts

set_hires_page2:
    pha
    lda #<hires2_msb
    sta HIRESPAGE
    lda #>hires2_msb
    sta HIRESPAGE_H
    pla
    rts

;draw a character
;draw_char:
;    pha
;    phx
;    phy

;    tay
;    iny

;    ldx #<font
;    stx CHAR
;    ldx #>font 
;    stx CHAR_H

;    clc

;   get char data
;    lda CHAR
;@getchar:
;    dey
;    beq @havechar
;    adc #$06
;    bcc @skiphighinc
;    inc CHAR_H
;    sta CHAR
;    lda CHAR
;    clc

;@skiphighinc:
;    bra @getchar

;@havechar:
;    adc #$06
;    sta CHAR

;    lda X1
;    sta ORIGIN
;    lda Y1
;    clc
;    ora #$80
;    sta ORIGIN_H

;    ldx #$00
;@looprow:
;    ldy #$00
;    lda (CHAR)
;@loopcol:

;    pha
;    lda #$00
;	sta (ORIGIN),Y
;    pla

;    asl
;    bcc @afterpixel

;    pha
;    lda DRAW_COLOR
;	sta (ORIGIN),Y
;    pla

;@afterpixel:
;    iny
;	cpy #$04
;	bne @loopcol

;    inc ORIGIN_H
;    inx
;    dec CHAR
;	cpx #$06
;	bne @looprow

;@exit:
;    ply
;    plx
;    pla
;    rts


; screen is 32x21
; from $400 to $C00
;draw_screen:
;    pha
;    phx
;    phy

    
;    ldx #<SCREEN
;    stx CURSOR
;    ldx #>SCREEN
;    stx CURSOR_H

;    stx X1
;    stx Y1
    
;    ldy #$00
;    ldx #$00

;@loop:
    
;    lda (CURSOR)
;    jsr draw_char

;    lda X1
;    clc
;    adc #$04
;    sta X1

;    inc CURSOR
;    bne @skiphigh
;    inc CURSOR_H
;@skiphigh:

;    inx
;    cpx #$40
;    bne @loop

;    ldx #$00
;    stx X1
;    lda Y1
;    clc
;    adc #$06
;    sta Y1
;    iny
;    cpy #$14
;    bne @loop

;    ply
;    plx
;    pla
;    rts


; ============================================================================================
; graphics tests
; ============================================================================================

linetests:
    jsr linetest1
    jsr cls
    jsr linetest2
    jsr cls
    jsr linetest3
    rts

linetest1:
    stz X1
    stz X1_H

    stz X2_H
    stz Y1

    ldy #$F0
    ldx #$01   
    sty Y2
@effectx:
    inc DRAW_COLOR
    stx X2
    jsr draw_line
    inx
    bne @effectx

    inc X2_H
@effectx2:
    inc DRAW_COLOR
    stx X2
    jsr draw_line
    inx
    cpx #$40
    bne @effectx2
@donex:
    dey
    dex
    stx X2

@effecty:
    inc DRAW_COLOR
    sty Y2
    jsr draw_line
    dey
    bne @effecty
    rts


linetest2:
    pha
    phx
    phy


    ; 0,0 - 255,128
    lda #$02
    sta DRAW_COLOR

    stz X1
    stz X1_H
    stz Y1        ; Origin is (0,0)
    
    lda #$01
    sta X2_H

    lda #$40
    sta X2    

    lda #$F0
    sta Y2        ; Destination (320x240)

    jsr draw_line

    ; 0,128 - 255,0
    lda #$04
    sta DRAW_COLOR

    stz X1_H
    stz X1       ; (0, 240)
    lda #$F0
    sta Y1

    lda #$01
    sta X2_H
    lda #$40
    sta X2        ; (320, 0)

    stz Y2
    jsr draw_line


    ; draw backwards

    ; 320,64 - 0,32 

    lda #$01
    sta X2_H

    lda #$40
    sta X2    

    lda #$40
    sta Y2        ; Origin (320x64)

    lda #$02
    sta DRAW_COLOR

    stz X1
    stz X1_H

    lda #$20
    sta Y1        ; Dest is (0,32)
    

    jsr draw_line

    ply
    plx
    pla
    rts

linetest3:
    pha
    phx
    phy

    lda #$03
    sta DRAW_COLOR

    stz X1
    stz X1_H
    stz X2
    stz X2_H
    stz Y2
    lda #$EF
    sta Y1

@loop:    

    inc DRAW_COLOR
    jsr draw_line

@xlow:    
    clc
    lda X2
    adc #$08
    sta X2
    bcs @xhigh
    
    cmp #$40
    beq @xcheck
    bra @y

@xhigh:
    inc X2_H

@y:
    sec
    lda Y1
    sbc #$06
    bcc @exit
    sta Y1    
    bra @loop

@xcheck:
    lda X2_H
    lsr
    bcc @loop

@exit:

    ply
    plx
    pla
    rts


; ============================================================================================
; interrupts
; ============================================================================================

nmi:
    jsr apple_draw_1

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

; ============================================================================================
; data
; ============================================================================================

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

 ; 4x6 font from
; https://github.com/idispatch/raster-fonts/blob/master/font-4x6.c
; removed

; Apple 2 high res lookup tables
hires1_msb:
        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
hires_lsb:
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $80, $80, $80, $80, $80, $80, $80, $80
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $80, $80, $80, $80, $80, $80, $80, $80
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $80, $80, $80, $80, $80, $80, $80, $80
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $80, $80, $80, $80, $80, $80, $80, $80
        .byte $28, $28, $28, $28, $28, $28, $28, $28
        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
        .byte $28, $28, $28, $28, $28, $28, $28, $28
        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
        .byte $28, $28, $28, $28, $28, $28, $28, $28
        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
        .byte $28, $28, $28, $28, $28, $28, $28, $28
        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
        .byte $50, $50, $50, $50, $50, $50, $50, $50
        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
        .byte $50, $50, $50, $50, $50, $50, $50, $50
        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
        .byte $50, $50, $50, $50, $50, $50, $50, $50
        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
        .byte $50, $50, $50, $50, $50, $50, $50, $50
        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
hires2_msb:
        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f


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
