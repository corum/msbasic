;memory map

;0x0000 0xCFFF RAM 48KB
;0xC000 0xC0FF Devices 
;0xC100 0xE2FF Basic ROM
;0xE300 0xFFFF OS

 

.segment "CODE"

;ACIA C0
A_RXD   = $C000
A_TXD   = $C000
A_STS   = $C001
A_RES   = $C001
A_CMD   = $C002
A_CTL   = $C003

; devices
;VIA D1
PORTB   = $C010
PORTA   = $C01F     ; PORTA is register 1, this is PORTA with no handshake
DDRB    = $C012
DDRA    = $C013
SHCTL   = $C01A
ACR     = $C01B     ; auxiliary control register
PCR     = $C01C     ; peripheral control register
IFR     = $C01D 
IER     = $C01E     ; interrupt enable register


;VIA config flags 
ICLR   = %01111111  ; clear all VIA interrupts
IMASK  = %10000011 ; enable interrupt for CA1
CFGCA  = %00000010  ; configure CA2 for negative active edge for PS/2 clock
ACRCFG = %00000011  ; enable latching


;CONSOLE
CURSOR_X       = $A5
CURSOR_Y       = $A6

;GRAPHICS
YADD           = $A7
XADD           = $A8
X1_            = $A9
Y1_            = $AA
X2_            = $AB
ORIGIN_L       = $AC
ORIGIN_H       = $AD

SCREEN_L       = $AE
SCREEN_H       = $AF

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
FONTPTR        = $C0
FONTPTR_H      = $C1

; TEXT MODE
TEXT           = $0400
CURSOR_ADDR    = $C2
CURSOR_ADDR_H  = $C3
CHAR_DRAW      = $C4

;ROMDISK
RD_LOW  = $C030
RD_HIGH = $C031
RD_BANK = $C032
RD_DATA = $C033

;ROMDISK VARIABLES

SOURCE_LOW  = $BC
SOURCE_HIGH = $BD

RD_BYTES_LOW  = $BE
RD_BYTES_HIGH = $BF

DEST_LOW   = $C5
DEST_HIGH  = $C6




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

KBBUF     = $200
KEYSTATE  = $300


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
    
    sta $C03E ; set graphics mode 1

    ldx #STACK_TOP
    txs

 ;   stz INPUTBUF
    stz INPUTBUFFER
    stz CURSOR_X
    stz CURSOR_Y

; initialize the ACIA
    sta A_RES      ; soft reset (value not important)

                   ; set specific modes and functions
                   ; no parity, no echo, no Tx interrupt, Rx interrupt, enable Tx/Rx
    lda #%00001001
    sta A_CMD      ; store to the command register

    ;lda #$00      ; 1 stop bits, 8 bit word length, external clock, 16x baud rate
    lda #$1F       ; 1 stop bits, 8 bit word length, internal clock, 19.2k baud rate
    sta A_CTL      ; program the ctl register



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


    lda #%00000000 ; configure all VIA1 A pins for input
    sta DDRA

    lda #CFGCA
    sta PCR        ; configure CA2 for negative edge independent interrupt, for PS/2

    ;lda #ACRCFG
    ;sta ACR        ; enable latching

    lda #$83
    sta IER        ; enable interrupts for CA1 and CA2
    
    ; set graphics page
    lda #$20
    sta SCREEN_H
    lda #$00
    sta SCREEN_L
    jsr set_hires_page1

    ;jsr cls
    
    cli

    lda #$9B
@loop:
    jsr WOZMON
    jmp @loop


; Display startup message
ShowStartMsg:
;    jsr tx_startup_message
     rts


Backspace:
  .byte $1B,"[D ",$1B,"[D",$00

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

    ;jsr console_add_char
    ; workaround for WDC chip
    jsr wdc_pause

    rts

;rx_char_sync:
;    lda A_STS              ; get status byte
;    and #$08               ; max rx buffer status flag
;    beq rx_char_sync       ; loop if rx buffer is empty   
;    lda A_RXD              ; get byte from ACIA
;    rts

;rx_char_sync_nowait:
;    lda A_STS              ; get status byte
;    and #$08               ; max rx buffer status flag
;    beq @nochar            ; exit with a null if the buffer is empty  
;    lda A_RXD              ; get byte from ACIA
;    bra @exit
;@nochar:
;    lda #$00
;    bra @exit

;@exit:
;    rts


;tx_backspace:
;    pha
;    phx
;    ldx #$FF
;@loop:
;    inx
;    lda Backspace,X
;    beq @exit
;    jsr MONCOUT
;    bne @loop
;@exit:
;    plx
;    pla
;    rts

;==========================================================================
; Keyboard
;==========================================================================

;read_char_async_apple:
;    lda KBCURR
;    cmp #$00
;    beq @exit
;    jsr read_char
;    ora #$80
;@exit:
;    rts

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
    jsr console_add_char
    ;jsr char_to_screen
@done: 
    rts


wozlong:
    jmp $FF00

;==========================================================================
; drawing routines
;==========================================================================

cls:
_cls:
    pha

    lda #$00
    sta DRAW_COLOR
    sta CURSOR_X
    sta CURSOR_Y

    jsr fillscreen
    jsr clear_text_region

    pla
    rts

fillscreen:    
    pha

    lda #$00
    sta X1
    sta Y1

    lda #$28
    sta X2    
    
    lda #$C8  ; 200
    sta Y2
    
    jsr draw_char_rect

    pla
    rts

;draw a rectangle
draw_char_rect:
    pha
    phx
    phy

; calculate the starting address in video memory
    ldy Y1
    lda eb_hires_lsb, y
    sta ORIGIN_L

    lda (HIRESPAGE),Y
    sta ORIGIN_H

    lda X1
    clc
    adc ORIGIN_L
    bcc @nocarry
    inc ORIGIN_H
@nocarry:
    sta ORIGIN_L

    lda X2
    sec
    sbc X1
    sta DRAW_WIDTH
    
    lda Y2
    sec
    sbc Y1
    sta DRAW_HEIGHT

    lda DRAW_COLOR
    
    ldx #$00
@dr_looprow:
    ldy #$00
@dr_loopcol:
    sta (ORIGIN_L),Y
    iny
	cpy DRAW_WIDTH
	bne @dr_loopcol

    ; add 40 to ORIGIN_L
    ; if overflow, increment origin_H
    lda #$28 ; 40
    clc
    adc ORIGIN_L
    bcc @after_carry
    inc ORIGIN_H
@after_carry:
    sta ORIGIN_L
    lda DRAW_COLOR
    inx
	cpx DRAW_HEIGHT
	bne @dr_looprow

    ply
    plx
    pla
    rts


draw_pixel:
    pha
    phx
    phy

    ldy Y1
    lda eb_hires_lsb, y
    sta ORIGIN_L

    lda (HIRESPAGE),Y
    sta ORIGIN_H

    clc
    lda X1_H
    lsr        ; if > 255 shift into carry
    lda X1
; divide by 8, but ror first bring in the carry flag
    ror
    lsr
    lsr 
; A now contains the X character betweeen 0-40

    clc
    adc ORIGIN_L
    bcc @after_carry
    inc ORIGIN_H
@after_carry:
    sta ORIGIN_L
; now at the byte of video memory that contains the pixel we want to flip

    lda X1
    and #$7   ; last 3 bits indicate which bit we want to display
    tax
    inx

    lda #0
    sec

@shiftloop:   
    ror       
    dex
    bne @shiftloop

    ora (ORIGIN_L)
    sta (ORIGIN_L)

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

    ; cache the start and end positions

    ; for convenience, let's deal with 8 bits
    lda X1_H
    pha                 ; store x1_h on the stack to restore at end of function
    and #$1
    sta X1_H            ; clamp max high value to 1
    lsr                 ; if X > 255, shift bit 0 into carry

    lda X1
    pha                 ; store x1 on the stack to restore at end of function
    ror                 ; divide by 2  and rotate carried bit into bit 7
    sta X1_
    
    ; do same for X2
    lda X2_H
    and #$1
    sta X2_H            ; clamp max high val to 1
    lsr
    lda X2
    ror
    sta X2_

    ; stash Y1
    lda Y1
    pha

    ; default increment to +1
    lda #$01 
    sta XADD
    sta YADD

    ; calc xdelta
    sec
    lda X2_
    sbc X1_
    sta XD  ; xdelta
    sta XT  ; xtemp

    ; calc ydelta
    sec
    lda Y2
    sbc Y1
    sta YD  ; ydelta
    sta YT  ; temp

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
    ; start drawing from x1,y1
    jsr draw_pixel

@xcheck:
    clc    
    lda XT
    adc XD 
    sta XT
    bcs @incX  ; if it carries on the xcheck, increment x

@ycheck:  
    clc     
    lda YT
    adc YD
    sta YT
    bcs @incY  ; carry on the ycheck means increment y

@testend:
    lda YD
    bne @xcheck

    lda XD
    bne @xcheck  

    bra @endloop



@incX:
; if X and X dest are equal, 
    lda X2_H
    cmp X1_H
    bne @incxsign 
    lda X2
    cmp X1
    beq @testend


@incxsign:
    lda X1
    clc
    adc XADD
    sta X1          ; update X1 with the +1 or -1

    lda XADD
    bmi @subx       ; was it subtraction?

    lda X1  
    bne @pixelskip  ; was addition, if result is 0, it means we rolled over
    inc X1_H
    bra @pixelskip

@subx:    
    lda X1          ; if it was subtraciton and X1 == #$FF, we rolled back
    cmp #$FF
    bne @pixelskip
    dec X1_H

@pixelskip:
    ; plot point (x3,y3)
    jsr draw_pixel

    ; have we completed X?
    lda X2_H
    cmp X1_H
    bne @ycheck
    lda X1
    cmp X2
    bne @ycheck

    lda #$00   ; we've hit X, null out XD
    sta XD
    bra @ycheck    

@incY:

    lda Y1
    clc
    adc YADD
    sta Y1

    ; plot point (x3, y3)
    jsr draw_pixel

    cmp Y2
    bne @xcheck

    lda #$00    ; we've hit Y
    sta YD
    bra @xcheck

@endloop:

    pla      ; restore Y1, X1, X1_H
    sta Y1
    pla
    sta X1
    pla
    sta X1_H

    ply
    plx
    pla
    rts

; ==============================================
; CONSOLE AND FONT
; ==============================================

clear_text_region:
    pha
    phx
    phy

@clearinput:
    ; clear the last line and draw screen
    ldx #$0
    ldy #$0

@loopy:
    lda eb_text_msb, y
    sta DEST_HIGH
    lda eb_text_lsb, y
    sta DEST_LOW
    lda #$00

; clear the input line
@loopx:
    sta (DEST_LOW)

    inc DEST_LOW
    bne @skipinc
    inc DEST_HIGH
@skipinc:
    inx
    cpx #$28
    bne @loopx
    iny
    cpy #$19
    bne @loopy

    ply
    plx
    pla

    rts

console_add_char:
    pha
    phy
    sta CHAR_DRAW

; check for non printables
    cmp #$00
    beq @done
    cmp #$0D
    beq @docr
    cmp #$0A
    beq @dolf
    cmp #$1b
    beq @clear
    cmp #$8
    bne @storechar

@backspace:
;backspace handling
    lda CURSOR_X
    beq @storechar
    lda #$00
    sta CHAR_DRAW  ; draw a blank char
    jsr draw_char
    
@storechar:
    ldy CURSOR_Y
    lda eb_text_msb, y
    sta CURSOR_ADDR_H
    lda eb_text_lsb, y
    sta CURSOR_ADDR

    ldy CURSOR_X
    ; write to text screen mem
    lda CHAR_DRAW
    sta (CURSOR_ADDR),Y

    ; if 0 char, skip to done
    bne @draw
    dec CURSOR_X
    jsr draw_char
    bra @done

@draw:
    jsr draw_char

@advance:
    inc CURSOR_X
    lda CURSOR_X
    cmp #$38        ; 40 chars max
    bne @done
    stz CURSOR_X
    bra @done

@dolf:
    lda #$00
    sta CHAR_DRAW
    inc CURSOR_Y
    lda CURSOR_Y
    cmp #$19    ; line 25?  scroll
    bne @draw
    dec CURSOR_Y
    jsr scroll_console
    bra @done
@docr:
    lda #$00
    sta CHAR_DRAW
    stz CURSOR_X
    bra @done

@clear:
    jsr cls

@done:
    ply
    pla    
    rts


; SCROLLING
scroll_console:
    pha
    phy
    phx

    ; copy from row 2-1, 3->2, etc...
    ldy #$0
@loopy:
    lda eb_text_msb, y
    sta DEST_HIGH
    lda eb_text_lsb, y
    sta DEST_LOW

    iny
    cpy #$19
    beq @clearinput
    
    lda eb_text_msb, y
    sta SOURCE_HIGH
    lda eb_text_lsb, y
    sta SOURCE_LOW

    ldx #$0
@loopx:
    ; copy from source to dest
    lda (SOURCE_LOW)
    sta (DEST_LOW)

    inc SOURCE_LOW
    bne @skipinc1
    inc SOURCE_HIGH
@skipinc1:
    
    inc DEST_LOW
    bne @skipinc2
    inc DEST_HIGH
@skipinc2:
 
    inx
    cpx #$28
    bne @loopx
    bra @loopy

@clearinput:
    ; clear the last line and draw screen
    ldx #$0
    ldy #$18

    lda eb_text_msb, y
    sta DEST_HIGH
    lda eb_text_lsb, y
    sta DEST_LOW
    lda #$00

; clear the input line
@loopclear:
    sta (DEST_LOW)

    inc DEST_LOW
    bne @skipinc3
    inc DEST_HIGH
@skipinc3:
    inx
    cpx #$28
    bne @loopclear

@draw:
    jsr draw_screen

    plx
    ply
    pla    
    rts

; DRAW SCREEN
draw_screen:
    pha
    phy
    phx

    ; stash the cursor
    lda CURSOR_X
    pha
    lda CURSOR_Y
    pha

    ldy #$00
@loopy:
    sty CURSOR_Y
    lda eb_text_msb, y
    sta DEST_HIGH
    lda eb_text_lsb, y
    sta DEST_LOW
    ldx #$00

@loopx: 
    stx CURSOR_X
    lda (DEST_LOW)
    sta CHAR_DRAW
    jsr draw_char

    inc DEST_LOW
    bne @skipinc
    inc DEST_HIGH
@skipinc:
    inx
    cpx #$28
    bne @loopx

    iny
    cpy #$19
    bne @loopy

@done:
    ; restore the cursor
    pla
    sta CURSOR_Y
    pla
    sta CURSOR_X
    plx
    ply
    pla    

    rts

; DRAW CHARACTER
draw_char:
    pha
    phx
    phy

    lda #<font_lookup
    sta FONTPTR
    lda #>font_lookup
    sta FONTPTR_H

    clc
    lda CHAR_DRAW
    asl
    bcc @nohigh
    inc FONTPTR_H
@nohigh:

    clc
    adc FONTPTR
    sta FONTPTR
    bcc @nohigh2
    inc FONTPTR_H
@nohigh2:

    ldy #$1
    lda (FONTPTR)
    tax
    lda (FONTPTR),y
    sta FONTPTR_H
    stx FONTPTR

    clc
    lda CURSOR_Y
    asl
    asl
    asl
    tay

    ldx #$00
@loopy:                  ; loop through 8 lines per char
    lda (HIRESPAGE),y
    sta ORIGIN_H
    lda eb_hires_lsb,y

    clc
    adc CURSOR_X
    bcc @skipcarry
    inc ORIGIN_H
@skipcarry:

    sta ORIGIN_L
    
    lda (FONTPTR)
    sta (ORIGIN_L)

    inc FONTPTR
    bne @skipcarry2
    inc FONTPTR_H
@skipcarry2:
    iny
    inx
    cpx #$8
    bne @loopy

    ply
    plx
    pla
    rts

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

    ldy #$C7
    ldx #$01   
    sty Y2
@effectx:
    ;inc DRAW_COLOR
    stx X2
    jsr draw_line
    inx
    bne @effectx

    inc X2_H
@effectx2:
    ;inc DRAW_COLOR
    stx X2
    jsr draw_line
    inx
    cpx #$3F
    bne @effectx2
@donex:
    dey
    dex
    stx X2

@effecty:
    ;inc DRAW_COLOR
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

    lda #$3F
    sta X2    

    lda #$C7
    sta Y2        ; Destination (320x200)

    jsr draw_line

    ; 0,128 - 255,0
    lda #$04
    sta DRAW_COLOR

    stz X1_H
    stz X1       ; (0, 200)
    lda #$C7
    sta Y1

    lda #$01
    sta X2_H
    lda #$3F
    sta X2        ; (320, 0)

    stz Y2
    jsr draw_line


    ; draw backwards

    ; 320,64 - 0,32 

    lda #$01
    sta X2_H

    lda #$3F
    sta X2    

    lda #$40
    sta Y2        ; Origin (320x64)

    ;lda #$02
    ;sta DRAW_COLOR

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

    ;lda #$03
    ;sta DRAW_COLOR

    stz X1
    stz X1_H
    stz X2
    stz X2_H
    stz Y2
    lda #$C7
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
    
    cmp #$3F
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

set_hires_page1:
    pha
    lda #<eb_hires1_msb
    sta HIRESPAGE
    lda #>eb_hires1_msb
    sta HIRESPAGE_H
    pla
    rts

set_hires_page2:
    pha
    lda #<eb_hires2_msb
    sta HIRESPAGE
    lda #>eb_hires2_msb
    sta HIRESPAGE_H
    pla
    rts

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


; ============================================================================================
; interrupts
; ============================================================================================

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
; https://github.com/idispatch/raster-fon(ts/blob/master/font-4x6.c
; removed

; Apple 2 high res lookup tables
;hires1_msb:
;        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
;        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
;        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
;        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
;        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
;        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
;        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
;        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
;        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
;        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
;        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
;        .byte $21, $25, $29, $2d, $31, $35, $39, $3dc
;        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
;        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
;        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
;        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
;        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
;        .byte $20, $24, $28, $2c, $30, $34, $38, $3c
;        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
;        .byte $21, $25, $29, $2d, $31, $35, $39, $3d
;        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
;        .byte $22, $26, $2a, $2e, $32, $36, $3a, $3e
;        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
;        .byte $23, $27, $2b, $2f, $33, $37, $3b, $3f
;hires_lsb:
;        .byte $00, $00, $00, $00, $00, $00, $00, $00
;        .byte $80, $80, $80, $80, $80, $80, $80, $80
;        .byte $00, $00, $00, $00, $00, $00, $00, $00
;        .byte $80, $80, $80, $80, $80, $80, $80, $80
;        .byte $00, $00, $00, $00, $00, $00, $00, $00
;        .byte $80, $80, $80, $80, $80, $80, $80, $80
;        .byte $00, $00, $00, $00, $00, $00, $00, $00
;        .byte $80, $80, $80, $80, $80, $80, $80, $80
;        .byte $28, $28, $28, $28, $28, $28, $28, $28
;        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
;        .byte $28, $28, $28, $28, $28, $28, $28, $28
;        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
;        .byte $28, $28, $28, $28, $28, $28, $28, $28
;        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
;        .byte $28, $28, $28, $28, $28, $28, $28, $28
;        .byte $a8, $a8, $a8, $a8, $a8, $a8, $a8, $a8
;        .byte $50, $50, $50, $50, $50, $50, $50, $50
;        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
;        .byte $50, $50, $50, $50, $50, $50, $50, $50
;        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
;        .byte $50, $50, $50, $50, $50, $50, $50, $50
;        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
;        .byte $50, $50, $50, $50, $50, $50, $50, $50
;        .byte $d0, $d0, $d0, $d0, $d0, $d0, $d0, $d0
;hires2_msb:
;        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
;        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
;        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
;        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
;        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
;        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
;        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
;        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
;        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
;        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
;        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
;        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
;        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
;        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
;        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
;        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
;        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
;        .byte $40, $44, $48, $4c, $50, $54, $58, $5c
;        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
;        .byte $41, $45, $49, $4d, $51, $55, $59, $5d
;        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
;        .byte $42, $46, $4a, $4e, $52, $56, $5a, $5e
;        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f
;        .byte $43, $47, $4b, $4f, $53, $57, $5b, $5f

; ebadger6502 high res lookup tables
eb_hires1_msb:
        .byte $20,$20,$20,$20,$20,$20,$20,$21
        .byte $21,$21,$21,$21,$21,$22,$22,$22
        .byte $22,$22,$22,$22,$23,$23,$23,$23
        .byte $23,$23,$24,$24,$24,$24,$24,$24
        .byte $25,$25,$25,$25,$25,$25,$25,$26
        .byte $26,$26,$26,$26,$26,$27,$27,$27
        .byte $27,$27,$27,$27,$28,$28,$28,$28
        .byte $28,$28,$29,$29,$29,$29,$29,$29
        .byte $2A,$2A,$2A,$2A,$2A,$2A,$2A,$2B
        .byte $2B,$2B,$2B,$2B,$2B,$2C,$2C,$2C
        .byte $2C,$2C,$2C,$2C,$2D,$2D,$2D,$2D
        .byte $2D,$2D,$2E,$2E,$2E,$2E,$2E,$2E
        .byte $2F,$2F,$2F,$2F,$2F,$2F,$2F,$30
        .byte $30,$30,$30,$30,$30,$31,$31,$31
        .byte $31,$31,$31,$31,$32,$32,$32,$32
        .byte $32,$32,$33,$33,$33,$33,$33,$33
        .byte $34,$34,$34,$34,$34,$34,$34,$35
        .byte $35,$35,$35,$35,$35,$36,$36,$36
        .byte $36,$36,$36,$36,$37,$37,$37,$37
        .byte $37,$37,$38,$38,$38,$38,$38,$38
        .byte $39,$39,$39,$39,$39,$39,$39,$3A
        .byte $3A,$3A,$3A,$3A,$3A,$3B,$3B,$3B
        .byte $3B,$3B,$3B,$3B,$3C,$3C,$3C,$3C
        .byte $3C,$3C,$3D,$3D,$3D,$3D,$3D,$3D
        .byte $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3F
eb_hires_lsb:
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8,$E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8,$20,$48,$70,$98
        .byte $C0,$E8,$10,$38,$60,$88,$B0,$D8
        .byte $00,$28,$50,$78,$A0,$C8,$F0,$18
eb_hires2_msb:
        .byte $40,$40,$40,$40,$40,$40,$40,$41
        .byte $41,$41,$41,$41,$41,$42,$42,$42
        .byte $42,$42,$42,$42,$43,$43,$43,$43
        .byte $43,$43,$44,$44,$44,$44,$44,$44
        .byte $45,$45,$45,$45,$45,$45,$45,$46
        .byte $46,$46,$46,$46,$46,$47,$47,$47
        .byte $47,$47,$47,$47,$48,$48,$48,$48
        .byte $48,$48,$49,$49,$49,$49,$49,$49
        .byte $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4B
        .byte $4B,$4B,$4B,$4B,$4B,$4C,$4C,$4C
        .byte $4C,$4C,$4C,$4C,$4D,$4D,$4D,$4D
        .byte $4D,$4D,$4E,$4E,$4E,$4E,$4E,$4E
        .byte $4F,$4F,$4F,$4F,$4F,$4F,$4F,$50
        .byte $50,$50,$50,$50,$50,$51,$51,$51
        .byte $51,$51,$51,$51,$52,$52,$52,$52
        .byte $52,$52,$53,$53,$53,$53,$53,$53
        .byte $54,$54,$54,$54,$54,$54,$54,$55
        .byte $55,$55,$55,$55,$55,$56,$56,$56
        .byte $56,$56,$56,$56,$57,$57,$57,$57
        .byte $57,$57,$58,$58,$58,$58,$58,$58
        .byte $59,$59,$59,$59,$59,$59,$59,$5A
        .byte $5A,$5A,$5A,$5A,$5A,$5B,$5B,$5B
        .byte $5B,$5B,$5B,$5B,$5C,$5C,$5C,$5C
        .byte $5C,$5C,$5D,$5D,$5D,$5D,$5D,$5D
        .byte $5E,$5E,$5E,$5E,$5E,$5E,$5E,$5F

eb_text_msb:
        .byte $04,$04,$04,$04
        .byte $04,$04,$04,$05
        .byte $05,$05,$05,$05
        .byte $05,$06,$06,$06
        .byte $06,$06,$06,$06
        .byte $07,$07,$07,$07
        .byte $07
eb_text_lsb:
        .byte $00,$28,$50,$78
        .byte $A0,$C8,$F0,$18
        .byte $40,$68,$90,$B8
        .byte $E0,$08,$30,$58
        .byte $80,$A8,$D0,$F8
        .byte $20,$48,$70,$98
        .byte $C0

font_lookup:
        .addr font8x8+$0000,font8x8+$0008,font8x8+$0010,font8x8+$0018
        .addr font8x8+$0020,font8x8+$0028,font8x8+$0030,font8x8+$0038
        .addr font8x8+$0040,font8x8+$0048,font8x8+$0050,font8x8+$0058
        .addr font8x8+$0060,font8x8+$0068,font8x8+$0070,font8x8+$0078
        .addr font8x8+$0080,font8x8+$0088,font8x8+$0090,font8x8+$0098
        .addr font8x8+$00A0,font8x8+$00A8,font8x8+$00B0,font8x8+$00B8
        .addr font8x8+$00C0,font8x8+$00C8,font8x8+$00D0,font8x8+$00D8
        .addr font8x8+$00E0,font8x8+$00E8,font8x8+$00F0,font8x8+$00F8
        .addr font8x8+$0100,font8x8+$0108,font8x8+$0110,font8x8+$0118
        .addr font8x8+$0120,font8x8+$0128,font8x8+$0130,font8x8+$0138
        .addr font8x8+$0140,font8x8+$0148,font8x8+$0150,font8x8+$0158
        .addr font8x8+$0160,font8x8+$0168,font8x8+$0170,font8x8+$0178
        .addr font8x8+$0180,font8x8+$0188,font8x8+$0190,font8x8+$0198
        .addr font8x8+$01A0,font8x8+$01A8,font8x8+$01B0,font8x8+$01B8
        .addr font8x8+$01C0,font8x8+$01C8,font8x8+$01D0,font8x8+$01D8
        .addr font8x8+$01E0,font8x8+$01E8,font8x8+$01F0,font8x8+$01F8
        .addr font8x8+$0200,font8x8+$0208,font8x8+$0210,font8x8+$0218
        .addr font8x8+$0220,font8x8+$0228,font8x8+$0230,font8x8+$0238
        .addr font8x8+$0240,font8x8+$0248,font8x8+$0250,font8x8+$0258
        .addr font8x8+$0260,font8x8+$0268,font8x8+$0270,font8x8+$0278
        .addr font8x8+$0280,font8x8+$0288,font8x8+$0290,font8x8+$0298
        .addr font8x8+$02A0,font8x8+$02A8,font8x8+$02B0,font8x8+$02B8
        .addr font8x8+$02C0,font8x8+$02C8,font8x8+$02D0,font8x8+$02D8
        .addr font8x8+$02E0,font8x8+$02E8,font8x8+$02F0,font8x8+$02F8
        .addr font8x8+$0300,font8x8+$0308,font8x8+$0310,font8x8+$0318
        .addr font8x8+$0320,font8x8+$0328,font8x8+$0330,font8x8+$0338
        .addr font8x8+$0340,font8x8+$0348,font8x8+$0350,font8x8+$0358
        .addr font8x8+$0360,font8x8+$0368,font8x8+$0370,font8x8+$0378
        .addr font8x8+$0380,font8x8+$0388,font8x8+$0390,font8x8+$0398
        .addr font8x8+$03A0,font8x8+$03A8,font8x8+$03B0,font8x8+$03B8
        .addr font8x8+$03C0,font8x8+$03C8,font8x8+$03D0,font8x8+$03D8
        .addr font8x8+$03E0,font8x8+$03E8,font8x8+$03F0,font8x8+$03F8
        .addr font8x8+$0400,font8x8+$0408,font8x8+$0410,font8x8+$0418
        .addr font8x8+$0420,font8x8+$0428,font8x8+$0430,font8x8+$0438
        .addr font8x8+$0440,font8x8+$0448,font8x8+$0450,font8x8+$0458
        .addr font8x8+$0460,font8x8+$0468,font8x8+$0470,font8x8+$0478
        .addr font8x8+$0480,font8x8+$0488,font8x8+$0490,font8x8+$0498
        .addr font8x8+$04A0,font8x8+$04A8,font8x8+$04B0,font8x8+$04B8
        .addr font8x8+$04C0,font8x8+$04C8,font8x8+$04D0,font8x8+$04D8
        .addr font8x8+$04E0,font8x8+$04E8,font8x8+$04F0,font8x8+$04F8
        .addr font8x8+$0500,font8x8+$0508,font8x8+$0510,font8x8+$0518
        .addr font8x8+$0520,font8x8+$0528,font8x8+$0530,font8x8+$0538
        .addr font8x8+$0540,font8x8+$0548,font8x8+$0550,font8x8+$0558
        .addr font8x8+$0560,font8x8+$0568,font8x8+$0570,font8x8+$0578
        .addr font8x8+$0580,font8x8+$0588,font8x8+$0590,font8x8+$0598
        .addr font8x8+$05A0,font8x8+$05A8,font8x8+$05B0,font8x8+$05B8
        .addr font8x8+$05C0,font8x8+$05C8,font8x8+$05D0,font8x8+$05D8
        .addr font8x8+$05E0,font8x8+$05E8,font8x8+$05F0,font8x8+$05F8
        .addr font8x8+$0600,font8x8+$0608,font8x8+$0610,font8x8+$0618
        .addr font8x8+$0620,font8x8+$0628,font8x8+$0630,font8x8+$0638
        .addr font8x8+$0640,font8x8+$0648,font8x8+$0650,font8x8+$0658
        .addr font8x8+$0660,font8x8+$0668,font8x8+$0670,font8x8+$0678
        .addr font8x8+$0680,font8x8+$0688,font8x8+$0690,font8x8+$0698
        .addr font8x8+$06A0,font8x8+$06A8,font8x8+$06B0,font8x8+$06B8
        .addr font8x8+$06C0,font8x8+$06C8,font8x8+$06D0,font8x8+$06D8
        .addr font8x8+$06E0,font8x8+$06E8,font8x8+$06F0,font8x8+$06F8
        .addr font8x8+$0700,font8x8+$0708,font8x8+$0710,font8x8+$0718
        .addr font8x8+$0720,font8x8+$0728,font8x8+$0730,font8x8+$0738
        .addr font8x8+$0740,font8x8+$0748,font8x8+$0750,font8x8+$0758
        .addr font8x8+$0760,font8x8+$0768,font8x8+$0770,font8x8+$0778
        .addr font8x8+$0780,font8x8+$0788,font8x8+$0790,font8x8+$0798
        .addr font8x8+$07A0,font8x8+$07A8,font8x8+$07B0,font8x8+$07B8
        .addr font8x8+$07C0,font8x8+$07C8,font8x8+$07D0,font8x8+$07D8
        .addr font8x8+$07E0,font8x8+$07E8,font8x8+$07F0,font8x8+$07F8

font8x8:
        .byte $00,$00,$00,$00,$00,$00,$00,$00      ;  0
        .byte $7e,$81,$a5,$81,$bd,$99,$81,$7e      ;  1
        .byte $7e,$ff,$db,$ff,$c3,$e7,$ff,$7e      ;  2
        .byte $6c,$fe,$fe,$fe,$7c,$38,$10,$00      ;  3
        .byte $10,$38,$7c,$fe,$7c,$38,$10,$00      ;  4
        .byte $38,$7c,$38,$fe,$fe,$d6,$10,$38      ;  5
        .byte $10,$38,$7c,$fe,$fe,$7c,$10,$38      ;  6
        .byte $00,$00,$18,$3c,$3c,$18,$00,$00      ;  7
        .byte $ff,$ff,$e7,$c3,$c3,$e7,$ff,$ff      ;  8
        .byte $00,$3c,$66,$42,$42,$66,$3c,$00      ;  9
        .byte $ff,$c3,$99,$bd,$bd,$99,$c3,$ff      ;  10
        .byte $0f,$07,$0f,$7d,$cc,$cc,$cc,$78      ;  11
        .byte $3c,$66,$66,$66,$3c,$18,$7e,$18      ;  12
        .byte $3f,$33,$3f,$30,$30,$70,$f0,$e0      ;  13
        .byte $7f,$63,$7f,$63,$63,$67,$e6,$c0      ;  14
        .byte $18,$db,$3c,$e7,$e7,$3c,$db,$18      ;  15
        .byte $80,$e0,$f8,$fe,$f8,$e0,$80,$00      ;  16
        .byte $02,$0e,$3e,$fe,$3e,$0e,$02,$00      ;  17
        .byte $18,$3c,$7e,$18,$18,$7e,$3c,$18      ;  18
        .byte $66,$66,$66,$66,$66,$00,$66,$00      ;  19
        .byte $7f,$db,$db,$7b,$1b,$1b,$1b,$00      ;  20
        .byte $3e,$61,$3c,$66,$66,$3c,$86,$7c      ;  21
        .byte $00,$00,$00,$00,$7e,$7e,$7e,$00      ;  22
        .byte $18,$3c,$7e,$18,$7e,$3c,$18,$ff      ;  23
        .byte $18,$3c,$7e,$18,$18,$18,$18,$00      ;  24
        .byte $18,$18,$18,$18,$7e,$3c,$18,$00      ;  25
        .byte $00,$18,$0c,$fe,$0c,$18,$00,$00      ;  26
        .byte $00,$30,$60,$fe,$60,$30,$00,$00      ;  27
        .byte $00,$00,$c0,$c0,$c0,$fe,$00,$00      ;  28
        .byte $00,$24,$66,$ff,$66,$24,$00,$00      ;  29
        .byte $00,$18,$3c,$7e,$ff,$ff,$00,$00      ;  30
        .byte $00,$ff,$ff,$7e,$3c,$18,$00,$00      ;  31
        .byte $00,$00,$00,$00,$00,$00,$00,$00      ;  32, " "
        .byte $18,$3c,$3c,$18,$18,$00,$18,$00      ;  33, "!"
        .byte $66,$66,$24,$00,$00,$00,$00,$00      ;  34, """
        .byte $6c,$6c,$fe,$6c,$fe,$6c,$6c,$00      ;  35, "#"
        .byte $18,$3e,$60,$3c,$06,$7c,$18,$00      ;  36, "$"
        .byte $00,$c6,$cc,$18,$30,$66,$c6,$00      ;  37, "%"
        .byte $38,$6c,$38,$76,$dc,$cc,$76,$00      ;  38, "&"
        .byte $18,$18,$30,$00,$00,$00,$00,$00      ;  39, "'"
        .byte $0c,$18,$30,$30,$30,$18,$0c,$00      ;  40, "("
        .byte $30,$18,$0c,$0c,$0c,$18,$30,$00      ;  41, ")"
        .byte $00,$66,$3c,$ff,$3c,$66,$00,$00      ;  42, "*"
        .byte $00,$18,$18,$7e,$18,$18,$00,$00      ;  43, "+"
        .byte $00,$00,$00,$00,$00,$18,$18,$30      ;  44, ","
        .byte $00,$00,$00,$7e,$00,$00,$00,$00      ;  45, "-"
        .byte $00,$00,$00,$00,$00,$18,$18,$00      ;  46, "."
        .byte $06,$0c,$18,$30,$60,$c0,$80,$00      ;  47, "/"
        .byte $38,$6c,$c6,$d6,$c6,$6c,$38,$00      ;  48, "0"
        .byte $18,$38,$18,$18,$18,$18,$7e,$00      ;  49, "1"
        .byte $7c,$c6,$06,$1c,$30,$66,$fe,$00      ;  50, "2"
        .byte $7c,$c6,$06,$3c,$06,$c6,$7c,$00      ;  51, "3"
        .byte $1c,$3c,$6c,$cc,$fe,$0c,$1e,$00      ;  52, "4"
        .byte $fe,$c0,$c0,$fc,$06,$c6,$7c,$00      ;  53, "5"
        .byte $38,$60,$c0,$fc,$c6,$c6,$7c,$00      ;  54, "6"
        .byte $fe,$c6,$0c,$18,$30,$30,$30,$00      ;  55, "7"
        .byte $7c,$c6,$c6,$7c,$c6,$c6,$7c,$00      ;  56, "8"
        .byte $7c,$c6,$c6,$7e,$06,$0c,$78,$00      ;  57, "9"
        .byte $00,$18,$18,$00,$00,$18,$18,$00      ;  58, ":"
        .byte $00,$18,$18,$00,$00,$18,$18,$30      ;  59, ";"
        .byte $06,$0c,$18,$30,$18,$0c,$06,$00      ;  60, "<"
        .byte $00,$00,$7e,$00,$00,$7e,$00,$00      ;  61, "="
        .byte $60,$30,$18,$0c,$18,$30,$60,$00      ;  62, ">"
        .byte $7c,$c6,$0c,$18,$18,$00,$18,$00      ;  63, "?"
        .byte $7c,$c6,$de,$de,$de,$c0,$78,$00      ;  64, "@"
        .byte $38,$6c,$c6,$fe,$c6,$c6,$c6,$00      ;  65, "A"
        .byte $fc,$66,$66,$7c,$66,$66,$fc,$00      ;  66, "B"
        .byte $3c,$66,$c0,$c0,$c0,$66,$3c,$00      ;  67, "C"
        .byte $f8,$6c,$66,$66,$66,$6c,$f8,$00      ;  68, "D"
        .byte $fe,$62,$68,$78,$68,$62,$fe,$00      ;  69, "E"
        .byte $fe,$62,$68,$78,$68,$60,$f0,$00      ;  70, "F"
        .byte $3c,$66,$c0,$c0,$ce,$66,$3a,$00      ;  71, "G"
        .byte $c6,$c6,$c6,$fe,$c6,$c6,$c6,$00      ;  72, "H"
        .byte $3c,$18,$18,$18,$18,$18,$3c,$00      ;  73, "I"
        .byte $1e,$0c,$0c,$0c,$cc,$cc,$78,$00      ;  74, "J"
        .byte $e6,$66,$6c,$78,$6c,$66,$e6,$00      ;  75, "K"
        .byte $f0,$60,$60,$60,$62,$66,$fe,$00      ;  76, "L"
        .byte $c6,$ee,$fe,$fe,$d6,$c6,$c6,$00      ;  77, "M"
        .byte $c6,$e6,$f6,$de,$ce,$c6,$c6,$00      ;  78, "N"
        .byte $7c,$c6,$c6,$c6,$c6,$c6,$7c,$00      ;  79, "O"
        .byte $fc,$66,$66,$7c,$60,$60,$f0,$00      ;  80, "P"
        .byte $7c,$c6,$c6,$c6,$c6,$ce,$7c,$0e      ;  81, "Q"
        .byte $fc,$66,$66,$7c,$6c,$66,$e6,$00      ;  82, "R"
        .byte $3c,$66,$30,$18,$0c,$66,$3c,$00      ;  83, "S"
        .byte $7e,$7e,$5a,$18,$18,$18,$3c,$00      ;  84, "T"
        .byte $c6,$c6,$c6,$c6,$c6,$c6,$7c,$00      ;  85, "U"
        .byte $c6,$c6,$c6,$c6,$c6,$6c,$38,$00      ;  86, "V"
        .byte $c6,$c6,$c6,$d6,$d6,$fe,$6c,$00      ;  87, "W"
        .byte $c6,$c6,$6c,$38,$6c,$c6,$c6,$00      ;  88, "X"
        .byte $66,$66,$66,$3c,$18,$18,$3c,$00      ;  89, "Y"
        .byte $fe,$c6,$8c,$18,$32,$66,$fe,$00      ;  90, "Z"
        .byte $3c,$30,$30,$30,$30,$30,$3c,$00      ;  91, "["
        .byte $c0,$60,$30,$18,$0c,$06,$02,$00      ;  92, "\"
        .byte $3c,$0c,$0c,$0c,$0c,$0c,$3c,$00      ;  93, "]"
        .byte $10,$38,$6c,$c6,$00,$00,$00,$00      ;  94, "^"
        .byte $00,$00,$00,$00,$00,$00,$00,$ff      ;  95, "_"
        .byte $30,$18,$0c,$00,$00,$00,$00,$00      ;  96, "`"
        .byte $00,$00,$78,$0c,$7c,$cc,$76,$00      ;  97, "a"
        .byte $e0,$60,$7c,$66,$66,$66,$dc,$00      ;  98, "b"
        .byte $00,$00,$7c,$c6,$c0,$c6,$7c,$00      ;  99, "c"
        .byte $1c,$0c,$7c,$cc,$cc,$cc,$76,$00      ;  100, "d"
        .byte $00,$00,$7c,$c6,$fe,$c0,$7c,$00      ;  101, "e"
        .byte $3c,$66,$60,$f8,$60,$60,$f0,$00      ;  102, "f"
        .byte $00,$00,$76,$cc,$cc,$7c,$0c,$f8      ;  103, "g"
        .byte $e0,$60,$6c,$76,$66,$66,$e6,$00      ;  104, "h"
        .byte $18,$00,$38,$18,$18,$18,$3c,$00      ;  105, "i"
        .byte $06,$00,$06,$06,$06,$66,$66,$3c      ;  106, "j"
        .byte $e0,$60,$66,$6c,$78,$6c,$e6,$00      ;  107, "k"
        .byte $38,$18,$18,$18,$18,$18,$3c,$00      ;  108, "l"
        .byte $00,$00,$ec,$fe,$d6,$d6,$d6,$00      ;  109, "m"
        .byte $00,$00,$dc,$66,$66,$66,$66,$00      ;  110, "n"
        .byte $00,$00,$7c,$c6,$c6,$c6,$7c,$00      ;  111, "o"
        .byte $00,$00,$dc,$66,$66,$7c,$60,$f0      ;  112, "p"
        .byte $00,$00,$76,$cc,$cc,$7c,$0c,$1e      ;  113, "q"
        .byte $00,$00,$dc,$76,$60,$60,$f0,$00      ;  114, "r"
        .byte $00,$00,$7e,$c0,$7c,$06,$fc,$00      ;  115, "s"
        .byte $30,$30,$fc,$30,$30,$36,$1c,$00      ;  116, "t"
        .byte $00,$00,$cc,$cc,$cc,$cc,$76,$00      ;  117, "u"
        .byte $00,$00,$c6,$c6,$c6,$6c,$38,$00      ;  118, "v"
        .byte $00,$00,$c6,$d6,$d6,$fe,$6c,$00      ;  119, "w"
        .byte $00,$00,$c6,$6c,$38,$6c,$c6,$00      ;  120, "x"
        .byte $00,$00,$c6,$c6,$c6,$7e,$06,$fc      ;  121, "y"
        .byte $00,$00,$7e,$4c,$18,$32,$7e,$00      ;  122, "z"
        .byte $0e,$18,$18,$70,$18,$18,$0e,$00      ;  123, "{"
        .byte $18,$18,$18,$18,$18,$18,$18,$00      ;  124, "|"
        .byte $70,$18,$18,$0e,$18,$18,$70,$00      ;  125, "}"
        .byte $76,$dc,$00,$00,$00,$00,$00,$00      ;  126, "~"
        .byte $00,$10,$38,$6c,$c6,$c6,$fe,$00      ;  127
        .byte $7c,$c6,$c0,$c0,$c6,$7c,$0c,$78      ;  128
        .byte $cc,$00,$cc,$cc,$cc,$cc,$76,$00      ;  129
        .byte $0c,$18,$7c,$c6,$fe,$c0,$7c,$00      ;  130
        .byte $7c,$82,$78,$0c,$7c,$cc,$76,$00      ;  131
        .byte $c6,$00,$78,$0c,$7c,$cc,$76,$00      ;  132
        .byte $30,$18,$78,$0c,$7c,$cc,$76,$00      ;  133
        .byte $30,$30,$78,$0c,$7c,$cc,$76,$00      ;  134
        .byte $00,$00,$7e,$c0,$c0,$7e,$0c,$38      ;  135
        .byte $7c,$82,$7c,$c6,$fe,$c0,$7c,$00      ;  136
        .byte $c6,$00,$7c,$c6,$fe,$c0,$7c,$00      ;  137
        .byte $30,$18,$7c,$c6,$fe,$c0,$7c,$00      ;  138
        .byte $66,$00,$38,$18,$18,$18,$3c,$00      ;  139
        .byte $7c,$82,$38,$18,$18,$18,$3c,$00      ;  140
        .byte $30,$18,$00,$38,$18,$18,$3c,$00      ;  141
        .byte $c6,$38,$6c,$c6,$fe,$c6,$c6,$00      ;  142
        .byte $38,$6c,$7c,$c6,$fe,$c6,$c6,$00      ;  143
        .byte $18,$30,$fe,$c0,$f8,$c0,$fe,$00      ;  144
        .byte $00,$00,$7e,$18,$7e,$d8,$7e,$00      ;  145
        .byte $3e,$6c,$cc,$fe,$cc,$cc,$ce,$00      ;  146
        .byte $7c,$82,$7c,$c6,$c6,$c6,$7c,$00      ;  147
        .byte $c6,$00,$7c,$c6,$c6,$c6,$7c,$00      ;  148
        .byte $30,$18,$7c,$c6,$c6,$c6,$7c,$00      ;  149
        .byte $78,$84,$00,$cc,$cc,$cc,$76,$00      ;  150
        .byte $60,$30,$cc,$cc,$cc,$cc,$76,$00      ;  151
        .byte $c6,$00,$c6,$c6,$c6,$7e,$06,$fc      ;  152
        .byte $c6,$38,$6c,$c6,$c6,$6c,$38,$00      ;  153
        .byte $c6,$00,$c6,$c6,$c6,$c6,$7c,$00      ;  154
        .byte $18,$18,$7e,$c0,$c0,$7e,$18,$18      ;  155
        .byte $38,$6c,$64,$f0,$60,$66,$fc,$00      ;  156
        .byte $66,$66,$3c,$7e,$18,$7e,$18,$18      ;  157
        .byte $f8,$cc,$cc,$fa,$c6,$cf,$c6,$c7      ;  158
        .byte $0e,$1b,$18,$3c,$18,$d8,$70,$00      ;  159
        .byte $18,$30,$78,$0c,$7c,$cc,$76,$00      ;  160
        .byte $0c,$18,$00,$38,$18,$18,$3c,$00      ;  161
        .byte $0c,$18,$7c,$c6,$c6,$c6,$7c,$00      ;  162
        .byte $18,$30,$cc,$cc,$cc,$cc,$76,$00      ;  163
        .byte $76,$dc,$00,$dc,$66,$66,$66,$00      ;  164
        .byte $76,$dc,$00,$e6,$f6,$de,$ce,$00      ;  165
        .byte $3c,$6c,$6c,$3e,$00,$7e,$00,$00      ;  166
        .byte $38,$6c,$6c,$38,$00,$7c,$00,$00      ;  167
        .byte $18,$00,$18,$18,$30,$63,$3e,$00      ;  168
        .byte $00,$00,$00,$fe,$c0,$c0,$00,$00      ;  169
        .byte $00,$00,$00,$fe,$06,$06,$00,$00      ;  170
        .byte $63,$e6,$6c,$7e,$33,$66,$cc,$0f      ;  171
        .byte $63,$e6,$6c,$7a,$36,$6a,$df,$06      ;  172
        .byte $18,$00,$18,$18,$3c,$3c,$18,$00      ;  173
        .byte $00,$33,$66,$cc,$66,$33,$00,$00      ;  174
        .byte $00,$cc,$66,$33,$66,$cc,$00,$00      ;  175
        .byte $22,$88,$22,$88,$22,$88,$22,$88      ;  176
        .byte $55,$aa,$55,$aa,$55,$aa,$55,$aa      ;  177
        .byte $77,$dd,$77,$dd,$77,$dd,$77,$dd      ;  178
        .byte $18,$18,$18,$18,$18,$18,$18,$18      ;  179
        .byte $18,$18,$18,$18,$f8,$18,$18,$18      ;  180
        .byte $18,$18,$f8,$18,$f8,$18,$18,$18      ;  181
        .byte $36,$36,$36,$36,$f6,$36,$36,$36      ;  182
        .byte $00,$00,$00,$00,$fe,$36,$36,$36      ;  183
        .byte $00,$00,$f8,$18,$f8,$18,$18,$18      ;  184
        .byte $36,$36,$f6,$06,$f6,$36,$36,$36      ;  185
        .byte $36,$36,$36,$36,$36,$36,$36,$36      ;  186
        .byte $00,$00,$fe,$06,$f6,$36,$36,$36      ;  187
        .byte $36,$36,$f6,$06,$fe,$00,$00,$00      ;  188
        .byte $36,$36,$36,$36,$fe,$00,$00,$00      ;  189
        .byte $18,$18,$f8,$18,$f8,$00,$00,$00      ;  190
        .byte $00,$00,$00,$00,$f8,$18,$18,$18      ;  191
        .byte $18,$18,$18,$18,$1f,$00,$00,$00      ;  192
        .byte $18,$18,$18,$18,$ff,$00,$00,$00      ;  193
        .byte $00,$00,$00,$00,$ff,$18,$18,$18      ;  194
        .byte $18,$18,$18,$18,$1f,$18,$18,$18      ;  195
        .byte $00,$00,$00,$00,$ff,$00,$00,$00      ;  196
        .byte $18,$18,$18,$18,$ff,$18,$18,$18      ;  197
        .byte $18,$18,$1f,$18,$1f,$18,$18,$18      ;  198
        .byte $36,$36,$36,$36,$37,$36,$36,$36      ;  199
        .byte $36,$36,$37,$30,$3f,$00,$00,$00      ;  200
        .byte $00,$00,$3f,$30,$37,$36,$36,$36      ;  201
        .byte $36,$36,$f7,$00,$ff,$00,$00,$00      ;  202
        .byte $00,$00,$ff,$00,$f7,$36,$36,$36      ;  203
        .byte $36,$36,$37,$30,$37,$36,$36,$36      ;  204
        .byte $00,$00,$ff,$00,$ff,$00,$00,$00      ;  205
        .byte $36,$36,$f7,$00,$f7,$36,$36,$36      ;  206
        .byte $18,$18,$ff,$00,$ff,$00,$00,$00      ;  207
        .byte $36,$36,$36,$36,$ff,$00,$00,$00      ;  208
        .byte $00,$00,$ff,$00,$ff,$18,$18,$18      ;  209
        .byte $00,$00,$00,$00,$ff,$36,$36,$36      ;  210
        .byte $36,$36,$36,$36,$3f,$00,$00,$00      ;  211
        .byte $18,$18,$1f,$18,$1f,$00,$00,$00      ;  212
        .byte $00,$00,$1f,$18,$1f,$18,$18,$18      ;  213
        .byte $00,$00,$00,$00,$3f,$36,$36,$36      ;  214
        .byte $36,$36,$36,$36,$ff,$36,$36,$36      ;  215
        .byte $18,$18,$ff,$18,$ff,$18,$18,$18      ;  216
        .byte $18,$18,$18,$18,$f8,$00,$00,$00      ;  217
        .byte $00,$00,$00,$00,$1f,$18,$18,$18      ;  218
        .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff      ;  219
        .byte $00,$00,$00,$00,$ff,$ff,$ff,$ff      ;  220
        .byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0      ;  221
        .byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f      ;  222
        .byte $ff,$ff,$ff,$ff,$00,$00,$00,$00      ;  223
        .byte $00,$00,$76,$dc,$c8,$dc,$76,$00      ;  224
        .byte $78,$cc,$cc,$d8,$cc,$c6,$cc,$00      ;  225
        .byte $fe,$c6,$c0,$c0,$c0,$c0,$c0,$00      ;  226
        .byte $00,$00,$fe,$6c,$6c,$6c,$6c,$00      ;  227
        .byte $fe,$c6,$60,$30,$60,$c6,$fe,$00      ;  228
        .byte $00,$00,$7e,$d8,$d8,$d8,$70,$00      ;  229
        .byte $00,$00,$66,$66,$66,$66,$7c,$c0      ;  230
        .byte $00,$76,$dc,$18,$18,$18,$18,$00      ;  231
        .byte $7e,$18,$3c,$66,$66,$3c,$18,$7e      ;  232
        .byte $38,$6c,$c6,$fe,$c6,$6c,$38,$00      ;  233
        .byte $38,$6c,$c6,$c6,$6c,$6c,$ee,$00      ;  234
        .byte $0e,$18,$0c,$3e,$66,$66,$3c,$00      ;  235
        .byte $00,$00,$7e,$db,$db,$7e,$00,$00      ;  236
        .byte $06,$0c,$7e,$db,$db,$7e,$60,$c0      ;  237
        .byte $1e,$30,$60,$7e,$60,$30,$1e,$00      ;  238
        .byte $00,$7c,$c6,$c6,$c6,$c6,$c6,$00      ;  239
        .byte $00,$fe,$00,$fe,$00,$fe,$00,$00      ;  240
        .byte $18,$18,$7e,$18,$18,$00,$7e,$00      ;  241
        .byte $30,$18,$0c,$18,$30,$00,$7e,$00      ;  242
        .byte $0c,$18,$30,$18,$0c,$00,$7e,$00      ;  243
        .byte $0e,$1b,$1b,$18,$18,$18,$18,$18      ;  244
        .byte $18,$18,$18,$18,$18,$d8,$d8,$70      ;  245
        .byte $00,$18,$00,$7e,$00,$18,$00,$00      ;  246
        .byte $00,$76,$dc,$00,$76,$dc,$00,$00      ;  247
        .byte $38,$6c,$6c,$38,$00,$00,$00,$00      ;  248
        .byte $00,$00,$00,$18,$18,$00,$00,$00      ;  249
        .byte $00,$00,$00,$18,$00,$00,$00,$00      ;  250
        .byte $0f,$0c,$0c,$0c,$ec,$6c,$3c,$1c      ;  251
        .byte $6c,$36,$36,$36,$36,$00,$00,$00      ;  252
        .byte $78,$0c,$18,$30,$7c,$00,$00,$00      ;  253
        .byte $00,$00,$3c,$3c,$3c,$3c,$00,$00      ;  254
        .byte $00,$00,$00,$00,$00,$00,$00,$00      ;  255
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
                AND #$07        ; For MOD 8=0
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
