.segment "A2MON"
; APPLE ROM

LOC0    = $00
LOC1    = $01
WNDLFT  = $20
WNDWDTH = $21
WNDTOP  = $22
WNDBTM  = $23
CH      = $24
CV      = $25
GBASL   = $26
GBASH   = $27
BASL    = $28
BASH    = $29
BAS2L   = $2a
BAS2H   = $2b

H2      = $2c
V2      = $2d
MASK    = $2e

COLOR   = $30

CSWL    = $36
KSWL    = $38

A1L     = $3c
A1H     = $3d
A2L     = $3e
A2H     = $3f
A3L     = $40
A3H     = $41
A4L     = $42
A4H     = $43

STATUS  = $48
RNDL    = $4e
RNDH    = $4f

IOADR   = $c000
KEYBD   = $c000           ;R last key pressed + 128
KBDSTRB = $c010           ;RW keyboard strobe
SPKR    = $c030           ;RW toggle speaker
TXTCLR  = $c050           ;RW display graphics
TXTSET  = $c051           ;RW display text
MIXSET  = $c053           ;RW display split screen
TXTPAGE1= $c054           ;RW display page 1
LORES   = $c056           ;RW display lo-res graphics
 
PLOT:
    lsr     A
    php
    jsr     GBASCALC
    plp
    lda     #$0f
    bcc     RTMASK
    adc     #$e0
RTMASK:
    sta     MASK
PLOT1:
    lda     (GBASL),y
    eor     COLOR
    and     MASK
    eor     (GBASL),y
    sta     (GBASL),y
    rts

HLINE:
    jsr     PLOT
HLINE1:
    cpy     H2
    bcs     RTS1
    iny
    jsr     PLOT1
    bcc     HLINE1
VLINEZ:
    adc     #$01
VLINE:
    pha
    jsr     PLOT
    pla
    cmp     V2
    bcc     VLINEZ
RTS1:
    rts

CLRSCR:
    ldy     #$2f
    bne     CLRSC2

CLRTOP:
    ldy     #$27
CLRSC2:
    sty     V2
    ldy     #$27
CLRSC3:
    lda     #$00
    sta     COLOR
    jsr     VLINE
    dey
    bpl     CLRSC3
    rts

GBASCALC:
    pha
    lsr     A
    and     #$03
    ora     #$04
    sta     GBASH
    pla
    and     #$18
    bcc     GBCALC
    adc     #$7f
GBCALC:
    sta     GBASL
    asl     A
    asl     A
    ora     GBASL
    sta     GBASL
    rts

NEXTCOL:
    lda     COLOR
    clc
    adc     #$03
    and     #$0f
    sta     COLOR
    asl     A
    asl     A
    asl     A
    asl     A
    ora     COLOR
    sta     COLOR
    rts

SCRN:
    lsr     A
    php
    jsr     GBASCALC
    lda     (GBASL),y
    plp
SCRN2:
    bcc     RTMSKZ
    lsr     A
    lsr     A
    lsr     A
    lsr     A
RTMSKZ:
    and     #$0f
    rts

;.org $fb11
.res $28F
XLTBL:
    .byte $c4,$c2,$c1,$ff,$c3,$ff,$ff,$ff

;.org $FB1E
.res $5
PREAD:
    RTS

A2INIT:
    lda     #$00
    sta     STATUS
    lda     LORES
    lda     TXTPAGE1
SETTEXT:
    lda     TXTSET
    lda     #$00
    beq     SETWND

SETGR:
    lda     TXTCLR
    lda     MIXSET
    jsr     CLRTOP
    lda     #$14
SETWND:
    sta     WNDTOP
    lda     #$00
    sta     WNDLFT
    lda     #$28
    sta     WNDWDTH
    lda     #$18
    sta     WNDBTM
    lda     #$17
TABV:
    sta     CV
    jmp     VTAB

;.org $FB97
.res $47
ESCOLD:
    sec                     ;insure carry set
    jmp     ESC1

ESCNOW:
    tay                     ;use char as index
    lda     XLTBL-201,y     ;xlate IJKM to CBAD
    jsr     ESCOLD          ;do this cursor motion
    jsr     RDKEY           ;and get next
ESCNEW:
    cmp     #$ce            ;is this an N ?
    bcs     ESCOLD          ;N or greater do it
    cmp     #$c9            ;less than I ?
    bcc     ESCOLD          ;yes so old way
    cmp     #$cc            ;is it a L ?
    beq     ESCOLD          ;do normal
    bne     ESCNOW          ;go do it

.res $E
;.org $FBC1
BASCALC:
    pha
    lsr     A
    and     #$03
    ora     #$04
    sta     BASH
    pla
    and     #$18
    bcc     BASCLC2
    adc     #$7f
BASCLC2:
    sta     BASL
    asl     A
    asl     A
    ora     BASL
    sta     BASL
    rts

BELL1:
    cmp     #$87
    bne     RTS2B
    lda     #$40
    jsr     WAIT
    ldy     #$c0
BELL2:
    lda     #$0c
    jsr     WAIT
    lda     SPKR
    dey
    bne     BELL2
RTS2B:
    rts

;.org $FBf0
STORADV:
    ldy     CH
    sta     (BASL),y
ADVANCE:
    inc     CH
    lda     CH
    cmp     WNDWDTH
    bcs     _CR
_RTS3:
    rts

VIDOUT:
    cmp     #$a0
    bcs     STORADV
    tay
    bpl     STORADV
    cmp     #$8d
    beq     _CR
    cmp     #$8a
    beq     _LF
    cmp     #$88
    bne     BELL1
BS:
    dec     CH
    bpl     _RTS3
    lda     WNDWDTH
    sta     CH
    dec     CH
UP:
    lda     WNDTOP
    cmp     CV
    bcs     RTS4
    dec     CV

;.res $32
;.org $FC22
VTAB:
    lda     CV
VTABZ:
    jsr     BASCALC
    adc     WNDLFT
    sta     BASL
RTS4:
    rts

ESC1:
    eor     #$c0            ;esc @ ?
    beq     HOME            ;if so do home and clear
    adc     #$fd            ;esc-A or B check
    bcc     ADVANCE         ;A, advance
    beq     BS              ;B, backspace
    adc     #$fd            ;esc-C or D check
    bcc     _LF              ;C, down
    beq     UP              ;D, go up
    adc     #$fd            ;esc-E or F check
    bcc     CLREOL          ;E, clear to end of line
    bne     RTS4            ;else not F, return
CLREOP:
    ldy     CH              ;esc F is clr to end of page
    lda     CV

CLEOP1:
    pha
    jsr     VTABZ
    jsr     CLEOLZ
    ldy     #$00
    pla
    adc     #$00
    cmp     WNDBTM
    bcc     CLEOP1
    bcs     VTAB

HOME:
    lda     WNDTOP
    sta     CV
    ldy     #$00
    sty     CH
    beq     CLEOP1
_CR:
    lda     #$00
    sta     CH
_LF:
    inc     CV
    lda     CV
    cmp     WNDBTM
    bcc     VTABZ
    dec     CV
SCROLL:
    lda     WNDTOP
    pha
    jsr     VTABZ
SCRL1:
    lda     BASL
    sta     BAS2L
    lda     BASH
    sta     BAS2H
    ldy     WNDWDTH
    dey
    pla
    adc     #$01
    cmp     WNDBTM
    bcs     SCRL3
    pha
    jsr     VTABZ
SCRL2:
    lda     (BASL),y
    sta     (BAS2L),y
    dey
    bpl     SCRL2
    bmi     SCRL1
SCRL3:
    ldy     #$00
    jsr     CLEOLZ
    bcs     VTAB
CLREOL:
    ldy     CH
CLEOLZ:
    lda     #$a0
CLREOL2:
    sta     (BASL),y
    iny
    cpy     WNDWDTH
    bcc     CLREOL2
    rts

WAIT1:
    sec
WAIT2:
    pha
WAIT3:
    sbc     #$01
    bne     WAIT3
    pla
    sbc     #$01
    bne     WAIT2
    rts

NXTA4:
    inc     A4L
    bne     NXTA1
    inc     A4H
NXTA1:
    lda     A1L
    cmp     A2L
    lda     A1H
    sbc     A2H
    inc     A1L
    bne     RTS4B
    inc     A1H
RTS4B:           
    rts

.res $43
;.org $FD0C
RDKEY:
    ldy     CH
    lda     (BASL),y
    pha
    and     #$3f
    ora     #$40
    sta     (BASL),y
    pla
    jmp     (KSWL)

KEYIN:
    inc     RNDL
    bne     KEYIN2
    inc     RNDH
KEYIN2:
    bit     KEYBD             ;read keyboard
    bpl     KEYIN
    sta     (BASL),y
    lda     KEYBD
    bit     KBDSTRB
    rts

ESC:
    jsr     RDKEY
    jsr     ESCNEW
RDCHAR:
    jsr     RDKEY
    cmp     #$9b
    beq     ESC
    rts

.res $9D
;.org $FDDA     
PRBYTE1:
    JSR print_hex
    RTS

.res $F
;.org $FDED
COUT1:
    jsr	display_char
    rts

.res $3B
;.org $FE2C
MOVE:
    lda     (A1L),y
    sta     (A4L),y
    jsr     NXTA4
    bcc     MOVE
    rts

.res $53
;.org $FE89
SETKBD:
    lda     #$00
INPORT:
    sta     A2L
INPRT1:
    ldx     #KSWL
    ldy     #<KEYIN
    bne     IOPRT

SETVID:
    lda     #$00
OUTPORT:
    sta     A2L
OUTPRT:
    ldx     #CSWL
    ldy     #<COUT1
IOPRT:
    lda     A2L
    and     #$0f
    beq     IOPRT1
    ora     #>IOADR
    ldy     #$00
    beq     IOPRT2
IOPRT1:
    lda     #>COUT1
IOPRT2:
    sty     LOC0,x          ;$94,$00
    sta     LOC1,x          ;$95,$01
    rts