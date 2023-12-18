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
LMNEM   = $2c
RTNL    = $2c

V2      = $2d
RMNEM   = $2d
RTNH    = $2d

MASK    = $2e
CHKSUM  = $2e
FORMAT  = $2e

LASTIN  = $2f
LENGTH  = $2f
A2SIGN  = $2f

COLOR   = $30
A2MODE  = $31
INVFLG  = $32
PROMPT  = $33
A2YSAV  = $34

YSAV1   = $35

CSWL    = $36
KSWL    = $38

PCL     = $3a
PCH     = $3b
XQT     = $3c

A1L     = $3c
A1H     = $3d
A2L     = $3e
A2H     = $3f
A3L     = $40
A3H     = $41
A4L     = $42
A4H     = $43

A5L     = $44
ACC     = $45
XREG    = $46
YREG    = $47

STATUS  = $48
SPNT    = $49

RNDL    = $4e
RNDH    = $4f

ACL     = $50
ACH     = $51
XTNDL   = $52
XTNDH   = $53
AUXL    = $54
AUXH    = $55
PICK    = $95

IN      = $0200  ;{addr/256}
USRADR  = $03f8  ;{addr/3}
NMI     = $03fb  ;{addr/3}
IRQLOC  = $03fe  ;{addr/2}

LINE1   = $0400           ;{addr/40}

SOFTEV  = $03f2           ;{addr/2} ;vector for warm start
PWREDUP = $03f4           ;this must = EOR #$A5 of SOFTEV+1
IOADR   = $c000
KEYBD   = $c000           ;R last key pressed + 128
KBDSTRB = $c010           ;RW keyboard strobe
TAPEOUT = $c020           ;RW toggle caseette tape output
SPKR    = $c030           ;RW toggle speaker
TXTCLR  = $c050           ;RW display graphics
TXTSET  = $c051           ;RW display text
MIXSET  = $c053           ;RW display split screen
TXTPAGE1= $c054           ;RW display page 1
LORES   = $c056           ;RW display lo-res graphics
TAPEIN  = $c060           ;R read cassette input

PADDL0  = $c064           ;R analog input 0
PTRIG   = $c070           ;RW analog input reset

BASIC   = $e000
BASIC2  = $e003

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

NXTCOL:
    lda     COLOR
    clc
    adc     #$03
SETCOL:
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

INSDS1:
    ldx     PCL             ;print PCL,H
    ldy     PCH
    jsr     PRYX2
    jsr     PRBLNK          ;followed by a blank
    lda     (PCL,x)         ;get op code
INSDS2:
    tay
    lsr     A               ;even/odd test
    bcc     IEVEN
    ror     A               ;bit 1 test
    bcs     ERR             ;XXXXXX11 invalid op
    cmp     #$a2
    beq     ERR             ;opcode $89 invalid
    and     #$87            ;mask bits
IEVEN:
    lsr     A               ;LSB into carry for L/R test
    tax
    lda     FMT1,x          ;get format index byte
    jsr     SCRN2           ;R/L H-byte on carry
    bne     GETFMT
ERR:
    ldy     #$80            ;substitute $80 for invalid ops
    lda     #$00            ;set print format index to 0
GETFMT:
    tax
    lda     FMT2,x          ;index into print format table
    sta     FORMAT          ;save for adr field formatting
    and     #$03            ;mask for 2-bit length (P=1 byte, 1=2 byte, 2=3 byte)
    sta     LENGTH
    tya                     ;opcode
    and     #$8f            ;mask for 1XXX1010 test
    tax                     ; save it
    tya                     ;opcode to A again
    ldy     #$03
    cpx     #$8a
    beq     MNNDX3
MNNDX1:
    lsr     A
    bcc     MNNDX3          ;form index into mnemonic table
    lsr     A
MNNDX2:
    lsr     A               ;1) 1XXX1010=>00101XXX
    ora     #$20            ;2) XXXYYY01=>00111XXX
    dey                     ;3) XXXYYY10=>00110XXX
    bne     MNNDX2          ;4) XXXYY100=>00100XXX
    iny                     ;5) XXXXX000=>000XXXXX
MNNDX3:
    dey
    bne     MNNDX1
    rts

    .byte   $ff,$ff,$ff

INSTDSP:
    jsr     INSDS1          ;gen fmt, len bytes
    pha                     ;save mnemonic table index
PRNTOP:
    lda     (PCL),y
    jsr     PRBYTE
    ldx     #$01            ;print 2 blanks
PRNTBL:
    jsr     PRBL2
    cpy     LENGTH          ;print inst (1-3 bytes)
    iny                     ;in a 12 chr field
    bcc     PRNTOP
    ldx     #$03            ;char count for mnemonic print
    cpy     #$04
    bcc     PRNTBL
    pla                     ;recover mnemonic index
    tay
    lda     MNEML,y
    sta     LMNEM           ;fech 3-char mnemonic
    lda     MNEMR,y         ;  (packed in 2-bytes)
    sta     RMNEM
PRMN1:
    lda     #$00
    ldy     #$05
PRMN2:
    asl     RMNEM           ;shift 5 bits of
    rol     LMNEM           ;  character into A
    rol     A               ;    (clear carry)
    dey
    bne     PRMN2
    adc     #$bf            ;add "?" offset
    jsr     COUT            ;output a char of mnem
    dex
    bne     PRMN1
    jsr     PRBLNK          ;output 3 blanks
    ldy     LENGTH
    ldx     #$06            ;cnt for 6 format bits
PRADR1:
    cpx     #$03
    beq     PRADR5          ;if X=3 then addr.
PRADR2:
    asl     FORMAT
    bcc     PRADR3
    lda     CHAR1-1,x
    jsr     COUT
    lda     CHAR2-1,x
    beq     PRADR3
    jsr     COUT
PRADR3:
    dex
    bne     PRADR1
    rts

PRADR4:
    dey
    bmi     PRADR2
    jsr     PRBYTE
PRADR5:
    lda     FORMAT
    cmp     #$e8            ;handle rel adr mode
    lda     (PCL),y         ;special (print target,
    bcc     PRADR4          ;  not offset)
RELADR:
    jsr     PCADJ3
    tax                     ;PCL,PCH+OFFSET+1 to A,Y
    inx
    bne     PRNTYX          ;+1 to Y,X
    iny
PRNTYX:
    tya
PRNTAX:
    jsr     PRBYTE          ;output target adr
PRNTX:
    txa                     ;  of branch and return
    jmp     PRBYTE

PRBLNK:
    ldx     #$03            ;blank count
PRBL2:
    lda     #$a0            ;load a space
PRBL3:
    jsr     COUT            ;output a blank
    dex
    bne     PRBL2           ;loop until count=0
    rts

PCADJ:
    sec                     ;0=1-byte,1=2-byte,
PCADJ2:      
    lda     LASTIN          ;  2=3-byte
PCADJ3:
    ldy     PCH
    tax                     ;test displacement sign
    bpl     PCADJ4          ;  (for rel branch)
    dey                     ;extend neg by decr PCH
PCADJ4:
    adc     PCL
    bcc     RTS2            ;PCL+LENGTH(or DISPL)+1 to A
    iny                     ;  carry into Y (PCH)
RTS2:
    rts

                   ; FMT1 bytes:  XXXXXXY0 instrs
                   ; if Y=0       then left half byte
                   ; if Y=1       then right half byte
                   ;                   (x=index)
FMT1:
    .byte   $04,$20,$54,$30,$0d,$80,$04,$90,$03,$22,$54,$33,$0d,$80,$04,$90
    .byte   $04,$20,$54,$33,$0d,$80,$04,$90,$04,$20,$54,$3b,$0d,$80,$04,$90
    .byte   $00,$22,$44,$33,$0d,$c8,$44,$00,$11,$22,$44,$33,$0d,$c8,$44,$a9
    .byte   $01,$22,$44,$33,$0d,$80,$04,$90,$01,$22,$44,$33,$0d,$80,$04,$90
    .byte   $26,$31,$87,$9a
                   ; ZZXXXY01 instr's
FMT2:
    .byte    $00             ;err
    .byte    $21             ;imm
    .byte    $81             ;z-page
    .byte    $82             ;abs
    .byte    $00             ;implied
    .byte    $00             ;accumulator
    .byte    $59             ;(zpag,x)
    .byte    $4d             ;(zpag),y
    .byte    $91             ;zpag,x
    .byte    $92             ;abs,x
    .byte    $86             ;abs,y
    .byte    $4a             ;(abs)
    .byte    $85             ;zpag,y
    .byte    $9d             ;relative
CHAR1:       
    .byte    ",),#($"
CHAR2:
    .byte    "Y"
    .byte    $00
    .byte    "X"
    .byte    "$"
    .byte    "$"
    .byte    $00
                   ; MNEML is of form:
                   ; (A) XXXXX000
                   ; (B) XXXYY100
                   ; (C) 1XXX1010
                   ; (D) XXXYYY10
                   ; (E) XXXYYY01
                   ;     (X=index)
MNEML:
    .byte $1c,$8a,$1c,$23,$5d,$8b,$1b,$a1,$9d,$8a,$1d,$23,$9d,$8b,$1d,$a1
    .byte $00,$29,$19,$ae,$69,$a8,$19,$23,$24,$53,$1b,$23,$24,$53,$19,$a1
    .byte $00,$1a,$5b,$5b,$a5,$69,$24,$24,$ae,$ae,$a8,$ad,$29,$00,$7c,$00
    .byte $15,$9c,$6d,$9c,$a5,$69,$29,$53,$84,$13,$34,$11,$a5,$69,$23,$a0

MNEMR:
    .byte   $d8,$62,$5a,$48,$26,$62,$94,$88,$54,$44,$c8,$54,$68,$44,$e8,$94
    .byte   $00,$b4,$08,$84,$74,$b4,$28,$6e,$74,$f4,$cc,$4a,$72,$f2,$a4,$8a
    .byte   $00,$aa,$a2,$a2,$74,$74,$74,$72,$44,$68,$b2,$32,$b2,$00,$22,$00
    .byte   $1a,$1a,$26,$26,$72,$72,$88,$c8,$c4,$ca,$26,$48,$44,$44,$a2,$c8
    .byte   $ff,$ff,$ff

A2STEP:
    jsr     INSTDSP         ;disassemble one inst
    pla                     ;  at (PCL,H)
    sta     RTNL            ;adjust to user
    pla                     ;  stack. Save
    sta     RTNH            ;  rtn adr.
    ldx     #$08
XQINIT:
    lda     INITBL-1,x      ;init XEQ area
    sta     XQT,x
    dex
    bne     XQINIT
    lda     (PCL,x)         ;user opcode byte
    beq     XBRK            ;special if break
    ldy     LENGTH          ;len from disassembly
    cmp     #$20
    beq     XJSR            ;handle JSR, RTS, JMP,
    cmp     #$60            ;  JMP ( ), RTI special
    beq     XRTS
    cmp     #$4c
    beq     XJMP
    cmp     #$6c
    beq     XJMPAT
    cmp     #$40
    beq     XRTI
    and     #$1f
    eor     #$14
    cmp     #$04            ;copy user inst to XEQ area
    beq     XQ2             ;  with trailing NOPs
XQ1:
    lda     (PCL),y         ;change rel branch
XQ2:
    sta     XQT,y           ;  disp to 4 for
    dey                     ;  jmp to branch or
    bpl     XQ1             ;  nbranch from XEQ.
    jsr     A2RESTORE       ; restore user reg contents.
    jmp     XQT             ; XEQ user op from RAM (return to NBRANCH)

IRQ:
    sta     ACC
    pla
    pha                     ;**IRQ handler
    asl     A
    asl     A
    asl     A
    bmi     BREAK           ;test for break
    jmp     (IRQLOC)        ;user routine vector in RAM

BREAK:
    plp
    jsr     SAV1            ;ave reg's on break
    pla                     ;  including PC
    sta     PCL
    pla
    sta     PCH
XBRK:
    jsr     INSDS1          ;print user PC.
    jsr     RGDSP1          ;  and reg's
    jmp     MON             ;go to monitor

XRTI:
    clc
    pla                     ;simulate RTI by expecting
    sta     STATUS          ;  status from stack, then RTS
XRTS:
    pla                     ;RTS simulation
    sta     PCL             ;  extract PC from stack
    pla                     ;  and update PC by 1 (len=0)
PCINC2:
    sta     PCH
PCINC3:
    lda     LENGTH          ;update PC by len
    jsr     PCADJ3
    sty     PCH
    clc
    bcc     NEWPCL

XJSR:
    clc
    jsr     PCADJ2          ;update PC and push
    tax                     ;  onto stack for
    tya                     ;  JSR simulate
    pha
    txa
    pha
    ldy     #$02
XJMP:
    clc
XJMPAT:
    lda     (PCL),y
    tax                     ;load PC for JMP,
    dey                     ;  (JMP) simulate.
    lda     (PCL),y
    stx     PCH
NEWPCL:      
    sta     PCL
    bcs     XJMP
RTNJMP:
    lda     RMNEM
    pha
    lda     H2
    pha
REGDSP:
    jsr     CROUT           ;display user reg
RGDSP1:
    lda     #$45            ;  contents with
    sta     A3L             ;  labels
    lda     #$00
    sta     A3H
    ldx     #$fb
RDSP1:       
    lda     #$a0
    jsr     COUT
    lda     MNEMR+30,x
    jsr     COUT
    lda     #$bd
    jsr     COUT
    lda     $4a,x
    jsr     PRBYTE
    inx
    bmi     RDSP1
    rts

BRANCH:
    clc                     ;branch taken,
    ldy     #$01            ;  add len+2 to PC
    lda     (PCL),y
    jsr     PCADJ3
    sta     PCL
    tya
    sec
    bcs     PCINC2
NBRNCH:
    jsr     A2SAVE            ;normal return after
    sec                     ;  XEQ user of
    bcs     PCINC3          ;go update PC

INITBL:
    nop
    nop                     ;dummy fill for
    jmp     NBRNCH          ;  XEQ area

    jmp     BRANCH

    .byte    "AXYPS"

PREAD:
    lda     PTRIG
    ldy     #$00
    nop
    nop
PREAD2:
    lda     PADDL0,x
    bpl     RTS2D
    iny
    bne     PREAD2
    dey
RTS2D:       
    rts

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

MULPM:
    jsr     MD1             ;abs val of AC AUX
MUL:
    ldy     #$10            ;index for 16 bits
MUL2:
    lda     ACL             ;ACX * AUX + XTND
    lsr     A               ;  to AC, XTND
    bcc     MUL4            ;if no carry,
    clc                     ;  no partial prod.
    ldx     #$fe
MUL3:
    lda     XTNDL+2,x       ;add mplcnd (AUX)
    adc     AUXL+2,x        ; to partial prod
    sta     XTNDL+2,x       ;    (XTND).
    inx
    bne     MUL3
MUL4:
    ldx     #$03
MUL5:
    ror     ACL,x           ;(original src: DFB #$76, DFB #$50)
    dex
    bpl     MUL5
    dey
    bne     MUL2
    rts

DIVPM:
    jsr     MD1             ;abs val of AC, AUX.
A2DIV:
    ldy     #$10            ;index for 16 bits
DIV2:
    asl     ACL
    rol     ACH
    rol     XTNDL           ;XTND/AUX
    rol     XTNDH           ;  to AC.
    sec
    lda     XTNDL
    sbc     AUXL            ;mod to XTND.
    tax
    lda     XTNDH
    sbc     AUXH
    bcc     DIV3
    stx     XTNDL
    sta     XTNDH
    inc     ACL
DIV3:
    dey
    bne     DIV2
    rts

MD1:
    ldy     #$00            ;abs val of AC, AUX
    sty     LASTIN          ;  with result sign
    ldx     #AUXL           ;  in LSB of SIGN.
    jsr     MD2
    ldx     #ACL
MD2:
    lda     LOC1,x          ;X specifies AC or AUX
    bpl     MDRTS
    sec
MD3:
    tya
    sbc     LOC0,x          ;compl specified reg
    sta     LOC0,x          ;  if neg.
    tya
    sbc     LOC1,x
    sta     LOC1,x
    inc     A2SIGN
MDRTS:
    rts

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
    jsr     WAIT1
    ldy     #$c0
BELL2:
    lda     #$0c
    jsr     WAIT1
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
    bcs     CR
_RTS3:
    rts

VIDOUT:
    cmp     #$A0
    bcs     STORADV
    tay
    bpl     STORADV
    cmp     #$8d
    beq     CR
    cmp     #$8a
    beq     LF
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
    bcc     LF              ;C, down
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
CR:
    lda     #$00
    sta     CH
LF:
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
    lda     #$A0
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

HEADR:       
    ldy     #$4b            ;write A*256 'long 1'
    jsr     ZERDLY          ;  half cycles
    bne     HEADR           ;  (650 usec each)
    adc     #$fe
    bcs     HEADR           ;then a 'short 0'
    ldy     #$21            ;  (400 usec)
WRBIT:
    jsr     ZERDLY          ;write two half cycles
    iny                     ;  of 250 usec ('0')
    iny                     ;  or 500 usec ('0')
ZERDLY:
    dey
    bne     ZERDLY
    bcc     WRTAPE          ;Y is count for
    ldy     #$32            ;  timing loop
ONEDLY:
    dey
    bne     ONEDLY
WRTAPE:
    ldy     TAPEOUT
    ldy     #$2c
    dex
    rts

RDBYTE:
    ldx     #$08            ;8 bits to read
RDBYT2:
    pha                     ;read two transitions
    jsr     RD2BIT          ;  (find edge)
    pla
    rol     A               ;next bit
    ldy     #$3a            ;count for samples
    dex
    bne     RDBYT2
    rts

RD2BIT:
    jsr     RDBIT
RDBIT:
    dey                     ;decr Y until
    lda     TAPEIN          ;  tape transition
    eor     LASTIN
    bpl     RDBIT
    eor     LASTIN
    sta     LASTIN
    cpy     #$80            ;set carry on Y-reg.
    rts

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
    stz     KEYBD             ; bit     KBDSTRB
    rts

ESC:
    jsr     RDKEY
    jsr     ESC1
RDCHAR:
    jsr     RDKEY
    cmp     #$9b
    beq     ESC
    rts

A2NOTCR:
    lda     INVFLG
    pha
    lda     #$ff
    sta     INVFLG          ;echo user line
    lda     IN,x            ;  non inverse
    jsr     COUT
    pla
    sta     INVFLG
    lda     IN,x
    cmp     #$88            ;check for edit keys
    beq     BCKSPC          ;  BS, ctrl-X
    cmp     #$98
    beq     CANCEL
    cpx     #$f8            ;margin?
    bcc     NOTCR1
    jsr     BELL            ;yes, sound bell
NOTCR1:
    inx                     ;advance input index
    bne     NXTCHAR
CANCEL:
    lda     #$dc            ;backslash after cancelled lin
    jsr     COUT
GETLNZ:
    jsr     CROUT           ;output CR
A2GETLN:
    lda     PROMPT
    jsr     COUT            ;output prompt char
    ldx     #$01            ;init input index
BCKSPC:
    txa                     ;  will backspace to 0
    beq     GETLNZ
    dex
NXTCHAR:
    jsr     RDCHAR
    cmp     #PICK           ;use screen char
    bne     CAPTST          ;  for ctrl-U
    lda     (BASL),y
CAPTST:
    cmp     #$e0
    bcc     ADDINP          ;convert to caps
    and     #$df
ADDINP:
    sta     IN,x            ;add to input buf
    cmp     #$8d
    bne     A2NOTCR
    jsr     CLREOL          ;clr to EOL if CR
CROUT:
    lda     #$8d
    bne     COUT

PRA1:
    ldy     A1H             ;print CR,A1 in hex
    ldx     A1L
PRYX2:
    jsr     CROUT
    jsr     PRNTYX
    ldy     #$00
    lda     #$ad            ;print '-'
    jmp     COUT

XAM8:
    lda     A1L
    ora     #$07            ;set to finish at
    sta     A2L             ;  mod 8=7
    lda     A1H
    sta     A2H
A2MOD8CHK:
    lda     A1L
    and     #$07
    bne     DATACUT
XAM:
    jsr     PRA1
DATACUT:
    lda     #$a0
    jsr     COUT            ;output blank
    lda     (A1L),y
    jsr     PRBYTE          ;output byte in hex
    jsr     NXTA1
    bcc     A2MOD8CHK         ;check if time to,
RTS4C:
    rts                     ;  print addr

XAMPM:
    lsr     A               ;determine if mon
    bcc     XAM             ;  mode is xam
    lsr     A               ;  add, or sub
    lsr     A
    lda     A2L
    bcc     ADD
    eor     #$ff            ;sub: form 2's complement
ADD:
    adc     A1L
    pha
    lda     #$bd
    jsr     COUT            ;print '=', then result
    pla
PRBYTE:
    pha                     ;print byte as 2 hex
    lsr     A               ;  digits, destroys A-reg
    lsr     A
    lsr     A
    lsr     A
    jsr     PRHEXZ
    pla
PRHEX:
    and     #$0f            ;print hex dig in A-reg
PRHEXZ:
    ora     #$b0            ;  LSB's
    cmp     #$ba
    bcc     COUT
    adc     #$06

COUT:
    jmp     (CSWL)
COUT1:
    cmp     #$a0
    bcc     COUTZ           ;don't output ctrl's inverse
    and     INVFLG          ;mask with inverse flag
COUTZ:
    sty     YSAV1           ;sav Y-reg
    pha                     ;sav A-reg
    jsr     VIDOUT          ;output A-reg as ASCII
    pla                     ;restore A-reg
    ldy     YSAV1           ;  and Y-reg
    rts

BL1:
    dec     A2YSAV
    beq     XAM8
BLANK:
    dex                     ;blank to mon
    bne     SETMDZ          ;after blank
    cmp     #$ba            ;data store mode?
    bne     XAMPM           ;  no, xam, add or sub
STOR:
    sta     A2MODE            ;keep in store mode
    lda     A2L
    sta     (A3L),y         ;store as lwo byte as (A3)
    inc     A3L
    bne     RTS5            ;incr A3, return
    inc     A3H
RTS5:
    rts

SETMODE:
    ldy     A2YSAV            ;save converted ':', '+',
    lda     IN-1,y          ;  '-', '.' as mode.
SETMDZ:
    sta     A2MODE
    rts

LT:
    ldx     #$01
LT2:
    lda     A2L,x           ;copy A2 (2 bytes) to
    sta     A4L,x           ;  A4 and A5
    sta     A5L,x
    dex
    bpl     LT2
    rts

;.res $38
;.org $FE2C
MOVE:
    lda     (A1L),y
    sta     (A4L),y
    jsr     NXTA4
    bcc     MOVE
    rts

VFY:
    lda     (A1L),y         ;verify (A1 to A2) with
    cmp     (A4L),y         ;  (A4)
    beq     VFYOK
    jsr     PRA1
    lda     (A1L),y
    jsr     PRBYTE
    lda     #$a0
    jsr     COUT
    lda     #$a8
    jsr     COUT
    lda     (A4L),y
    jsr     PRBYTE
    lda     #$a9
    jsr     COUT
VFYOK:
    jsr     NXTA4
    bcc     VFY
    rts

A2LIST:
    jsr     A1PC            ;move A1 (2 bytes) to
    lda     #$14            ;  PC if spec'd and
LIST2:
    pha                     ;  dissemble 20 instrs
    jsr     INSTDSP
    jsr     PCADJ           ;adjust PC each instr
    sta     PCL
    sty     PCH
    pla
    sec
    sbc     #$01            ;next of 20 instrs
    bne     LIST2
    rts

A1PC:
    txa                     ;if user spec'd adr
    beq     A1PCRTS         ;  copy from A1 to PC
A1PCLP:
    lda     A1L,x
    sta     PCL,x
    dex
    bpl     A1PCLP
A1PCRTS:
    rts

SETINV:
    ldy     #$3f            ;set for inverse vid
    bne     SETIFLG         ;  via COUT1

SETNORM:
    ldy     #$ff            ;set for normal vid
SETIFLG:
    sty     INVFLG
    rts

;.res $53
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

    nop
    nop
XBASIC:
    jmp     BASIC           ;to BASIC with scratch

BASCONT:
    jmp     BASIC2          ;continue BASIC

GO:
    jsr     A1PC            ;adr to PC if spec'd
    jsr     A2RESTORE       ;restore meta regs
    jmp     (PCL)           ;go to user subr

REGZ:
    jmp     REGDSP          ;to reg display

TRACE:
    dec     A2YSAV
STEPZ:
    jsr     A1PC            ;adr to PC if spec'd
    jmp     STEP            ;take one step

A2USR:
    jmp     USRADR          ;to usr subr at USRADR

WRITE:
    lda     #$40
    jsr     HEADR           ;write 10-sec header
    ldy     #$27
WR1:
    ldx     #$00
    eor     (A1L,x)
    pha
    lda     (A1L,x)
    jsr     WRBYTE
    jsr     NXTA1
    ldy     #$1d
    pla
    bcc     WR1
    ldy     #$22
    jsr     WRBYTE
    beq     BELL
WRBYTE:
    ldx     #$10
WRBYT2:
    asl     A
    jsr     WRBIT
    bne     WRBYT2
    rts

CRMON:
    jsr     BL1             ;handle CR as blank
    pla                     ;  then pop stack
    pla                     ;  and rtn to mon
    bne     MONZ

A2READ:
    jsr     RD2BIT          ;find tapein edge
    lda     #$16
    jsr     HEADR           ;delay 3.5 seconds
    sta     CHKSUM          ;init CHKSUM=$ff
    jsr     RD2BIT          ;find tapein edge
RD2:
    ldy     #$24            ;look for sync bit
    jsr     RDBIT           ;  (short 0)
    bcs     RD2             ;  loop until found
    jsr     RDBIT           ;skip second sync H-cycle
    ldy     #$3b            ;index for 0/1 test
RD3:
    jsr     RDBYTE          ;read a byte
    sta     (A1L,x)         ;store at (A1)
    eor     CHKSUM
    sta     CHKSUM          ;update running chksum
    jsr     NXTA1           ;incr A1, compare to A2
    ldy     #$35            ;compenstate 0/1 index
    bcc     RD3             ;loop until done
    jsr     RDBYTE          ;read chksum byte
    cmp     CHKSUM
    beq     BELL            ;good, sound bell and return
PRERR:
    lda     #$c5
    jsr     COUT            ;print "ERR", then bell
    lda     #$d2
    jsr     COUT
    jsr     COUT
BELL:
    lda     #$87            ;output bell and return
    jmp     COUT

A2RESTORE:
    lda     STATUS          ;restore 6502 reg contents
    pha                     ;  used by debug software
    lda     ACC
RESTR1:
    ldx     XREG
    ldy     YREG
    plp
    rts

A2SAVE:
    sta     ACC             ;save 6502 reg contents
SAV1:
    stx     XREG
    sty     YREG
    php
    pla
    sta     STATUS
    tsx
    stx     SPNT
    cld
    rts

RESET:
    jsr     SETNORM         ;  set screen mode
    jsr     A2INIT          ;  and init kbd/screen
    jsr     SETVID          ;  as I/O dev's
    jsr     SETKBD
MON:
    cld                     ;must set hex mode!
    jsr     BELL
MONZ:
    lda     #$aa            ;'*' prompt for mon
    sta     PROMPT
    jsr     GETLNZ          ;read a line
    jsr     ZMODE           ;clear mon mode, scan idx
NXTITM:
    jsr     GETNUM          ;get item, non-hex
    sty     A2YSAV          ;char in A-reg
    ldy     #$17            ;  X-reg=0 if no hex input
CHRSRCH:
    dey
    bmi     MON             ;not found, go to mon
    cmp     CHRTBL,y        ;find cmnd char in tbl
    bne     CHRSRCH
    jsr     TOSUB           ;found, call corresponding
    ldy     A2YSAV          ;  subroutine
    jmp     NXTITM

DIG:
    ldx     #$03
    asl     A
    asl     A               ;got hex dig,
    asl     A               ;shift into A2
    asl     A
NXTBIT:
    asl     A
    rol     A2L
    rol     A2H
    dex                     ;leave X=$ff if dig
    bpl     NXTBIT
NXTBAS:
    lda     A2MODE
    bne     NXTBS2          ;if mode is zero
    lda     A2H,x           ;  then copy A2 to
    sta     A1H,x           ;  A1 and A3
    sta     A3H,x
NXTBS2:
    inx
    beq     NXTBAS
    bne     NXTCHR

GETNUM:
    ldx     #$00            ;clear A2
    stx     A2L
    stx     A2H
NXTCHR:
    lda     IN,y            ;get char
    iny
    eor     #$b0
    cmp     #$0a
    bcc     DIG             ;if hex dig, then
    adc     #$88
    cmp     #$fa
    bcs     DIG
    rts

TOSUB:
    lda     #>GO            ;push high-order
    pha                     ;  subr adr on stk
    lda     SUBTBL,y        ;push low order
    pha                     ;  subr adr on stk
    lda     A2MODE
ZMODE:
    ldy     #$00            ;clr mode, old mode
    sty     A2MODE          ;  to A-reg
    rts                     ;go to subr via RTS

CHRTBL:
    .byte    $bc             ;F("Ctrl+C")
    .byte    $b2             ;F("Ctrl+Y")
    .byte    $be             ;F("Ctrl+E")
    .byte    $ed             ;F("T")
    .byte    $ef             ;F("V")
    .byte    $c4             ;F("Ctrl+K")
    .byte    $ec             ;F("S")
    .byte    $a9             ;F("Ctrl+P")
    .byte    $bb             ;F("Ctrl+B")
    .byte    $a6             ;F("-")
    .byte    $a4             ;F("+")
    .byte    $06             ;F("M")  (F=EX-OR $B0+$89)
    .byte    $95             ;F("<")
    .byte    $07             ;F("N")
    .byte    $02             ;F("I")
    .byte    $05             ;F("L")
    .byte    $f0             ;F("W")
    .byte    $00             ;F("G")
    .byte    $eb             ;G("R")
    .byte    $93             ;F(":")
    .byte    $a7             ;F(".")
    .byte    $c6             ;F("CR")
    .byte    $99             ;F(BLANK)
SUBTBL:
    .byte    <BASCONT-1
    .byte    <A2USR-1
    .byte    <REGZ-1
    .byte    <TRACE-1
    .byte    <VFY-1
    .byte    <INPRT-1
    .byte    <STEPZ-1
    .byte    <OUTPRT-1
    .byte    <XBASIC-1
    .byte    <SETMODE-1
    .byte    <SETMODE-1
    .byte    <MOVE-1
    .byte    <LT-1
    .byte    <SETNORM-1
    .byte    <SETINV-1
    .byte    <A2LIST-1
    .byte    <WRITE-1
    .byte    <GO-1
    .byte    <A2READ-1
    .byte    <SETMODE-1
    .byte    <SETMODE-1
    .byte    <CRMON-1
    .byte    <BLANK-1
 
 ;   .dd2    NMI             ;NMI vector
 ;   .dd2    RESET           ;reset vector
 ;   .dd2    IRQ             ;IRQ vector