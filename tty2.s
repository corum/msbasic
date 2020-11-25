; memory map
; 0x0000 - 0x7FFF  RAM (32KB)
; 0x8000 - 0x8FFF  Devices (4KB) 16 devices, 256 bytes each, D1 = 0x8000-0x80FF, D2= 0x8100-0x81FF ...
; 0x9000 - 0xFFFF  ROM (28KB)


; PS/2 keyboard state
KBCURR   = $A2
KBBUF    = $100
KEYSTATE = $200
KEYTEMP  = $A8

; VGA display stuff
COORD  = $80
SHADOW = $90

COLVAL = $80
ROWVAL = $81

SHADOWCOL = $90
SHADOWROW = $91

.org $2000

init:
    ldx #$FF 
    txs

    lda #$00
    sta COLVAL
    sta SHADOWCOL

    lda #$C0
    sta ROWVAL
    and #$7F
    sta SHADOWROW

    jsr cls

loop:
    jsr read_char
    jsr output_char
    jmp loop       ; repeat


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
    cli

    plx
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

