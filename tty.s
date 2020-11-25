; memory map
; 0x0000 - 0x7FFF  RAM (32KB)
; 0x8000 - 0x8FFF  Devices (4KB) 16 devices, 256 bytes each, D1 = 0x8000-0x80FF, D2= 0x8100-0x81FF ...
; 0x9000 - 0xFFFF  ROM (28KB)


; VGA display stuff

A_RXD = $8100
A_TXD = $8100
A_STS = $8101
A_RES = $8101
A_CMD = $8102
A_CTL = $8103

COORD  = $80
SHADOW = $90

COLVAL = $80
ROWVAL = $81

SHADOWCOL = $90
SHADOWROW = $91

.org $2000

init:
    sei
    ldx #$FF 
    txs

    lda #$00
    sta COLVAL
    sta SHADOWCOL

    lda #$C0
    sta ROWVAL
    and #$7F
    sta SHADOWROW

; initialize the ACIA
    sta A_RES      ; soft reset (value not important)
    lda #$0B       ; set specific modes and functions
                   ; no parity, no echo, no Tx interrupt, no Rx interrupt, enable Tx/Rx
    sta A_CMD      ; store to the command register

    ;lda #$00      ; 1 stop bits, 8 bit word length, external clock, 16x baud rate
    lda #$1F       ; 1 stop bits, 8 bit word length, internal clock, 19.2k baud rate
    sta A_CTL      ; program the ctl register

loop:
    jsr rx_char_sync
    jsr output_char
    jsr tx_char_sync
    jmp loop       ; repeat


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

tx_char_sync:
    pha
@wait:
    lda A_STS              ; get status byte
    and #$10               ; mask transmit buffer status flag
    beq @wait              ; loop if tx buffer full

    pla                    ; restore A
    sta A_TXD              ; save byte to ACIA data port
    rts

rx_char_sync:
    lda A_STS              ; get status byte
    and #$08               ; max rx buffer status flag
    beq rx_char_sync       ; loop if rx buffer is empty   
    lda A_RXD              ; get byte from ACIA
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

