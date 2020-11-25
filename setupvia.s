

;VIA addresses
PORTB  = $8000
PORTA  = $8001
DDRB   = $8002
DDRA   = $8003
SHCTL  = $800A
ACR    = $800B     ; auxiliary control register
PCR    = $800C     ; peripheral control register
IFR    = $800D  
IER    = $800E     ; interrupt enable register

;VIA config flgs
IMASK = %10000001  ; enable interrupt for CA2
CFGCA = %00000010  ; configure CA2 for negative active edge for PS/2 clock
ACRCFG = %00000011  ; enable latching


; PS/2 keyboard state
KBSTATE   = $A0
KBTEMP    = $A1
KBCURR    = $A2
KBBIT     = $A3
KBEXTEND  = $A4
KBKEYUP   = $A5
KBDBG     = $A6
KBDBG2    = $A7

KBBUF     = $100
KEYSTATE  = $200

    .org $3000

reset:
    pha
    phx

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
loopx:
    sta KEYSTATE, x
    sta KBBUF, x
    inx
    cpx #$00
    bne loopx
       
; initialize the LCD via the VIA

    lda IMASK
    sta IER

    lda CFGCA
    sta PCR        ; configure CB21for negative active edge and independent interrupt

    lda ACRCFG
    sta ACR

    plx
    pla

    jmp $FF00

