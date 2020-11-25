; memory map
; 0x0000 - 0x7FFF  RAM (32KB)
; 0x8000 - 0x8FFF  Devices (4KB) 16 devices, 256 bytes each, D1 = 0x8000-0x80FF, D2= 0x8100-0x81FF ...
; 0x9000 - 0xFFFF  ROM (28KB)

.segment "CODE"

;VIA registers
PORTB   = $8000
PORTA   = $8001
DDRB    = $8002
DDRA    = $8003
IFR     = $800D 
IER     = $800E     ; interrupt enable register

;LCD bits
E     = %10000000
RW    = %01000000
RS    = %00100000

; IFR clear bits
ICLR  = %01111111  ; clear CA2 interrupt

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

; state definitions
START   = $00
KEYS    = $01
PARITY  = $02
STOP    = $03

    .org $1000
irq:
    pha
    phx
    phy

    lda PORTA
    ror
    ror       ; rotate into high order bit
    and #$80

    ldx KBSTATE
    cpx #START
    beq @start 
    
    cpx #KEYS
    beq @keys
  
    cpx #PARITY
    beq @parity

    cpx #STOP
    beq @stop
    ; should never get here
    jmp @exit
    
@start:
    ; should be zero - maybe check later
    lda #KEYS
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
    lda #PARITY
    sta KBSTATE
    jmp @exit

@parity:
    ; should probably check the parity bit - all 1 data bits + parity bit should be odd #
    lda #STOP
    sta KBSTATE
    inc KBDBG
    jmp @exit

@stop:
    ; write our temp kb to kbbuf
    inc KBDBG
    lda #START
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
    lda ps2_ascii_shifted, x
    tax
    lda #$00
    sta KEYSTATE,x
    ; clear flags
    sta KBEXTEND
    sta KBKEYUP
    jmp @exit

@setkeystate:          ; set the key state - this is key down path
    ldx KBTEMP
    lda ps2_ascii_shifted, x
    tax
    lda #$01
    and KBEXTEND
    sta KEYSTATE, x

    ; check shift state  (0x12=LSHIFT  0x59=RSHIFT)
    ldx KBTEMP         ; store in buffer only if it's a key down for now
    
    cpx #$012          ; left shfit
    beq @nonprint
    cpx #$059          ; right shift
    beq @nonprint
    cpx #$11           ; alt
    beq @nonprint
    cpx #$14           ; ctrl
    beq @nonprint 
    cpx #$58           ; caps lock
    beq @nonprint

    lda ps2_ascii_shifted, x
    tax
    lda KEYSTATE, x
    cmp #$012 ; left shift
    beq @shifted
    cmp #$059 ; right shift
    beq @shifted

    ldx KBTEMP
    lda ps2_ascii_shifted, x
    ldx KBCURR
    sta KBBUF, x
    inc KBCURR
    jmp @exit    
    
@shifted:
    ldx KBTEMP
    lda ps2_ascii, x    
    ldx KBCURR
    sta KBBUF, x
    inc KBCURR
    jmp @exit

@nonprint:
@exit:

    lda ICLR
    sta IFR   ; vlear all interrupts from VIA

    ply
    plx
    pla

    rti

ps2_ascii:
  ;     0123456789ABCDEF
  .byte "??????????????`?" ; 0
  .byte "?????q1???zsaw2?" ; 1
  .byte "?cxde43?? vftr5?" ; 2
  .byte "?nbhgy6???mju78?" ; 3
  .byte "?,kio09??./l;p-?" ; 4
  .byte "??'?[=?????]?\??" ; 5

ps2_ascii_shifted:
  ;      0123456789ABCDEF
  .byte "??????????????`?" ; 0
  .byte "?????Q!???ZSAW@?" ; 1
  .byte "?CXDE$#?? VFTR%?" ; 2
  .byte "?NBHGY^???MJU&*?" ; 3
  .byte "?<KIO)(??>?L:P_?" ; 4
  .byte "??", $22, "?{+?????}?|??" ; 5


