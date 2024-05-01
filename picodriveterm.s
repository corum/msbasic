.segment "BANKROM"

PD          = $C700

picodriveterm:
    jsr cls

    LDA #$0
@output: 
    CMP #$3                  ; ctrl-c pressed?
    bne @doread
    jmp MON
@doread:
    LDA PD                   ; read a byte from the picodrive console
    beq @input               ; nothing to read? jump to output
    jsr display_apple_char   ; output to the console
    jmp @output              ; another?
@input:
    LDA $C000
    bpl @output
    sta PD                   ; write the byte to the picodrive console
    jsr display_apple_char   ; echo to local console
    LDA $C010                ; kbd strobe
    jmp @output
    rts