; SD card interface module
;
; Requires zero-page variable storage:
;   zp_sd_address - 2 bytes
;   zp_sd_currentsector - 4 bytes

sd_init:
  ; Let the SD card boot up, by pumping the clock with SD CS disabled

  ; We need to apply around 80 clock pulses with CS and MOSI high.
  ; Normally MOSI doesn't matter when CS is high, but the card is
  ; not yet is SPI mode, and in this non-SPI state it does care.

  lda #SD_CS | SD_MOSI
  ldx #160               ; toggle the clock 160 times, so 80 low-high transitions
@preinitloop:
  eor #SD_SCK
  sta PORTA
  dex
  bne @preinitloop
  

@cmd0: ; GO_IDLE_STATE - resets card to idle state, and SPI mode
  lda #<sd_cmd0_bytes
  sta zp_sd_address
  lda #>sd_cmd0_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

@cmd8: ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
  lda #<sd_cmd8_bytes
  sta zp_sd_address
  lda #>sd_cmd8_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

  ; Read 32-bit return value, but ignore it
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte
  jsr sd_readbyte

@cmd55: ; APP_CMD - required prefix for ACMD commands
  lda #<sd_cmd55_bytes
  sta zp_sd_address
  lda #>sd_cmd55_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; Expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

@cmd41: ; APP_SEND_OP_COND - send operating conditions, initialize card
  lda #<sd_cmd41_bytes
  sta zp_sd_address
  lda #>sd_cmd41_bytes
  sta zp_sd_address+1

  jsr sd_sendcommand

  ; Status response $00 means initialised
  cmp #$00
  beq @initialized

  ; Otherwise expect status response $01 (not initialized)
  cmp #$01
  bne @initfailed

  ; Not initialized yet, so wait a while then try again@
  ; This retry is important, to give the card time to initialize.

  ldx #0
  ldy #0
@delayloop:
  dey
  bne @delayloop
  dex
  bne @delayloop

  jmp @cmd55


@initialized:
  rts

@initfailed:
  rts


sd_cmd0_bytes:
  .byte $40, $00, $00, $00, $00, $95
sd_cmd8_bytes:
  .byte $48, $00, $00, $01, $aa, $87
sd_cmd55_bytes:
  .byte $77, $00, $00, $00, $00, $01
sd_cmd41_bytes:
  .byte $69, $40, $00, $00, $00, $01



sd_readbyte:
  ; Enable the card and tick the clock 8 times with MOSI high, 
  ; capturing bits from MISO and returning them

  ldx #$fe    ; Preloaded with seven ones and a zero, so we stop after eight bits

@loop:

  lda #SD_MOSI                ; enable card (CS low), set MOSI (resting state), SCK low
  sta PORTA

  lda #SD_MOSI | SD_SCK       ; toggle the clock high
  sta PORTA

  lda PORTA                   ; read next bit
  and #SD_MISO

  clc                         ; default to clearing the bottom bit
  beq @bitnotset              ; unless MISO was set
  sec                         ; in which case get ready to set the bottom bit
@bitnotset:

  txa                         ; transfer partial result from X
  rol                         ; rotate carry bit into read result, and loop bit into carry
  tax                         ; save partial result back to X
  
  bcs @loop                   ; loop if we need to read more bits

  rts


sd_writebyte:
  ; Tick the clock 8 times with descending bits on MOSI
  ; SD communication is mostly half-duplex so we ignore anything it sends back here
  ldx #8                      ; send 8 bits

@loop:
  asl                         ; shift next bit into carry
  tay                         ; save remaining bits for later

  lda #0
  bcc @sendbit                ; if carry clear, don't set MOSI for this bit
  ora #SD_MOSI

@sendbit:
  sta PORTA                   ; set MOSI (or not) first with SCK low
  eor #SD_SCK
  sta PORTA                   ; raise SCK keeping MOSI the same, to send the bit

  tya                         ; restore remaining bits to send

  dex
  bne @loop                   ; loop if there are more bits to send
  rts


sd_waitresult:
  ; Wait for the SD card to return something other than $ff
  jsr sd_readbyte
  cmp #$ff
  beq sd_waitresult
  rts


sd_sendcommand:
  ; Debug print which command is being executed

  lda #SD_MOSI           ; pull CS low to begin command
  sta PORTA

  ldy #0
  lda (zp_sd_address),y    ; command byte
  jsr sd_writebyte
  ldy #1
  lda (zp_sd_address),y    ; data 1
  jsr sd_writebyte
  ldy #2
  lda (zp_sd_address),y    ; data 2
  jsr sd_writebyte
  ldy #3
  lda (zp_sd_address),y    ; data 3
  jsr sd_writebyte
  ldy #4
  lda (zp_sd_address),y    ; data 4
  jsr sd_writebyte
  ldy #5
  lda (zp_sd_address),y    ; crc
  jsr sd_writebyte

  jsr sd_waitresult
  pha

  ; End command
  lda #SD_CS | SD_MOSI   ; set CS high again
  sta PORTA

  pla   ; restore result code
  rts


sd_readsector:
  ; Read a sector from the SD card.  A sector is 512 bytes.
  ;
  ; Parameters:
  ;    zp_sd_currentsector   32-bit sector number
  ;    zp_sd_address     address of buffer to receive data
  
  lda #SD_MOSI
  sta PORTA

  ;jsr display_message
  ;.byte 10,13,"read : ", 0

  ; Command 17, arg is sector number, crc not checked
  lda #$51                    ; CMD17 - READ_SINGLE_BLOCK
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector+3   ; sector 24:31
  ;jsr print_space
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector+2   ; sector 16:23
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector+1   ; sector 8:15
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector     ; sector 0:7
  ;jsr print_hex
  ;jsr print_space
  jsr sd_writebyte
  lda #$01                    ; crc (not checked)
  ;jsr print_hex
  jsr sd_writebyte
  ;jsr print_crlf

  ;jsr fat32_dump_fat32_address

  jsr sd_waitresult
  cmp #$00
  bne sd_fail

  ; wait for data
  jsr sd_waitresult
  cmp #$fe
  bne sd_fail

  ; Need to read 512 bytes - two pages of 256 bytes each
  jsr @readpage
  inc zp_sd_address+1
  jsr @readpage
  dec zp_sd_address+1

  ; End command
  lda #SD_CS | SD_MOSI
  sta PORTA
  
  rts

@readpage:
  ; Read 256 bytes to the address at zp_sd_address
  ldy #0
@readloop:
  jsr sd_readbyte
  sta (zp_sd_address),y
  ;jsr print_hex
  ;jsr print_space
  iny
  bne @readloop
  rts

sd_fail:
@fail:
  jsr display_message
  .byte 10,13,"SD Read Operation Failed", 10, 13, 0

@failloop:
  jmp @failloop


  sd_writesector:
  ; Write a sector to the SD card.  A sector is 512 bytes.
  ;
  ; Parameters:
  ;    zp_sd_currentsector   32-bit sector number
  ;    zp_sd_address     address of buffer to take data from
  
  lda #SD_MOSI
  sta PORTA

  ;jsr display_message
  ;.byte 10,13,"write: ", 0

  ; Command 24, arg is sector number, crc not checked
  lda #$58                    ; CMD24 - WRITE_BLOCK
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector+3   ; sector 24:31
  ;jsr print_space
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector+2   ; sector 16:23
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector+1   ; sector 8:15
  ;jsr print_hex
  jsr sd_writebyte
  lda zp_sd_currentsector     ; sector 0:7
  ;jsr print_hex
  jsr sd_writebyte
  ;jsr print_space
  lda #$01                    ; crc (not checked)
  ;jsr print_hex
  jsr sd_writebyte

  ;jsr fat32_dump_fat32_address
  
  jsr sd_waitresult
  cmp #$00
  bne sd_fail_write

  ; Send start token
  lda #$fe
  jsr sd_writebyte

  ; Need to write 512 bytes - two pages of 256 bytes each
  jsr @writepage
  inc zp_sd_address+1
  jsr @writepage
  dec zp_sd_address+1

  ; wait for data response
  jsr sd_waitresult
  and #$1f
  cmp #$05
  bne sd_fail_write

@waitidle:
  jsr sd_readbyte
  cmp #$ff
  bne @waitidle

  ; End command
  lda #SD_CS | SD_MOSI ; set cs and mosi high (disconnected)
  sta PORTA

  rts

@writepage:
  ; Write 256 bytes fom zp_sd_address
  ldy #0
@writeloop:
  tya
  pha
  lda (zp_sd_address),y
  jsr sd_writebyte
  ;jsr print_hex
  ;jsr print_space
  pla
  tay
  iny
  bne @writeloop
  rts

sd_fail_write:
@fail:

  jsr print_hex

  jsr display_message
  .byte 10,13,"SD Write Operation Failed", 10, 13, 0

@failloop:
  jmp @failloop
