; FAT32/SD interface library
;
; This module requires some RAM workspace to be defined elsewhere:
; 
; fat32_workspace    - a large page-aligned 512-byte workspace
; zp_fat32_variables - 24 bytes of zero-page storage for variables etc


zp_fat32_variables = $30

fat32_readbuffer   = fat32_workspace

fat32_fatstart          = zp_fat32_variables + $00  ; 4 bytes
fat32_datastart         = zp_fat32_variables + $04  ; 4 bytes
fat32_rootcluster       = zp_fat32_variables + $08  ; 4 bytes
fat32_sectorspercluster = zp_fat32_variables + $0C  ; 1 byte
fat32_pendingsectors    = zp_fat32_variables + $0D  ; 1 byte
fat32_address           = zp_fat32_variables + $0E  ; 2 bytes
fat32_nextcluster       = zp_fat32_variables + $10  ; 4 bytes
fat32_bytesremaining    = zp_fat32_variables + $14  ; 4 bytes 
zp_sd_address           = zp_fat32_variables + $18  ; 2 bytes
zp_sd_currentsector     = zp_fat32_variables + $1A  ; 4 bytes
zp_sd_cd_cluster        = zp_fat32_variables + $1E  ; 4 bytes
zp_sd_temp              = zp_fat32_variables + $22  ; 4 bytes

fat32_errorstage        = fat32_bytesremaining  ; only used during initializatio
fat32_filenamepointer   = fat32_bytesremaining  ; only used when searching for a file

FSTYPE_FAT32 = 12

.include "libsd.s"

fat32_init:
  ; Initialize the module - read the MBR etc, find the partition,
  ; and set up the variables ready for navigating the filesystem

  ; Read the MBR and extract pertinent information

  lda #0
  sta fat32_errorstage

  ; Sector 0
  lda #0
  sta zp_sd_currentsector
  sta zp_sd_currentsector+1
  sta zp_sd_currentsector+2
  sta zp_sd_currentsector+3

  ; Target buffer
  lda #<fat32_readbuffer
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta zp_sd_address+1

  ; Do the read
  jsr sd_readsector

  jsr display_message
  .byte "Checking boot sector signature", 10, 13, 0

  inc fat32_errorstage ; stage 1 = boot sector signature check

  ; Check some things
  lda fat32_readbuffer+510 ; Boot sector signature 55
  cmp #$55
  bne @fail
  lda fat32_readbuffer+511 ; Boot sector signature aa
  cmp #$aa
  bne @fail


  inc fat32_errorstage ; stage 2 = finding partition

  jsr display_message
  .byte "Finding FAT partition", 10, 13, 0

  ; Find a FAT32 partition
  ldx #0
  lda fat32_readbuffer+$1c2,x
  cmp #FSTYPE_FAT32
  beq @foundpart
  ldx #16
  lda fat32_readbuffer+$1c2,x
  cmp #FSTYPE_FAT32
  beq @foundpart
  ldx #32
  lda fat32_readbuffer+$1c2,x
  cmp #FSTYPE_FAT32
  beq @foundpart
  ldx #48
  lda fat32_readbuffer+$1c2,x
  cmp #FSTYPE_FAT32
  beq @foundpart

@fail:
  jmp @error

@foundpart:

  ; Read the FAT32 BPB
  lda fat32_readbuffer+$1c6,x
  sta zp_sd_currentsector
  lda fat32_readbuffer+$1c7,x
  sta zp_sd_currentsector+1
  lda fat32_readbuffer+$1c8,x
  sta zp_sd_currentsector+2
  lda fat32_readbuffer+$1c9,x
  sta zp_sd_currentsector+3

  jsr sd_readsector

  jsr display_message
  .byte "Reading Fat32 BPB", 10, 13, 0

  inc fat32_errorstage ; stage 3 = BPB signature check

  ; Check some things
  lda fat32_readbuffer+510 ; BPB sector signature 55
  cmp #$55
  bne @fail
  lda fat32_readbuffer+511 ; BPB sector signature aa
  cmp #$aa
  bne @fail

  jsr display_message
  .byte "Reading RootEntCnt", 10, 13, 0

  inc fat32_errorstage ; stage 4 = RootEntCnt check

  lda fat32_readbuffer+17 ; RootEntCnt should be 0 for FAT32
  ora fat32_readbuffer+18
  bne @fail

  inc fat32_errorstage ; stage 5 = TotSec16 check

  lda fat32_readbuffer+19 ; TotSec16 should be 0 for FAT32
  ora fat32_readbuffer+20
  bne @fail

  inc fat32_errorstage ; stage 6 = SectorsPerCluster check

  ; Check bytes per filesystem sector, it should be 512 for any SD card that supports FAT32
  lda fat32_readbuffer+11 ; low byte should be zero
  bne @fail
  lda fat32_readbuffer+12 ; high byte is 2 (512), 4, 8, or 16
  cmp #2
  bne @fail

  jsr display_message
  .byte "Calculating starting FAT sector", 10, 13, 0

  ; Calculate the starting sector of the FAT
  clc
  lda zp_sd_currentsector
  adc fat32_readbuffer+14    ; reserved sectors lo
  sta fat32_fatstart
  sta fat32_datastart
  lda zp_sd_currentsector+1
  adc fat32_readbuffer+15    ; reserved sectors hi
  sta fat32_fatstart+1
  sta fat32_datastart+1
  lda zp_sd_currentsector+2
  adc #0
  sta fat32_fatstart+2
  sta fat32_datastart+2
  lda zp_sd_currentsector+3
  adc #0
  sta fat32_fatstart+3
  sta fat32_datastart+3

  ; Calculate the starting sector of the data area
  ldx fat32_readbuffer+16   ; number of FATs
@skipfatsloop:
  clc
  lda fat32_datastart
  adc fat32_readbuffer+36 ; fatsize 0
  sta fat32_datastart
  lda fat32_datastart+1
  adc fat32_readbuffer+37 ; fatsize 1
  sta fat32_datastart+1
  lda fat32_datastart+2
  adc fat32_readbuffer+38 ; fatsize 2
  sta fat32_datastart+2
  lda fat32_datastart+3
  adc fat32_readbuffer+39 ; fatsize 3
  sta fat32_datastart+3
  dex
  bne @skipfatsloop

  ; Sectors-per-cluster is a power of two from 1 to 128
  lda fat32_readbuffer+13
  sta fat32_sectorspercluster

  ; Remember the root cluster
  lda fat32_readbuffer+44
  sta fat32_rootcluster
  lda fat32_readbuffer+45
  sta fat32_rootcluster+1
  lda fat32_readbuffer+46
  sta fat32_rootcluster+2
  lda fat32_readbuffer+47
  sta fat32_rootcluster+3

  clc
  rts

@error:
  sec
  rts


fat32_seekcluster:
  ; Gets ready to read fat32_nextcluster, and advances it according to the FAT
  
  ; FAT sector = (cluster*4) / 512 = (cluster*2) / 256
  lda fat32_nextcluster
  asl
  lda fat32_nextcluster+1
  rol
  sta zp_sd_currentsector
  lda fat32_nextcluster+2
  rol
  sta zp_sd_currentsector+1
  lda fat32_nextcluster+3
  rol
  sta zp_sd_currentsector+2
  ; note: cluster numbers never have the top bit set, so no carry can occur

  ; Add FAT starting sector
  lda zp_sd_currentsector
  adc fat32_fatstart
  sta zp_sd_currentsector
  lda zp_sd_currentsector+1
  adc fat32_fatstart+1
  sta zp_sd_currentsector+1
  lda zp_sd_currentsector+2
  adc fat32_fatstart+2
  sta zp_sd_currentsector+2
  lda #0
  adc fat32_fatstart+3
  sta zp_sd_currentsector+3

  ; Target buffer
  lda #<fat32_readbuffer
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta zp_sd_address+1

  ; Read the sector from the FAT
  jsr sd_readsector

  ; Before using this FAT data, set currentsector ready to read the cluster itself
  ; We need to multiply the cluster number minus two by the number of sectors per 
  ; cluster, then add the data region start sector

  ; Subtract two from cluster number
  sec
  lda fat32_nextcluster
  sbc #2
  sta zp_sd_currentsector
  lda fat32_nextcluster+1
  sbc #0
  sta zp_sd_currentsector+1
  lda fat32_nextcluster+2
  sbc #0
  sta zp_sd_currentsector+2
  lda fat32_nextcluster+3
  sbc #0
  sta zp_sd_currentsector+3
  
  ; Multiply by sectors-per-cluster which is a power of two between 1 and 128
  lda fat32_sectorspercluster
@spcshiftloop:
  lsr
  bcs @spcshiftloopdone
  asl zp_sd_currentsector
  rol zp_sd_currentsector+1
  rol zp_sd_currentsector+2
  rol zp_sd_currentsector+3
  jmp @spcshiftloop
@spcshiftloopdone:

  ; Add the data region start sector
  clc
  lda zp_sd_currentsector
  adc fat32_datastart
  sta zp_sd_currentsector
  lda zp_sd_currentsector+1
  adc fat32_datastart+1
  sta zp_sd_currentsector+1
  lda zp_sd_currentsector+2
  adc fat32_datastart+2
  sta zp_sd_currentsector+2
  lda zp_sd_currentsector+3
  adc fat32_datastart+3
  sta zp_sd_currentsector+3

  ; That's now ready for later code to read this sector in - tell it how many consecutive
  ; sectors it can now read
  lda fat32_sectorspercluster
  sta fat32_pendingsectors

  ; Now go back to looking up the next cluster in the chain
  ; Find the offset to this cluster's entry in the FAT sector we loaded earlier

  ; Offset = (cluster*4) & 511 = (cluster & 127) * 4
  lda fat32_nextcluster
  and #$7f
  asl
  asl
  tay ; Y = low byte of offset

  ; Add the potentially carried bit to the high byte of the address
  lda zp_sd_address+1
  adc #0
  sta zp_sd_address+1

  ; Copy out the next cluster in the chain for later use
  lda (zp_sd_address),y
  sta fat32_nextcluster
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+1
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+2
  iny
  lda (zp_sd_address),y
  and #$0f
  sta fat32_nextcluster+3

  ; See if it's the end of the chain
  ora #$f0
  and fat32_nextcluster+2
  and fat32_nextcluster+1
  cmp #$ff
  bne @notendofchain
  lda fat32_nextcluster
  cmp #$f8
  bcc @notendofchain

  ; It's the end of the chain, set the top bits so that we can tell this later on
  sta fat32_nextcluster+3
@notendofchain:

  rts


fat32_readnextsector:
  ; Reads the next sector from a cluster chain into the buffer at fat32_address.
  ;
  ; Advances the current sector ready for the next read and looks up the next cluster
  ; in the chain when necessary.
  ;
  ; On return, carry is clear if data was read, or set if the cluster chain has ended.

  ; Maybe there are pending sectors in the current cluster
  lda fat32_pendingsectors
  bne @readsector

  ; No pending sectors, check for end of cluster chain
  lda fat32_nextcluster+3
  bmi @endofchain

  ; Prepare to read the next cluster
  jsr fat32_seekcluster

@readsector:
  dec fat32_pendingsectors

  ; Set up target address  
  lda fat32_address
  sta zp_sd_address
  lda fat32_address+1
  sta zp_sd_address+1

  ; Read the sector
  jsr sd_readsector

  ; Advance to next sector
  inc zp_sd_currentsector
  bne @sectorincrementdone
  inc zp_sd_currentsector+1
  bne @sectorincrementdone
  inc zp_sd_currentsector+2
  bne @sectorincrementdone
  inc zp_sd_currentsector+3
@sectorincrementdone:

  ; Success - clear carry and return
  clc
  rts

@endofchain:
  ; End of chain - set carry and return
  sec
  rts


fat32_openroot:
  ; Prepare to read the root directory


  lda fat32_rootcluster
  sta fat32_nextcluster
  sta zp_sd_cd_cluster

  lda fat32_rootcluster+1
  sta fat32_nextcluster+1
  sta zp_sd_cd_cluster+1

  lda fat32_rootcluster+2
  sta fat32_nextcluster+2
  sta zp_sd_cd_cluster+2

  lda fat32_rootcluster+3
  sta fat32_nextcluster+3
  sta zp_sd_cd_cluster+3

  jsr fat32_seekcluster

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1

  rts

fat32_open_cd:
  ; Prepare to read from a file or directory based on a dirent
  ;

  pha
  phx
  phy

  ; Seek to first cluster of current directory
  lda zp_sd_cd_cluster
  sta fat32_nextcluster
  lda zp_sd_cd_cluster+1
  sta fat32_nextcluster+1
  lda zp_sd_cd_cluster+2
  sta fat32_nextcluster+2
  lda zp_sd_cd_cluster+3
  sta fat32_nextcluster+3

  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1

  jsr fat32_seekcluster
  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1
  
  ply
  plx
  pla


  rts


fat32_opendirent:
  ; Prepare to read from a file or directory based on a dirent
  ;
  ; Point zp_sd_address at the dirent

  ; Remember file size in bytes remaining
  ldy #28
  lda (zp_sd_address),y
  sta fat32_bytesremaining
  iny
  lda (zp_sd_address),y
  sta fat32_bytesremaining+1
  iny
  lda (zp_sd_address),y
  sta fat32_bytesremaining+2
  iny
  lda (zp_sd_address),y
  sta fat32_bytesremaining+3

  ; Seek to first cluster
  ldy #26
  lda (zp_sd_address),y
  sta fat32_nextcluster
  sta zp_sd_cd_cluster
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+1
  sta zp_sd_cd_cluster+1
  ldy #20
  lda (zp_sd_address),y
  sta fat32_nextcluster+2
  sta zp_sd_cd_cluster+2
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+3
  sta zp_sd_cd_cluster+3

  ; if we're opening a directory entry with 0 cluster, use the root cluster
  lda fat32_nextcluster+3
  bne @seek
  lda fat32_nextcluster+2
  bne @seek
  lda fat32_nextcluster+1
  bne @seek
  lda fat32_nextcluster
  bne @seek
  lda fat32_rootcluster
  sta fat32_nextcluster
  sta zp_sd_cd_cluster

@seek:
  jsr fat32_seekcluster

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1

  rts


fat32_readdirent:
  ; Read a directory entry from the open directory
  ;
  ; On exit the carry is set if there were no more directory entries.
  ;
  ; Otherwise, A is set to the file's attribute byte and
  ; zp_sd_address points at the returned directory entry.
  ; LFNs and empty entries are ignored automatically.

  ; Increment pointer by 32 to point to next entry
  clc
  lda zp_sd_address
  adc #32
  sta zp_sd_address
  lda zp_sd_address+1
  adc #0
  sta zp_sd_address+1

  ; If it's not at the end of the buffer, we have data already
  cmp #>(fat32_readbuffer+$200)
  bcc @gotdata

  ; Read another sector
  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1

  jsr fat32_readnextsector
  bcc @gotdata

@endofdirectory:
  sec
  rts

@gotdata:
  ; Check first character
  ldy #0
  lda (zp_sd_address),y

  ; End of directory => abort
  beq @endofdirectory

  ; Empty entry => start again
  cmp #$e5
  beq fat32_readdirent

  ; Check attributes
  ldy #11
  lda (zp_sd_address),y
  and #$3f
  cmp #$0f ; LFN => start again
  beq fat32_readdirent

  ; Yield this result
  clc
  rts



;
; prepare temps for filename string evaluations
;
fat32_init_filepointers:
  ; setup pointer to filename operand
  stx fat32_filenamepointer
  sty fat32_filenamepointer+1
  stx zp_sd_temp
  sty zp_sd_temp+1

  ; setup pointer to fat directory entry
  lda zp_sd_address
  sta zp_sd_temp+2
  lda zp_sd_address + 1
  sta zp_sd_temp+3

  rts

; walk through fat directory entry and file operand
; exact match?

fat32_is_exact_match:
  ldy #$0A
@comparenameloop:
  lda (zp_sd_address),y
  cmp (fat32_filenamepointer),y
  bne @nomatch
  dey
  bpl @comparenameloop

@match:
  clc 
  rts

@nomatch:
  sec
  rts


; takes a command line parameter for filename and compares to fat directory entry
; should support * and ? characters.  Should support embedded '.' for file extension
; zp_sd_address points to the filename in the FAT dirent
; x,y point to low and high word of address with command line parameter
; . and .. are special
; carry set means it doesn't match, carry clear means it matches

fat32_evaluate_filename:
  jsr fat32_init_filepointers

; walk through the fat directory entry and move all $00 to $20
  ldy #$A
@normalize_fat:
  lda (zp_sd_address),y
  bne @next
  lda #$20
  sta (zp_sd_address),y  ; change the #$20 to #$00  
@next:
  dey
  bpl @normalize_fat

; walk through command line parameter and move all $00 to $20
  ldy #$A
@normalize_param:
  lda (fat32_filenamepointer),y
  bne @next2
  lda #$20
  sta (fat32_filenamepointer),y  ; change the #$20 to #$00  
@next2:
  dey
  bpl @normalize_param

; check for an exact match 
  jsr fat32_is_exact_match
  bcc @match

  ldy #$00
@compare_filename:
  lda (zp_sd_temp)
  cmp #'*'
  beq @skip2
  cmp #'.'
  beq @period
  cmp #'?'
  beq @nextchar
  cmp (zp_sd_temp+2)
  bne @nomatch
@nextchar:
  inc zp_sd_temp   ; increment operand pointer
  inc zp_sd_temp+2 ; increment fat entry pointer
  iny
  cpy #$08
  bne @compare_filename
  beq @skip2

@period:
  ; period in operand, matches only if dirent filename is all #$20 up to extension
  lda (zp_sd_temp+2)
  cmp #$20
  bne @nomatch
  inc zp_sd_temp+2
  iny
  cpy #$08
  bne @period
  inc zp_sd_temp  ; skip the period in the operand
  beq @skip2

@skip2:
  ; reset pointer to dirent filename
  lda zp_sd_address
  clc
  adc #$8
  sta zp_sd_temp+2

  ; reset pointer to the operand  
  ldy #9
  lda fat32_filenamepointer
  sta zp_sd_temp
@seekdot:
  dey
  beq @compare_extension
  inc zp_sd_temp
  lda (zp_sd_temp)
  cmp #'.'
  bne @seekdot
  inc zp_sd_temp ; step past the dot

@compare_extension:
  ldy #$2
@extension_loop:
  lda (zp_sd_temp)
  cmp #'*'
  beq @match
  cmp (zp_sd_temp+2)
  bne @nomatch
  inc zp_sd_temp
  inc zp_sd_temp+2
  dey
  bpl @extension_loop

@match:
  clc
  rts

@nomatch:
  sec
  rts


fat32_finddirent:
  ; Finds a particular directory entry.  X,Y point to the 11-character filename to seek.
  ; The directory should already be open for iteration.
  jsr fat32_init_filepointers

  ; Iterate until name is found or end of directory
@direntloop:
  jsr fat32_readdirent
  bcc @comparename
  rts ; with carry set

; walk through filename and paramter to see if they are exact match
@comparename:
  jsr fat32_evaluate_filename
  bcs @direntloop ; no match

  ; Found it
  clc
  rts


fat32_finddirent_directory:
  ; Finds a particular directory entry.  X,Y point to the 11-character filename to seek.
  ; The directory should already be open for iteration.
  jsr fat32_init_filepointers

  ; Iterate until name is found or end of directory
@direntloop:
  jsr fat32_readdirent
  and #$10                ; is it a directory?
  bne @direntloop
  bcc @comparename
  rts ; with carry set

@comparename:
  jsr fat32_evaluate_filename
  bcs @direntloop ; no match

@skip: 
  ; Found it
  clc
  rts


fat32_file_readbyte:
  ; Read a byte from an open file
  ;
  ; The byte is returned in A with C clear; or if end-of-file was reached, C is set instead

  sec

  ; Is there any data to read at all?
  lda fat32_bytesremaining
  ora fat32_bytesremaining+1
  ora fat32_bytesremaining+2
  ora fat32_bytesremaining+3
  beq @rts

  ; Decrement the remaining byte count
  lda fat32_bytesremaining
  sbc #1
  sta fat32_bytesremaining
  lda fat32_bytesremaining+1
  sbc #0
  sta fat32_bytesremaining+1
  lda fat32_bytesremaining+2
  sbc #0
  sta fat32_bytesremaining+2
  lda fat32_bytesremaining+3
  sbc #0
  sta fat32_bytesremaining+3
  
  ; Need to read a new sector?
  lda zp_sd_address+1
  cmp #>(fat32_readbuffer+$200)
  bcc @gotdata

  ; Read another sector
  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1

  jsr fat32_readnextsector
  bcs @rts                    ; this shouldn't happen

@gotdata:
  ldy #0
  lda (zp_sd_address),y

  inc zp_sd_address
  bne @rts
  inc zp_sd_address+1
  bne @rts
  inc zp_sd_address+2
  bne @rts
  inc zp_sd_address+3

@rts:
  rts


fat32_file_read:
  ; Read a whole file into memory.  It's assumed the file has just been opened 
  ; and no data has been read yet.
  ;
  ; Also we read whole sectors, so data in the target region beyond the end of the 
  ; file may get overwritten, up to the next 512-byte boundary.
  ;
  ; And we don't properly support 64k+ files, as it's unnecessary complication given
  ; the 6502's small address space

  ; Round the size up to the next whole sector
  lda fat32_bytesremaining
  cmp #1                      ; set carry if bottom 8 bits not zero
  lda fat32_bytesremaining+1
  adc #0                      ; add carry, if any
  lsr                         ; divide by 2
  adc #0                      ; round up

  ; No data?
  beq @done

  ; Store sector count - not a byte count any more
  sta fat32_bytesremaining

  ; Read entire sectors to the user-supplied buffer
@wholesectorreadloop:
  ; Read a sector to fat32_address
  jsr fat32_readnextsector

  ; Advance fat32_address by 512 bytes
  lda fat32_address+1
  adc #2                      ; carry already clear
  sta fat32_address+1

  ldx fat32_bytesremaining    ; note - actually loads sectors remaining
  dex
  stx fat32_bytesremaining    ; note - actually stores sectors remaining

  bne @wholesectorreadloop

@done:
  rts

fat32_start:
  jsr display_message
  .byte 10,13, "Initializing SD card", 10, 13, 0

  jsr sd_init
  jsr fat32_init
  
  bcc @sdinitsuccess

  jsr display_message
  .byte "SD card init failed", 10,13,0

  lda fat32_errorstage
  jsr print_hex
  rts

@sdinitsuccess:
  jsr display_message
  .byte "SD card intialization succeeded", 10,13,0
  jsr fat32_openroot
  rts

; parse a FAT32 directory entry and output
fat32_dir:
  pha
  phx
  phy
  
@start:
  jsr fat32_readdirent
  bcs @done

  ; structure of a directory entry - pointed to by zp_sd_address
  ; $00 11 bytes filename
  ; $0B 1 byte attributes
  ;      $01: read only
  ;      $02: hidden
  ;      $04: system
  ;      $08: volume label
  ;      $10: directory
  ;      $20: archive
  ; $0C 1 byte reserved
  ; $0D 5 bytes creation time
  ; $12 2 bytes access date
  ; $14 2 bytes high-order bytes of first cluster address (always 0 in FAT 16, only used in FAT32)
  ; $16 4 bytes written date time
  ; $1A 2 bytes low-order bytes of the first cluster address
  ; $1C 4 bytes file size ( 0 for directories)
  
  ldy #$00

  ; A contains attributes
  and #$10
  tax           ; stash file attributes in x
  beq @notadir
  jsr display_message
  .byte "<DIR> ", 0

  bra @loopfilename
@notadir:
  jsr display_message
  .byte "      ", 0

; WRITE THE FILENAME
@loopfilename:
  lda (zp_sd_address), y
  jsr print_char
  cpy #$7
  bne @nodot
  jsr print_space
@nodot:
  iny
  cpy #$B
  bne @loopfilename


; WRITE OUT THE FILE SIZE
  jsr display_message
  .byte " $", 0
  ldy #$1F
  @filesizeloop:
  lda (zp_sd_address), y
  jsr print_hex
  dey
  cpy #$1B
  bne @filesizeloop

  ; WRITE OUT THE FIRST CLUSTER
  jsr display_message
  .byte " $", 0
  
  ; get low byte
  ldy #$15 
  lda (zp_sd_address),y
  jsr print_hex
  dey
  lda (zp_sd_address),y
  jsr print_hex

  ; get high byte
  ldy #$1B
  lda (zp_sd_address),y
  jsr print_hex
  dey
  lda (zp_sd_address),y
  jsr print_hex


  jsr print_crlf

  bra @start
  
@done:

  jsr fat32_open_cd

  ply
  plx
  pla
  rts


;;dump_file:
  ; Open root directory
;  jsr fat32_openroot

;  ; Find file by name
;  ldx #<fat32_filename
;  ldy #>fat32_filename
;  jsr fat32_finddirent
;  bcc @foundfile

;  ; File not found:
;  jsr display_message
;  .byte 10,13,"File Not Found", 10, 13, 0
;  rts

;@foundfile:
 
;   jsr display_message
;  .byte 10,13,"File found opening", 10, 13, 0

;  ; Open file
;  jsr fat32_opendirent

;  ; Read file contents into buffer
;  lda #<$400 ; screen ram
;  sta fat32_address
;  lda #>$400
;  sta fat32_address+1

;  jsr fat32_file_read

;rts

; open file and read contents into RAM
; reuse the RamDisk variables
; fat32_filename      = $BF0

fat32_load_file:
  ; Open root directory
  jsr fat32_openroot

  ; Find file by name
  ldx #<fat32_filename
  ldy #>fat32_filename
  jsr fat32_finddirent
  bcc @foundfile

  ; File not found:
  jsr display_message
  .byte 10,13,"File Not Found", 10, 13, 0
  rts

@foundfile:
 
   jsr display_message
  .byte 10,13,"File found opening", 10, 13, 0

  ; Open file
  jsr fat32_opendirent

  ; address to write to in fat32_address and fat32_address+1
  ; Read file contents into buffer
  jsr fat32_file_read

rts

