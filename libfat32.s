; FAT32/SD interface library
;
; This module requires some RAM workspace to be defined elsewhere:
; 
; fat32_workspace    - a large page-aligned 512-byte workspace
; zp_fat32_variables - 24 bytes of zero-page storage for variables etc

; Short Filename Dates and Times
; • Date Fields (2 byte field size)
; • Date is relative to the MS-DOS epoch of 1/1/1980
; • Bits 15-9: Count of years from 1980, valid value range 0-127 inclusive (represents 1980-2107)
; • Bits 8-5: Month of year, 1 = January, valid value range 1-12 inclusive
; • Bits 4-0: Day of month, valid value range 1-31 inclusive
; • Time Fields (2 byte field size)
; • Bits 15-11: Hours, valid value range 0-23 inclusive
; • Bits 10-5: Minutes, valid value range 0-59 inclusive
; • Bits 4-0: 2-second count, valid value range 0–29 inclusive (0-58 seconds)
; • TimeHundth Field (1 byte field size)
; • Hundredths of a second, valid value range 0-199 inclusive
; • Because the seconds portion of the Time field above denotes a time with a granularity of 2 
; seconds, this field may increment the number of seconds in addition to supplying the 
; number of hundredths of a second

; • The attributes of the file, DIR_Attr, byte consists of:
; • #define DIR_ENTRY_ATTR_READ_ONLY 0x01
; • #define DIR_ENTRY_ATTR_HIDDEN 0x02
; • #define DIR_ENTRY_ATTR_SYSTEM 0x04
; • #define DIR_ENTRY_ATTR_VOLUME_ID 0x08
; • DIR_ENTRY_ATTR_VOLUME_ID is set only in the root directory
; • #define DIR_ENTRY_ATTR_DIRECTORY 0x10
; • #define DIR_ENTRY_ATTR_ARCHIVE 0x20
; • A short filename entry attribute cannot have the DIR_ENTRY_ATTR_READ_ONLY, 
; DIR_ENTRY_ATTR_HIDDEN, DIR_ENTRY_ATTR_SYSTEM , and 
; DIR_ENTRY_ATTR_VOLUME_ID bits all asserted; this configuration is reserved to identify 
; a long filename entry and is referred to as DIR_ENTRY_ATTR_LONG_NAME
; • First apply the DIR_ENTRY_ATTR_LONG_NAME_MASK to DIR_Attr byte before comparing to 
; DIR_ENTRY_ATTR_LONG_NAME

zp_fat32_variables      = $10

fat32_readbuffer = fat32_workspace
zp_sd_currentsector             = zp_fat32_variables + $00  ; 4 bytes 
zp_sd_temp                      = zp_fat32_variables + $04  ; 4 bytes
zp_sd_address                   = zp_fat32_variables + $08  ; 2 bytes
fat32_filenamepointer           = zp_fat32_variables + $0A  ; 2 bytes

fat32_fatstart                  = fat32_variables + $00  ; 4 bytes
fat32_datastart                 = fat32_variables + $04  ; 4 bytes
fat32_rootcluster               = fat32_variables + $08  ; 4 bytes
fat32_nextcluster               = fat32_variables + $0C  ; 4 bytes
fat32_bytesremaining            = fat32_variables + $10  ; 4 bytes           
fat32_lastsector                = fat32_variables + $14  ; 4 bytes
fat32_filecluster               = fat32_variables + $18  ; 4 bytes
fat32_sectorsperfat             = fat32_variables + $1C  ; 4 bytes
zp_sd_cd_cluster                = fat32_variables + $20  ; 4 bytes
fat32_lastfoundfreecluster      = fat32_variables + $24  ; 4 bytes
fat32_lastcluster               = fat32_variables + $28  ; 4 bytes
fat32_prevcluster               = fat32_variables + $2C  ; 4 bytes
fat32_address                   = fat32_variables + $30  ; 2 bytes
fat32_sectorspercluster         = fat32_variables + $32  ; 1 byte
fat32_pendingsectors            = fat32_variables + $33  ; 1 byte
fat32_numfats                   = fat32_variables + $34  ; 1 byte

fat32_errorstage                = fat32_bytesremaining  ; only used during initialization


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
  sta fat32_address
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta fat32_address+1
  sta zp_sd_address+1

  ; Do the read
  jsr sd_readsector

  inc fat32_errorstage ; stage 1 = boot sector signature check

  ; Check some things
  lda fat32_readbuffer+510 ; Boot sector signature 55
  cmp #$55
  bne @fail
  lda fat32_readbuffer+511 ; Boot sector signature aa
  cmp #$aa
  bne @fail


  inc fat32_errorstage ; stage 2 = finding partition

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


  inc fat32_errorstage ; stage 3 = BPB signature check

  ; Check some things
  lda fat32_readbuffer+510 ; BPB sector signature 55
  cmp #$55
  bne @fail
  lda fat32_readbuffer+511 ; BPB sector signature aa
  cmp #$aa
  bne @fail

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
  stx fat32_numfats         ; (stash for later as well)
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

  ; Save Sectors Per FAT
  lda fat32_readbuffer+36
  sta fat32_sectorsperfat
  lda fat32_readbuffer+37
  sta fat32_sectorsperfat+1
  lda fat32_readbuffer+38
  sta fat32_sectorsperfat+2
  lda fat32_readbuffer+39
  sta fat32_sectorsperfat+3

  ; Set the last found free cluster to 0.
  lda #0
  sta fat32_lastfoundfreecluster
  sta fat32_lastfoundfreecluster+1
  sta fat32_lastfoundfreecluster+2
  sta fat32_lastfoundfreecluster+3

  ; As well as the last read clusters and sectors
  sta fat32_lastcluster
  sta fat32_lastcluster+1
  sta fat32_lastcluster+2
  sta fat32_lastcluster+3
  sta fat32_lastsector
  sta fat32_lastsector+1
  sta fat32_lastsector+2
  sta fat32_lastsector+3

  clc
  rts

@error:
  sec
  rts


fat32_seekcluster:
  ; Calculates the FAT sector given fat32_nextcluster and stores in zp_sd_currentsector
  ; Optionally will load the 512 byte FAT sector into memory at fat32_readbuffer
  ; If carry is set, subroutine is optimized to skip the loading if the expected
  ; sector is already loaded. Clearing carry before calling will skip optimization
  ; and force reload of the FAT sector.  Once the FAT sector is loaded, 
  ; the next cluster in the chain is loaded into fat32_nextcluster and 
  ; zp_sd_currentsector is updated to point to the referenced data sector
  ; routine leaves Y pointing to the LSB for the 32 bit next cluster

  ; Gets ready to read fat32_nextcluster, and advances it according to the FAT
  ; Before calling, set carry to compare the current FAT sector with lastsector.
  ; Otherwize, clear carry to force reading the FAT.

  clc
    
  php

  ; Target buffer
  lda #<fat32_readbuffer
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta zp_sd_address+1

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

  ; Branch if we don't need to check
  plp
  bcc @newsector

  ; Check if this sector is the same as the last one
  lda fat32_lastsector
  cmp zp_sd_currentsector
  bne @newsector
  lda fat32_lastsector+1
  cmp zp_sd_currentsector+1
  bne @newsector
  lda fat32_lastsector+2
  cmp zp_sd_currentsector+2
  bne @newsector
  lda fat32_lastsector+3
  cmp zp_sd_currentsector+3
  beq @notnew

@newsector:

  ; Read the sector from the FAT
  jsr sd_readsector

  ; Update fat32_lastsector

  lda zp_sd_currentsector
  sta fat32_lastsector
  lda zp_sd_currentsector+1
  sta fat32_lastsector+1
  lda zp_sd_currentsector+2
  sta fat32_lastsector+2
  lda zp_sd_currentsector+3
  sta fat32_lastsector+3

@notnew:

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

  phy ; stash the index to next value for the cluster

  ; Store the previous cluster
  lda fat32_nextcluster
  sta fat32_prevcluster
  lda fat32_nextcluster+1
  sta fat32_prevcluster+1
  lda fat32_nextcluster+2
  sta fat32_prevcluster+2
  lda fat32_nextcluster+3
  sta fat32_prevcluster+3

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

  ply ; restore index to the table entry for the cluster

  ;jsr fat32_dump_clusterinfo

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
  sec
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


fat32_writenextsector:
  ; Writes the next sector from the buffer at fat32_address.
  ; On return, carry is set if its the end of the chain. 

  ; Maybe there are pending sectors in the current cluster
  lda fat32_pendingsectors
  bne @wr

  ; No pending sectors, check for end of cluster chain
  lda fat32_nextcluster+3
  bmi @endofchain

  ; Prepare to read the next cluster
  sec
  jsr fat32_seekcluster

@wr:
  jsr writesector

  ; Success - clear carry and return
  clc
  rts

@endofchain:
  ; End of chain - set carry, write a sector, and return
  jsr writesector
  sec
  rts

writesector:
  dec fat32_pendingsectors

  ; Set up target address
  lda fat32_address
  sta zp_sd_address
  lda fat32_address+1
  sta zp_sd_address+1

  ; Write the sector
  jsr sd_writesector

  ; Advance to next sector
  inc zp_sd_currentsector
  bne @nextsectorincrementdone
  inc zp_sd_currentsector+1
  bne @nextsectorincrementdone
  inc zp_sd_currentsector+2
  bne @nextsectorincrementdone
  inc zp_sd_currentsector+3
@nextsectorincrementdone:
  rts

fat32_updatefat:
 ; Preserve the current sector
  lda zp_sd_currentsector
  pha 
  lda zp_sd_currentsector+1
  pha 
  lda zp_sd_currentsector+2
  pha 
  lda zp_sd_currentsector+3
  pha

  ; Write FAT sector
  lda fat32_lastsector
  sta zp_sd_currentsector
  lda fat32_lastsector+1
  sta zp_sd_currentsector+1
  lda fat32_lastsector+2
  sta zp_sd_currentsector+2
  lda fat32_lastsector+3
  sta zp_sd_currentsector+3

  ; Target buffer
  lda #<fat32_readbuffer
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta zp_sd_address+1

  jsr display_message
  .byte 10,13,"updating FAT #1",0

  jsr fat32_dump_diskstats
  
  ; Write the FAT sector
  jsr sd_writesector

  ; Check if FAT mirroring is enabled
  lda fat32_numfats
  cmp #2
  bne @onefat

  ; Add the last sector to the amount of sectors per FAT
  ; (to get the second fat location)
  clc
  lda fat32_lastsector
  adc fat32_sectorsperfat
  sta zp_sd_currentsector
  lda fat32_lastsector+1
  adc fat32_sectorsperfat+1
  sta zp_sd_currentsector+1
  lda fat32_lastsector+2
  adc fat32_sectorsperfat+2
  sta zp_sd_currentsector+2
  lda fat32_lastsector+3
  adc fat32_sectorsperfat+3
  sta zp_sd_currentsector+3

  jsr display_message
  .byte 10,13,"updating FAT #2",0

  jsr fat32_dump_diskstats

  ; Write the FAT sector
  jsr sd_writesector

@onefat:
  ; Pull back the current sector
  pla
  sta zp_sd_currentsector+3
  pla
  sta zp_sd_currentsector+2
  pla
  sta zp_sd_currentsector+1
  pla
  sta zp_sd_currentsector

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

  clc
  jsr fat32_seekcluster

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1

  rts

; todo: zp_sd_address points to what?
; what is expected to set the y register?
; assumption: this subroutine should seek to that cluster
; should update the next cluster to be the end of chain


fat32_allocatecluster:
  ; Allocate a cluster to start storing a file at.

  ; Find a free cluster
  jsr fat32_findnextfreecluster

  ; Cache the value so we can add the address of the next one later, if any
  lda fat32_lastfoundfreecluster
  sta fat32_lastcluster
  sta fat32_filecluster
  lda fat32_lastfoundfreecluster+1
  sta fat32_lastcluster+1
  sta fat32_filecluster+1
  lda fat32_lastfoundfreecluster+2
  sta fat32_lastcluster+2
  sta fat32_filecluster+2
  lda fat32_lastfoundfreecluster+3
  sta fat32_lastcluster+3
  sta fat32_filecluster+3

  ; Add marker for the following routines, so we don't think this is free.
  lda #$0f
  sta (zp_sd_address),y

  rts

;todo: who calls? fat32_allocatecluster depends on y register
;so this depends on y register and zp_sd_address
;assuming the fat should be loaded and y == 0

fat32_allocatefile:
  ; Allocate an entire file in the FAT, with the
  ; file's size in fat32_bytesremaining

  ; We will read a new sector the first time around
  lda #0
  sta fat32_lastsector
  sta fat32_lastsector+1
  sta fat32_lastsector+2
  sta fat32_lastsector+3

  ; BUG if we have a FAT enty at the end of a sector, it may be ignored!

  ; Allocate the first cluster.
  jsr fat32_allocatecluster

  ; We don't properly support 64k+ files, as it's unnecessary complication given
  ; the 6502's small address space. So we'll just empty out the top two bytes.
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3

  ; Stash filesize, as we will be clobbering it here
  lda fat32_bytesremaining
  pha
  lda fat32_bytesremaining+1
  pha

  ; Round the size up to the next whole sector
  lda fat32_bytesremaining
  cmp #1                      ; set carry if bottom 8 bits not zero
  lda fat32_bytesremaining+1
  adc #0                      ; add carry, if any
  lsr                         ; divide by 2
  adc #0                      ; round up

  ; No data?
  bne @nofail

  jsr display_message
  .byte 10,13,"fat32_allocatefile: unexpected, 0 clusters?",10,13,0
  
  jmp @done

@nofail:
  ; This will be clustersremaining now.
  sta fat32_bytesremaining

  ; Divide by sectors per cluster (power of 2)
  ; If it's 1, then skip
  lda fat32_sectorspercluster
  cmp #1
  beq @one 
  lda fat32_sectorspercluster
  lsr
@cl:
  lsr fat32_bytesremaining
  lsr
  bcc @cl

@one:
  ; decrement fat32_bytesremaining since we created a cluster
  dec fat32_bytesremaining

  ; We will be making a new cluster every time
  stz fat32_pendingsectors

  ; Find free clusters and allocate them for use for this file.
@allocatelp:
  ; Check if it's the last cluster in the chain 
  lda fat32_bytesremaining
  beq @lastcluster
  cmp #1                 ; CHECK! is 1 the right amound for this?
  bcc @notlastcluster    ; clustersremaining <=1?

  ; It is the last one.

@lastcluster:

; go back the previous one
  lda fat32_lastcluster
  sta fat32_nextcluster
  lda fat32_lastcluster+1
  sta fat32_nextcluster+1
  lda fat32_lastcluster+2
  sta fat32_nextcluster+2
  lda fat32_lastcluster+3
  sta fat32_nextcluster+3

  sec
  jsr fat32_seekcluster

  phy
  ; Write 0x0FFFFFFF (EOC)
  lda #$FF
  sta (zp_sd_address),y
  iny
  sta (zp_sd_address),y
  iny
  sta (zp_sd_address),y
  iny
  and #$0F
  sta (zp_sd_address),y
  ply

  ;jsr fat32_dump_clusterinfo

  ; Update the FAT
  jsr fat32_updatefat

  ; End of chain - exit
  jmp @done

@notlastcluster:
  ; Wait! Is there exactly 1 cluster left?
  beq @lastcluster

  ; Find the next cluster
  jsr fat32_findnextfreecluster

  ; Add marker so we don't think this is free.
  lda #$0f
  sta (zp_sd_address),y

  ; Seek to the previous cluster
  lda fat32_lastcluster
  sta fat32_nextcluster
  lda fat32_lastcluster+1
  sta fat32_nextcluster+1
  lda fat32_lastcluster+2
  sta fat32_nextcluster+2
  lda fat32_lastcluster+3
  sta fat32_nextcluster+3

  sec
  jsr fat32_seekcluster

  phy
  ; Enter the address of the next one into the FAT
  lda fat32_lastfoundfreecluster
  sta fat32_lastcluster
  sta (zp_sd_address),y
  iny
  lda fat32_lastfoundfreecluster+1
  sta fat32_lastcluster+1
  sta (zp_sd_address),y
  iny
  lda fat32_lastfoundfreecluster+2
  sta fat32_lastcluster+2
  sta (zp_sd_address),y
  iny
  lda fat32_lastfoundfreecluster+3
  sta fat32_lastcluster+3
  sta (zp_sd_address),y
  ply

  ;jsr fat32_dump_clusterinfo

  ; Update the FAT
  jsr fat32_updatefat

  ldx fat32_bytesremaining    ; note - actually loads clusters remaining
  dex
  stx fat32_bytesremaining    ; note - actually stores clusters remaining

  beq @done
  jmp @allocatelp

  ; Done!
@done:
  ; Pull the filesize back from the stack
  pla
  sta fat32_bytesremaining+1
  pla
  sta fat32_bytesremaining

  rts


; fat32_findnextfreecluster
; Find next free cluster
; 
; This function will search the FAT for an empty entry, and
; save the 32-bit cluster number at fat32_lastfoundfreecluter.
;
; Also sets the carry bit if the SD card is full.

fat32_findnextfreecluster:

  ; Find a free cluster and store it's location in fat32_lastfoundfreecluster

  lda #0
  sta fat32_nextcluster
  sta fat32_lastfoundfreecluster
  sta fat32_nextcluster+1
  sta fat32_lastfoundfreecluster+1
  sta fat32_nextcluster+2
  sta fat32_lastfoundfreecluster+2
  sta fat32_nextcluster+3
  sta fat32_lastfoundfreecluster+3

@searchclusters:
;  jsr fat32_dump_lastfoundfreecluster

  ; Seek cluster
  sec
  jsr fat32_seekcluster

  ; Is the cluster free?
  lda fat32_nextcluster
  and #$0f
  ora fat32_nextcluster+1
  ora fat32_nextcluster+2
  ora fat32_nextcluster+3
  beq @foundcluster

  ; No, increment the cluster count
  inc fat32_lastfoundfreecluster
  bne @copycluster
  inc fat32_lastfoundfreecluster+1
  bne @copycluster
  inc fat32_lastfoundfreecluster+2
  bne @copycluster
  inc fat32_lastfoundfreecluster+3

@copycluster:

  ; Copy the cluster count to the next cluster
  lda fat32_lastfoundfreecluster
  sta fat32_nextcluster
  lda fat32_lastfoundfreecluster+1
  sta fat32_nextcluster+1
  lda fat32_lastfoundfreecluster+2
  sta fat32_nextcluster+2
  lda fat32_lastfoundfreecluster+3
  sta fat32_nextcluster+3
  
  ; Go again for another pass
  jmp @searchclusters

@foundcluster:
  ; done.  
;  jsr fat32_dump_lastfoundfreecluster
  rts

fat32_opendirent:
  ; Prepare to read/write a file or directory based on a dirent
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
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+1
  ldy #20
  lda (zp_sd_address),y
  sta fat32_nextcluster+2
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+3

  clc
  ldy #$0B
  lda (zp_sd_address),Y
  and #$10   ; is it a directory?
  beq @skip_cd_cache
  
  ; If it's a directory, cache the cluster
  lda fat32_nextcluster
  sta zp_sd_cd_cluster
  lda fat32_nextcluster+1
  sta zp_sd_cd_cluster+1
  lda fat32_nextcluster+2
  sta zp_sd_cd_cluster+2
  lda fat32_nextcluster+3
  sta zp_sd_cd_cluster+3

@skip_cd_cache:

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
  clc
  jsr fat32_seekcluster

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1

  rts

fat32_writedirent:
  ; Write a directory entry from the open directory
  ; requires:
  ;   fat32bytesremaining (2 bytes) = file size in bytes (little endian)
  ;
  ; zp_sp_address should have a loaded directory
  ; walk through directory entries until the first available slot to write
  ; if flots are full, allocate and link a new cluster, seek there and add
  ; to new cluster.

  jsr display_message
  .byte 10,13,"fat32_writedirent",0

@nextentry:

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
  bcc @gotdirrent

  ; Read another sector
  lda #<fat32_readbuffer
  sta fat32_address
  lda #>fat32_readbuffer
  sta fat32_address+1

  jsr fat32_readnextsector
  bcc @gotdirrent

@endofdirectorywrite:
  sec
  rts

@gotdirrent:
  ; Check first character
  clc
  ldy #0
  lda (zp_sd_address),y
  bne @nextentry ; go again
  ; End of directory. Now make a new entry.
@dloop:
  lda (fat32_filenamepointer),y	; copy filename
  sta (zp_sd_address),y
  iny
  cpy #$0b
  bne @dloop
  ; The full Short filename is #11 bytes long so,
  ; this start at 0x0b - File type
  ; BUG assumes that we are making a file, not a folder...
  lda #$20		; File Type: ARCHIVE
  sta (zp_sd_address),y
  iny   ; 0x0c - Checksum/File accsess password
  lda #$10		            ; No checksum or password
  sta (zp_sd_address),y
  iny   ; 0x0d - first char of deleted file - 0x7d for nothing
  lda #$7D
  sta (zp_sd_address),y
  iny	; 0x0e-0x11 - File creation time/date
  lda #0
@empty:
  sta (zp_sd_address),y	; No time/date because I don't have an RTC
  iny
  cpy #$14 ; also empty the user ID (0x12-0x13)
  bne @empty
  ; 0x14-0x15 - File start cluster (high word)
  lda fat32_lastfoundfreecluster+2
  sta (zp_sd_address),y
  iny
  lda fat32_lastfoundfreecluster+3
  sta (zp_sd_address),y
  iny ; 0x16-0x19 - File modifiaction date
  lda #0
  sta (zp_sd_address),y
  iny
  sta (zp_sd_address),y   ; no rtc
  iny
  sta (zp_sd_address),y
  iny
  sta (zp_sd_address),y
  iny ; 0x1a-0x1b - File start cluster (low word)
  lda fat32_lastfoundfreecluster
  sta (zp_sd_address),y
  iny
  lda fat32_lastfoundfreecluster+1
  sta (zp_sd_address),y
  iny ; 0x1c-0x1f File size in bytes
  lda fat32_bytesremaining
  sta (zp_sd_address),y
  iny
  lda fat32_bytesremaining+1
  sta (zp_sd_address),y
  iny
  lda #0
  sta (zp_sd_address),y ; No bigger that 64k
  iny
  sta (zp_sd_address),y
  iny
  ; are we over the buffer?
  lda zp_sd_address+1
  cmp #>(fat32_readbuffer+$200)
  bcc @notoverbuffer
  jsr fat32_writecurrentsector ; if so, write the current sector
  jsr fat32_readnextsector  ; then read the next one.
  bcs @dfail
  ldy #0
  lda #<fat32_readbuffer
  sta zp_sd_address
  lda #>fat32_readbuffer
  sta zp_sd_address+1
@notoverbuffer:
  ; next entry is 0 (end of dir)
  lda #0
  sta (zp_sd_address),y
  ; Write the dirent.
  jsr fat32_writecurrentsector

  ; Great, lets get this ready for other code to read in.

  ; Seek to first cluster
  lda fat32_filecluster
  sta fat32_nextcluster
  lda fat32_filecluster+1
  sta fat32_nextcluster+1
  lda fat32_filecluster+2
  sta fat32_nextcluster+2
  lda fat32_filecluster+3
  sta fat32_nextcluster+3
  
  clc
  jsr fat32_seekcluster

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1

  clc
  rts

@dfail:
  ; Card Full
  sec
  rts

fat32_writecurrentsector:

  ; decrement the sector so we write the current one (not the next one)
  lda zp_sd_currentsector
  bne @skip
  dec zp_sd_currentsector+1
  bne @skip
  dec zp_sd_currentsector+2
  bne @skip
  dec zp_sd_currentsector+3

@skip:
  dec zp_sd_currentsector

@nodec:

  lda fat32_address
  sta zp_sd_address
  lda fat32_address+1
  sta zp_sd_address+1

  ; Read the sector
  jsr sd_writesector

  ; Advance to next sector
  inc zp_sd_currentsector
  bne @sectorincrementdone
  inc zp_sd_currentsector+1
  bne @sectorincrementdone
  inc zp_sd_currentsector+2
  bne @sectorincrementdone
  inc zp_sd_currentsector+3

@sectorincrementdone:
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


fat32_finddirent:
  ; Finds a particular directory entry.  X,Y point to the 11-character filename to seek.
  ; The directory should already be open for iteration.

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

fat32_markdeleted:
  ; Mark the file as deleted
  ; We need to stash the first character at index 0x0D
  ldy #$00
  lda (zp_sd_address),y
  ldy #$0d
  sta (zp_sd_address),y

  ; Now put 0xE5 at the first byte
  ldy #$00
  lda #$e5 
  sta (zp_sd_address),y

  ; Get start cluster high word
  ldy #$14
  lda (zp_sd_address),y
  sta fat32_nextcluster+2
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+3

  ; And low word
  ldy #$1a
  lda (zp_sd_address),y
  sta fat32_nextcluster
  iny
  lda (zp_sd_address),y
  sta fat32_nextcluster+1

  ; Write the dirent
  jsr fat32_writecurrentsector

  ; Done
  clc
  rts

;fat32_deletefile:
  ; Removes the open file from the SD card.
  ; The directory needs to be open and
  ; zp_sd_address pointed to the first byte of the file entry.

  ; Mark the file as "Removed"
;  jsr fat32_markdeleted

  ; We will read a new sector the first time around
;  stz fat32_lastsector
;  stz fat32_lastsector+1
;  stz fat32_lastsector+2
;  stz fat32_lastsector+3

  ; Now we need to iterate through this file's cluster chain, and remove it from the FAT.
;  ldy #0
;@chainloop:
  ; Seek to cluster
;  sec
;  jsr fat32_seekcluster

  ; Is this the end of the chain?
;  lda fat32_nextcluster+3
;  bmi @endofchain

  ; Zero it out
;  lda #0
;  sta (zp_sd_address),y
;  iny
;  sta (zp_sd_address),y
;  iny
;  sta (zp_sd_address),y
;  iny
;  sta (zp_sd_address),y

  ; Write the FAT
;  jsr fat32_updatefat

  ; And go again for another pass.
;  jmp @chainloop

;@endofchain:
  ; This is the last cluster in the chain.

  ; Just zero it out,
;  lda #0
;  sta (zp_sd_address),y
;  dey
;  sta (zp_sd_address),y
;  dey
;  sta (zp_sd_address),y
;  dey
;  sta (zp_sd_address),y

  ; Write the FAT
;  jsr fat32_updatefat

  ; And we're done!
;  clc
;  rts

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

  ; reset the read buffer
  ; Read another sector
  ;lda #<fat32_readbuffer
  ;sta fat32_address
  ;lda #>fat32_readbuffer
  ;sta fat32_address+1

  ;jsr fat32_open_cd

  rts

fat32_file_write:
  ; Write a whole file from memory.  It's assumed the dirent has just been created 
  ; and no data has been written yet.

  jsr display_message
  .byte 10,13,"fat32_file_write",0
  jsr fat32_dump_diskstats

  ; Start at the first cluster for this file
  lda fat32_filecluster
  sta fat32_lastcluster
  lda fat32_filecluster+1
  sta fat32_lastcluster+1
  lda fat32_filecluster+2
  sta fat32_lastcluster+2
  lda fat32_filecluster+3
  sta fat32_lastcluster+3

  ; Round the size up to the next whole sector
  lda fat32_bytesremaining
  cmp #1                      ; set carry if bottom 8 bits not zero
  lda fat32_bytesremaining+1
  adc #0                      ; add carry, if any
  lsr                         ; divide by 2
  adc #0                      ; round up

  ; No data?
  beq @fail

  ; Store sector count - not a byte count anymore.
  sta fat32_bytesremaining

  ; We will be making a new cluster the first time around
  stz fat32_pendingsectors

  ; Write entire sectors from the user-supplied buffer
@wholesectorwriteloop:
  ; Write a sector from fat32_address
  jsr fat32_writenextsector
  ;bcs .fail	; this shouldn't happen

  ; Advance fat32_address by 512 bytes
  lda fat32_address+1
  adc #2                      ; carry already clear
  sta fat32_address+1

  ldx fat32_bytesremaining    ; note - actually loads sectors remaining
  dex
  stx fat32_bytesremaining    ; note - actually stores sectors remaining

  bne @wholesectorwriteloop

  ; Done!
@fail:
  rts


fat32_start:
;  jsr display_message
;  .byte 10,13, "Initializing SD card", 10, 13, 0

  jsr sd_init
  jsr fat32_init
  
  bcc @sdinitsuccess

;  jsr display_message
;  .byte "SD init failed", 10,13,0

  lda fat32_errorstage
  jsr print_hex
  rts

@sdinitsuccess:
;  jsr display_message
;  .byte "SD init succeeded", 10,13,0
  
  jsr fat32_openroot
  jsr fat32_dump_diskstats
  rts

; parse a FAT32 directory entry and output
fat32_dir:
  pha
  phx
  phy
  
  jsr fat32_open_cd

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
  .byte "<D> ", 0

  bra @loopfilename
@notadir:
  jsr display_message
  .byte "    ", 0

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


; dump an opened file to the console
fat32_cat_file:
@read_loop:
  jsr fat32_file_readbyte
  bcs @eof
  jsr display_char
  bra @read_loop

@eof:
  jsr fat32_open_cd

  rts

; copy from filename to 11 bytes in form of FAT entry
fat32_prep_fileparam:
  pha
  phy
  ; fat32_filenamepointer points to the dos parameter
  ; dos_file_param points to the formatted buffer

  ldy #$A
  lda #$20
@clearfilename:
  sta dos_file_param,y
  dey
  bpl @clearfilename

@special_path_check:  
  ldy #$00
; check for '.'
  lda (fat32_filenamepointer),y 
  cmp #'.'
  bne @not_special
  sta dos_file_param,y
  iny
  lda (fat32_filenamepointer),y
  cmp #'.'
  bne @check_null             ; 2nd byte is not a period, see if it's null
  sta dos_file_param,y
  iny
@check_null:
  lda (fat32_filenamepointer),y
  beq @special

; not special directory filename, do normal evaluation
@not_special:
  ldy #$FF

@copyfilename:
  iny
  cpy #$9
  beq @copy_extension
  lda (fat32_filenamepointer),y
  beq @done                     ; if we encounter a 0 in the param, we're done
  cmp #$20                      ; if we encounter a space, we're done
  beq @done
  cmp #'.'                      ; if we encounter a dot, jump to the extension
  beq @copy_extension      
  sta dos_file_param,y          ; write to the buffer
  bra @copyfilename

@copy_extension:
  iny
  ldx #$8
@copy_extension_loop:
  ; y is the index into the dos param 
  lda (fat32_filenamepointer),y
  beq @done
  cmp #$20
  beq @done
  sta dos_file_param, x
  iny
  inx
  cpx #$B
  bne @copy_extension_loop

@done:  
@special:

  ; by the end, the filename is stored in dos_file_param in a format 
  ; helpful for dirent filename comparison

  ply
  pla
  rts


; takes a command line parameter for filename and compares to fat directory entry
; should support * and ? characters.  Should support embedded '.' for file extension
; zp_sd_address points to the filename in the FAT dirent
; x,y point to low and high word of address with command line parameter
; . and .. are special
; carry set means it doesn't match, carry clear means it matches

fat32_evaluate_filename:
; walk through the fat directory entry and move all $00 to $20
  ldy #$A
@normalize_fat:
  lda (zp_sd_address),y
  bne @next
  lda #$20
  sta (zp_sd_address),y  ; change the #$00 to #$20  
@next:
  dey
  bpl @normalize_fat

  ldy #$00
@compare_filename:
  lda dos_file_param,y
  cmp #'*'
  beq @skip2       ; match remainder of filename
  cmp #'?'
  beq @nextchar    ; automatic char match
  cmp (zp_sd_address),y
  bne @nomatch
@nextchar:
  iny
  cpy #$08
  bne @compare_filename

@skip2:
  ldy #$8  
@compare_extension:
  lda dos_file_param,y
  cmp #'*'
  beq @match
  cmp (zp_sd_address),y
  bne @nomatch
  iny
  cpy #$B
  bne @compare_extension

@match:
  clc
  rts

@nomatch:
  sec
  rts

fat32_basic_load:
  lda $28
  sta RD_BYTES_LOW
  lda $29
  sta RD_BYTES_HIGH

  ; now start writing
@write_loop:
  jsr fat32_file_readbyte
  bcs @eof
  sta (RD_BYTES_LOW)
  inc RD_BYTES_LOW
  bne @write_loop
  inc RD_BYTES_HIGH
  bra @write_loop

@eof:  
  rts

fat32_open_cd:
  ; Prepare to read from a file or directory based on a dirent
  ;

  pha
  phx
  phy

  ;jsr display_message
  ;.byte 10,13,"open_cd",0

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

  clc
  jsr fat32_seekcluster
  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta zp_sd_address+1
  
  ply
  plx
  pla
  rts

fat32_dump_fat32_address:
;  jsr display_message
;  .byte 10,13,"fat32_address:          $", 0
;  lda fat32_address+1
;  jsr print_hex
;  lda fat32_address
;  jsr print_hex

;  jsr display_message
;  .byte 10,13,"zp_sd_address:          $", 0
;  lda zp_sd_address+1
;  jsr print_hex
;  lda zp_sd_address
;  jsr print_hex
  rts

fat32_dump_diskstats:
;  pha
;  phx
;  phy

;  jsr display_message
;  .byte 10,13,"fat32_fatstart          $", 0
;  lda #<fat32_fatstart
;  sta zp_sd_temp
;  lda #>fat32_fatstart
;  sta zp_sd_temp+1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"fat32_datastart         $", 0
;  lda #<fat32_datastart
;  sta zp_sd_temp
;  lda #>fat32_datastart
;  sta zp_sd_temp+1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"fat32_rootcluster       $", 0
;  lda #<fat32_rootcluster
;  sta zp_sd_temp
;  lda #>fat32_rootcluster
;  sta zp_sd_temp+1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"fat32_sectorspercluster $", 0
;  lda fat32_sectorspercluster
;  jsr print_hex

;  jsr display_message
;  .byte 10,13,"fat32_pendingsectors    $", 0
;  lda fat32_pendingsectors
;  jsr print_hex

;  jsr fat32_dump_fat32_address

;  jsr display_message
;  .byte 10,13,"fat32_nextcluster       $", 0
;  lda #<fat32_nextcluster
;  sta zp_sd_temp
;  lda #>fat32_nextcluster
;  sta zp_sd_temp + 1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"fat32_bytesremaining    $", 0
;  lda #<fat32_bytesremaining
;  sta zp_sd_temp
;  lda #>fat32_bytesremaining
;  sta zp_sd_temp+1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"zp_sd_currentsector     $", 0
;  lda #<zp_sd_currentsector
;  sta zp_sd_temp
;  lda #>zp_sd_currentsector
;  sta zp_sd_temp + 1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"zp_sd_cd_cluster        $", 0
;  lda #<zp_sd_cd_cluster
;  sta zp_sd_temp
;  lda #>zp_sd_cd_cluster
;  sta zp_sd_temp + 1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"fat32_numfats           $", 0
;  lda fat32_numfats
;  jsr print_hex
  
;  ply
;  plx
;  pla
  rts

;fat32_dump_lastfoundfreecluster:
;  pha
;  phx
;  phy
;  jsr display_message
;  .byte 10,13,"LFC: ",0
;  lda #<fat32_lastfoundfreecluster
;  sta zp_sd_temp
;  lda #>fat32_lastfoundfreecluster
;  sta zp_sd_temp+1
;  jsr print_hex_dword
;  ply
;  plx
;  pla
;  rts

;fat32_dump_clusterinfo:
;  pha
;  phy
;  jsr display_message
;  .byte 10,13,"fat32_prevcluster: ",0

;  lda #<fat32_prevcluster
;  sta zp_sd_temp
;  lda #>fat32_prevcluster
;  sta zp_sd_temp+1
;  jsr print_hex_dword

;  jsr display_message
;  .byte 10,13,"fat table entry  : ",0

;  lda zp_sd_address
;  sta zp_sd_temp
;  lda zp_sd_address + 1
;  sta zp_sd_temp+1

;  ply
;  phy

;  tya
;  clc
;  adc zp_sd_temp
;  sta zp_sd_temp
;  lda zp_sd_temp+1
;  adc #$0
;  sta zp_sd_temp+1

;  jsr print_hex_dword
  
;  ply
;  pla
;  rts

