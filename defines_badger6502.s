; configuration
CONFIG_2A := 1

CONFIG_CBM_ALL := 1

CONFIG_DATAFLG := 1
CONFIG_EASTER_EGG := 1
;CONFIG_FILE := 1; support PRINT#, INPUT#, GET#, CMD
CONFIG_NO_CR := 1; terminal doesn't need explicit CRs on line ends
CONFIG_NO_LINE_EDITING := 1; support for "@", "_", BEL etc.
CONFIG_NO_READ_Y_IS_ZERO_HACK := 1
CONFIG_PEEK_SAVE_LINNUM := 1
CONFIG_SCRTCH_ORDER := 2

; zero page
ZP_START1 = $00
ZP_START2 = $0D
ZP_START3 = $03
ZP_START4 = $13

; extra/override ZP variables
CURDVC			:= $000E
TISTR			:= $008D
Z96				:= $0096
POSX			:= $00C6
TXPSV			:= LASTOP
USR				:= GORESTART ; XXX

; inputbuffer
INPUTBUFFER     := $0200

; constants
SPACE_FOR_GOSUB := $3E
STACK_TOP		:= $FA
WIDTH			:= 40
WIDTH2			:= 30

RAMSTART2		:= $0800

; magic memory locations
ENTROPY = $7000

; monitor functions
MONRDKEY        := read_char_echo
MONCOUT         := display_char
;CHKIN           := do_nothing
;CHKOUT          := do_nothing
CLALL           := do_nothing
;CHRIN           := do_nothing
;CLRCH           := do_nothing
CLOSE           := do_nothing
OPEN            := do_nothing
;SYS             := do_nothing
VERIFY          := do_nothing
;LE7F3           := do_nothing
LOAD            := do_nothing
SAVE            := do_nothing
;L2420           := do_nothing
;L2423           := do_nothing