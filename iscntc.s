.segment "CODE"
; ----------------------------------------------------------------------------
; SEE IF CONTROL-C TYPED
; ----------------------------------------------------------------------------
.ifdef BADGER6502
.include "badger6502_iscntc.s"
.endif

.ifndef CONFIG_CBM_ALL
.include "cbm_iscntc.s"
.endif

.ifndef BADGER6502
.ifdef KBD
.include "kbd_iscntc.s"
.endif
.if .def(OSI)
.include "osi_iscntc.s"
.endif
.ifdef APPLE
.include "apple_iscntc.s"
.endif
.ifdef KIM
.include "kim_iscntc.s"
.endif
.ifdef MICROTAN
.include "microtan_iscntc.s"
.endif
.ifdef AIM65
.include "aim65_iscntc.s"
.endif
.ifdef SYM1
.include "sym1_iscntc.s"
.endif
.endif

;!!! runs into "STOP"