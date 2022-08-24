;************************************************************
;                                                           *
;     Enhanced BASIC for the Motorola MC680xx               *
;                                                           *
; Derived from EhBASIC for the 6502 and adapted by:         *
; Lee Davison                                               *
;                                                           *
; mail : leeedavison@lycos.co.uk                            *
;                                                           *
; This is the generic version with I/O and LOAD/SAVE        *
; example code for the EASy68k editor/simulator. 2002/3.    *
;                                                           *
;************************************************************
;     Copyright (C) 2002/3 by Lee Davison. This program     *
;     may be freely distributed for personal use only.      *
;     All commercial rights are reserved.                   *
;************************************************************
;                                                           *
;     The choice of memory areas in this code is set to     *
;     reflect the actual memory present on a 68008 SBC      *
;     that I have.                                          *
;                                                           *
;     Memory map:                                           *
;                                                           *
;     ROM   $000000 - $00FFFF                               *
;     RAM   $040000 - $048000 ($050000 optional)            *
;                                                           *
;************************************************************

; Ver 1.10

	;opt	cre
	.area	text			; org 0x400, past the vectors in a real system

; the following code is simulator specific, change to suit your system

; output character to the console from register d0

vec_out:
	movem.l	d0-d1,-(sp)		; save d0, d1
	move.b	d0,d1			; copy character
	moveq	#6,d0			; character out
	trap	#15			; do i/o function
	movem.l	(sp)+,d0-d1		; restore d0, d1
	rts

; input a character from the console into register d0
; else return Cb=0 if there's no character available

vec_in:
	move.l	d1,-(sp)		; save d1
	moveq	#7,d0			; get status
	trap	#15			; do i/o function
	move.b	d1,d0			; copy status
	bne.s	retchr			; branch if character waiting

	move.l	(sp)+,d1		; restore d1
	ori.b	#0x00,d0			; set z flag
;	andi.b	#0xfe,ccr		* clear carry, flag we not got byte (done by ORI.b)
	rts

retchr:
	moveq	#5,d0			; get byte
	trap	#15			; do i/o function
	move.b	d1,d0			; copy byte
	move.l	(sp)+,d1		; restore d1
	ori.b	#0x00,d0			; set z flag on received byte
	ori.b	#1,ccr			; set carry, flag we got a byte
	rts

; LOAD routine for the Easy68k simulator

vec_ld:
	subq.w	#1,a5			; decrement execute pointer
	bsr	lab_gval		; get value from line

	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	beq	lab_fcer		; if null do function call error, then warm start

	movea.l	a0,a1			; copy filename pointer
	adda.w	d0,a0			; add length to find end of string
	move.b	(a0),-(sp)		; save byte
	move.l	a0,-(sp)		; save address
	moveq	#0,d0			; set for null
	move.b	d0,(a0)			; null terminate string
	move.w	#51,d0			; SHOULD BE MOVEQ !!! open existing file
	trap	#15
	tst.w	d0			; test load result
	bne.l	load_exit		; if error clear up and exit

	move.l	d1,file_id		; save file ID
	movea.l	#file_byte,a1		; point to byte buffer
	moveq	#1,d2			; set byte count
	moveq	#53,d0			; read first byte from file
	trap	#15

	tst.w	d0			; test status
	bne.l	load_close		; if error close files & exit

	moveq	#0,d2			; file position
	moveq	#55,d0			; reset file position
	trap	#15

	move.b	(a1),d0			; get first file byte
	bne.l	load_ascii		; if first byte not $00 go do ASCII load

					; do binary load
	movea.l	smeml,a1		; get start of program memory
	move.w	#0x7fff,d2		; set to $7FFF (max read length)
	moveq	#53,d0			; read from file
	trap	#15

	add.l	a1,d2			; add start of memory to loaded program length
	move.l	d2,sfncl		; save end of program

load_close:
	moveq	#50,d0			; close all files
	trap	#15

load_exit:
	movea.l	(sp)+,a0		; get string end back
	move.b	(sp)+,(a0)		; put byte back
	bsr	lab_147a		; go do "CLEAR"
	bra	lab_1274		; BASIC warm start entry, go wait for Basic command

; is ASCII file so just change the input vector

load_ascii:
	lea	load_in(pc),a1		; get byte from file vector
	move.l	a1,v_inptv		; set input vector
	movea.l	(sp)+,a0		; get string end back
	move.b	(sp)+,(a0)		; put byte back
	bra	lab_127d		; now we just wait for Basic command (no "Ready")

; input character to register d0 from file

load_in:
	movem.l	d1-d2/a1,-(sp)		; save d1, d2 & a1
	move.l	file_id,d1		; get file ID back
	movea.l	#file_byte,a1		; point to byte buffer
	moveq	#1,d2			; set count for one byte
	moveq	#53,d0			; read from file
	trap	#15

	tst.w	d0			; test status
	bne.l	load_eof		; branch if byte read failed

	move.b	(a1),d0			; get byte
	movem.l	(sp)+,d1-d2/a1		; restore d1, d2 & a1
	ori.b	#1,ccr			; set carry, flag we got a byte
	rts
					; got an error on read so restore vector and tidy up
load_eof:
	moveq	#50,d0			; close all files
	trap	#15

	lea	vec_in(pc),a1		; get byte from input device vector
	move.l	a1,v_inptv		; set input vector
	moveq	#0,d0			; clear byte
	movem.l	(sp)+,d1-d2/a1		; restore d1, d2 & a1
	bsr	lab_147a		; do CLEAR (erase variables/functions & flush stacks)
	bra	lab_1274		; BASIC warm start entry, go wait for Basic command

; SAVE routine for the Easy68k simulator

vec_sv:
	subq.w	#1,a5			; decrement execute pointer
	bsr	lab_gval		; get value from line
	bsr	lab_ctst		; check if source is string, else do type mismatch

	bsr	lab_gbyt		; get next BASIC byte
	beq.l	save_bas		; branch if no following

	cmp.b	#',,d0			; compare with ","
	bne	lab_sner		; not "," so go do syntax error/warm start

	bsr	lab_igby		; increment & scan memory
	ori.b	#0x20,d0			; ensure lower case
	cmp.b	#'a,d0			; compare with "a"
	bne	lab_sner		; not "a" so go do syntax error/warm start

	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	beq	lab_fcer		; if null do function call error, then warm start

	movea.l	a0,a1			; copy filename pointer
	adda.w	d0,a0			; add length to find end of string
	move.b	(a0),-(sp)		; save byte
	move.l	a0,-(sp)		; save address
	moveq	#0,d0			; set for null
	move.b	d0,(a0)			; null terminate string
	move.w	#52,d0			; SHOULD BE MOVEQ !!! open new file
	trap	#15
	tst.w	d0			; test save result
	bne.l	save_exit		; if error clear up and exit

	move.l	d1,file_id		; save file ID

	move.l	v_outpv,-(sp)		; save the output vector
	lea	save_out(pc),a1	; send byte to file vector
	move.l	a1,v_outpv		; change output vector

	bsr	lab_igby		; increment & scan memory
	bsr	lab_list		; go do list (line numbers applicable)

	move.l	(sp)+,v_outpv		; restore the output vector
	bra.l	save_close

save_bas:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	beq	lab_fcer		; if null do function call error, then warm start

	movea.l	a0,a1			; copy filename pointer
	adda.w	d0,a0			; add length to find end of string
	move.b	(a0),-(sp)		; save byte
	move.l	a0,-(sp)		; save address
	moveq	#0,d0			; set for null
	move.b	d0,(a0)			; null terminate string
	move.w	#52,d0			; SHOULD BE MOVEQ !!! open new file
	trap	#15
	tst.w	d0			; test save result
	bne.l	save_exit		; if error clear up and exit

	movea.l	smeml,a1		; get start of program
	move.l	sfncl,d2		; get end of program
	sub.l	a1,d2			; subtract start of program (= length)

	moveq	#54,d0			; write to file
	trap	#15

save_close:
	moveq	#50,d0			; close all files
	trap	#15

save_exit:
	movea.l	(sp)+,a0		; get string end back
	move.b	(sp)+,(a0)		; put byte back
	tst.w	d0			; test save result
	bne	lab_fcer		; if error do function call error, then warm start

	rts

; output character to file from register d0

save_out:
	movem.l	d0-d2/a1,-(sp)		; save d0, d1, d2 & a1
	move.l	file_id,d1		; get file ID back
	movea.l	#file_byte,a1		; point to byte buffer
	move.b	d0,(a1)			; save byte
	moveq	#1,d2			; set byte count
	moveq	#54,d0			; write to file
	trap	#15
	movem.l	(sp)+,d0-d2/a1		; restore d0, d1, d2 & a1
	rts

;***************************************************************************************
;***************************************************************************************
;***************************************************************************************
;***************************************************************************************
;
; Register use :- (must improve this !!)
;
;     a6 -  temp Bpntr			* temporary BASIC execute pointer
;     a5 -  Bpntr			* BASIC execute (get byte) pointer
;     a4 -  des_sk			* descriptor stack pointer
;     a3 -
;     a2 -
;     a1 -
;     a0 -
;
;     d7 -  FAC1 mantissa		* to do
;     d6 -  FAC1 sign & exponent	* to do
;     d5 -  FAC2 mantissa		* to do
;     d4 -  FAC2 sign & exponent	* to do
;     d3 -  BASIC got byte		* to do
;     d2 -
;     d1 -  general purpose
;     d0 -  general purpose
;
;

; turn off simulator key echo

code_start:
	moveq	#12,d0			; keyboard echo
	moveq	#0,d1			; turn off echo
	trap	#15			; do i/o function

; end of simulator specific code

; BASIC cold start entry point

lab_cold:
	move.l	#ram_base,sp		; set simulator stack for this prog
	move.w	#0x4ef9,d0		; JMP opcode
	movea.l	sp,a0			; point to start of vector table

	move.w	d0,(a0)+		; LAB_WARM
	lea	lab_cold(pc),a1	; initial warm start vector
	move.l	a1,(a0)+		; set vector

	move.w	d0,(a0)+		; Usrjmp
	lea	lab_fcer(pc),a1	; initial user function vector
					; "Function call" error
	move.l	a1,(a0)+		; set vector

	move.w	d0,(a0)+		; V_INPT JMP opcode
	lea	vec_in(pc),a1		; get byte from input device vector
	move.l	a1,(a0)+		; set vector

	move.w	d0,(a0)+		; V_OUTP JMP opcode
	lea	vec_out(pc),a1		; send byte to output device vector
	move.l	a1,(a0)+		; set vector

	move.w	d0,(a0)+		; V_LOAD JMP opcode
	lea	vec_ld(pc),a1		; load BASIC program vector
	move.l	a1,(a0)+		; set vector

	move.w	d0,(a0)+		; V_SAVE JMP opcode
	lea	vec_sv(pc),a1		; save BASIC program vector
	move.l	a1,(a0)+		; set vector

	move.w	d0,(a0)+		; V_CTLC JMP opcode
	lea	vec_cc(pc),a1		; save CTRL-C check vector
	move.l	a1,(a0)+		; set vector

	move.l	sp,(a0)			; save entry stack value

; set-up start values

lab_gmem:
	moveq	#0x00,d0			; clear d0
	move.b	d0,nullct		; default NULL count
	move.b	d0,tpos			; clear terminal position
	move.b	d0,ccflag		; allow CTRL-C check
	move.w	d0,prg_strt-2		; clear start word
	move.w	d0,bhsend		; clear value to string end word

	move.b	#0x50,twidth		; default terminal width byte for simulator
;	move.b	d0,twidth		* default terminal width byte

	move.b	#0x0e,tabsiz		; save default tab size = 14

	move.b	#0x38,iclim		; default limit for TAB = 14 for simulator
;	move.b	#0xf2,iclim		* default limit for TAB = 14

	movea.l	#des_sk,a4		; set descriptor stack start

	bsr	lab_crlf		; print CR/LF
	lea	lab_mszm(pc),a0	; point to memory size message
	bsr	lab_18c3		; print null terminated string from memory
	bsr	lab_inln		; print "? " and get BASIC input
					; return a0 pointing to the buffer start
	movea.l	a0,a5			; set BASIC execute pointer to buffer
	bsr	lab_gbyt		; scan memory
	bne.s	lab_2daa		; branch if not null (user typed something)

					; character was null so get memory size the hard way

	movea.l	#prg_strt,a0		; get start of program RAM
	move.l	#ram_top,d2		; remember top of ram+1
	move.w	#0x5555,d0		; test pattern 1
	move.w	#0xaaaa,d1		; test pattern 2

lab_2d93:
	cmpa.l	d2,a0			; compare with top of RAM+1
	beq.s	lab_2db6		; branch if match (end of user RAM)

	move.w	d0,(a0)			; set test word
	cmp.w	(a0),d0			; compare it
	bne.s	lab_2db6		; branch if fail

	move.w	d1,(a0)			; set new test word
	cmp.w	(a0)+,d1		; compare it
	beq.s	lab_2d93		; if ok go do next word

	subq.w	#2,a0			; decrement pointer
	bra.s	lab_2db6		; and branch if fail

lab_2daa:
	bsr	lab_2887		; get FAC1 from string
	move.b	fac1_e,d1		; get FAC1 exponent
	cmp.b	#0x81,d1			; compare with min
	bcs	lab_gmem		; if <1 go get again

	cmp.b	#0xa0,d1			; compare maximum integer range exponent
	bne.s	lab_2dab		; if not $A0 go test is less

	tst.b	fac1_s			; test FAC1 sign
	bpl.s	lab_2dad		; branch if FAC1 +ve

					; FAC1 was -ve and exponent is $A0
	cmpi.l	#0x80000000,fac1_m	; compare with max -ve
	beq.s	lab_2dad		; branch if max -ve

lab_2dab:
	bcc	lab_gmem		; get again if too big

lab_2dad:
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	movea.l	d0,a0			; copy result to address reg

lab_2db6:
	cmpa.l	#(prg_strt+0x100),a0	; compare with start of RAM+$100
	bcs	lab_gmem		; if too small go try again

; uncomment these lines if you want to check on the high limit of memory. Note if
; Ram_top is set too low then this will fail. default is ignore it and assume the
; users know what they're doing!
;
;	cmpa.l	#ram_top,a0		* compare with end of RAM+1
;	bhi.s	lab_gmem		* if too large go try again

	move.l	a0,ememl		; set end of mem
	move.l	a0,sstorl		; set bottom of string space

	moveq	#0,d0			; longword clear
	movea.l	#prg_strt,a0		; get start of mem
	move.l	d0,(a0)			; clear first longword
	move.l	a0,smeml		; save start of mem

	bsr	lab_1463		; do "NEW" and "CLEAR"
	bsr	lab_crlf		; print CR/LF
	move.l	ememl,d0		; get end of mem
	sub.l	smeml,d0		; subtract start of mem

	bsr	lab_295e		; print d0 as unsigned integer (bytes free)
	lea	lab_smsg(pc),a0	; point to start message
	bsr	lab_18c3		; print null terminated string from memory

	movea.l	#lab_rsed,a0		; point to value
	bsr	lab_ufac		; unpack memory (a0) into FAC1

	move.l	#lab_1274,wrmjpv	; warm start vector
	bsr	lab_rnd			; initialise
	jmp	lab_warm		; go do warm start

; search the stack for FOR, GOSUB or DO activity
; exit with z=1 if FOR, else exit with z=0
; return modified stack in a2

lab_11a1:
	movea.l	sp,a2			; copy stack pointer
	addq.w	#8,a2			; back past two levels of return address
lab_11a6:
	move.w	(a2),d0			; get token
	cmp.w	#tk_for,d0		; is FOR token on stack?
	bne.s	rts_002			; exit if not

					; was FOR token
	movea.l	2(a2),a0		; get stacked FOR variable pointer
	movea.l	frnxtl,a1		; get variable pointer for FOR/NEXT
	cmpa.l	#0,a1			; set the flags
	bne.s	lab_11bb		; branch if not null

	move.l	a0,frnxtl		; save var pointer for FOR/NEXT
	cmp.w	d0,d0			; set z for ok exit
rts_002:
	rts

lab_11bb:
	cmpa.l	a1,a0			; compare var pointer with stacked var pointer
	beq.s	rts_003			; exit if match found

	adda.w	#0x1a,a2			; add FOR stack use size
	cmpa.l	ram_base,a2		; compare with stack top
	bcs.s	lab_11a6		; loop if not at start of stack

rts_003:
	rts

; check room on stack for d0 bytes

;LAB_1212
;	add.l	#ram_strt,d0		* add start of ram to value to check
;	cmp.l	sp,d0			* compare new "limit" with stack pointer
;	bcc.s	lab_omer		* if sp<limit do "Out of memory" error/warm start

;	rts

; check available memory, "Out of memory" error if no room
; addr to check is in a0

lab_121f:
	cmpa.l	sstorl,a0		; compare with bottom of string memory
	bcs.s	rts_004			; if less then exit (is ok)

	bsr	lab_garb		; garbage collection routine
	cmpa.l	sstorl,a0		; compare with bottom of string memory
	bls.s	lab_omer		; if Sstorl <= a0 do "Out of memory" error/warm start

rts_004:					; ok exit, carry set
	rts

;* do internal error
;
;lab_iter
;	moveq	#0x58,d0			* error code $58 "Internal" error
;	bra.s	lab_xerr		* do error #d0, then warm start

; do address error

lab_ader:
	moveq	#0x54,d0			; error code $54 "Address" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do wrong dimensions error

lab_wder:
	moveq	#0x50,d0			; error code $50 "Wrong dimensions" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do undimensioned array error

lab_uder:
	moveq	#0x4c,d0			; error code $4C "undimensioned array" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do undefined variable error

lab_uver:
	moveq	#0x48,d0			; error code $24*2 "undefined variable" error
	bra.s	lab_xerr		; do error #X then warm start

; if you want undefined variables to return 0 (or "") then comment out the above
; two lines and uncomment these two
;
; value returned by this is either numeric zero (exponent byte is $00) or null string
; (string pointer is $00). in fact a pointer to any $00 longword would have done.
;
;	lea	lab_1d96(pc),a0	* else return dummy null pointer
;	rts

; do loop without do error

lab_lder:
	moveq	#0x44,d0			; error code $22*2 "LOOP without DO" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do undefined function error

lab_ufer:
	moveq	#0x40,d0			; error code $20*2 "Undefined function" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do can't continue error

lab_ccer:
	moveq	#0x3c,d0			; error code $1E*2 "Can't continue" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do string too complex error

lab_scer:
	moveq	#0x38,d0			; error code $1C*2 "String too complex" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do string too long error

lab_sler:
	moveq	#0x34,d0			; error code $1A*2 "String too long" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do type missmatch error

lab_tmer:
	moveq	#0x30,d0			; error code $18*2 "Type mismatch" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do illegal direct error

lab_ider:
	moveq	#0x2c,d0			; error code $16*2 "Illegal direct" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do divide by zero error

lab_dzer:
	moveq	#0x28,d0			; error code $14*2 "Divide by zero" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do double dimension error

lab_dder:
	moveq	#0x24,d0			; set error $12*2 "Double dimension" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do array bounds error

lab_aber:
	moveq	#0x20,d0			; error code $10*2 "Array bounds" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do undefine satement error

lab_user:
	moveq	#0x1c,d0			; error code $0E*2 "Undefined statement" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do out of memory error

lab_omer:
	moveq	#0x18,d0			; error code $0C*2 "Out of memory" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do overflow error

lab_ofer:
	moveq	#0x14,d0			; error code $0A*2 "Overflow" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do function call error

lab_fcer:
	moveq	#0x10,d0			; error code $08*2 "Function call" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do out of data error

lab_oder:
	moveq	#0x0c,d0			; error code $06*2 "Out of DATA" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do return without gosub error

lab_rger:
	moveq	#0x08,d0			; error code $04*2 "RETURN without GOSUB" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do syntax error

lab_sner:
	moveq	#0x04,d0			; error code $02*2 "Syntax" error
	bra.s	lab_xerr		; do error #d0, then warm start

; do next without for error

lab_nfer:
	moveq	#0x00,d0			; else set error $00 "NEXT without FOR" error

; do error #d0, then warm start

lab_xerr:
	move.w	d0,d7			; copy word
	bsr	lab_1491		; flush stack & clear continue flag
	bsr	lab_crlf		; print CR/LF
	lea	lab_baer(pc),a1	; start of error message pointer table
	movea.l	(a1,d7.w),a0		; get error message address
	bsr	lab_18c3		; print null terminated string from memory
	lea	lab_emsg(pc),a0	; point to " Error" message
lab_1269:
	bsr	lab_18c3		; print null terminated string from memory
	move.l	clinel,d0		; get current line
	bmi.s	lab_1274		; go do warm start if -ve # (was immediate mode)

					; else print line number
	bsr	lab_2953		; print " in line [LINE #]"

; BASIC warm start entry point, wait for Basic command

lab_1274:
	lea	lab_rmsg(pc),a0	; point to "Ready" message
	bsr	lab_18c3		; go do print string

; wait for Basic command (no "Ready")

lab_127d:
	moveq	#-1,d1			; set  to -1
	move.l	d1,clinel		; set current line #
	move.b	d1,breakf		; set break flag
	move.l	#ibuffs,a5		; set basic execute pointer ready for new line
lab_127e:
	bsr	lab_1357		; call for BASIC input
	bsr	lab_gbyt		; scan memory
	beq.s	lab_127e		; loop while null

; got to interpret input line now ....

	bcs.s	lab_1295		; branch if numeric character (handle new BASIC line)

					; no line number, immediate mode, a5 is buffer start
	bsr	lab_13a6		; crunch keywords into Basic tokens
					; crunch from (a5), output to (a0)
					; returns ..
					; d2 is length, d1 trashed, d0 trashed, a1 trashed

	exg	a0,a5			; set execute pointer to buffer
	bra	lab_15f6		; go scan & interpret code

; handle new BASIC line

lab_1295:
	bsr	lab_gfpn		; get fixed-point number into temp integer
	bsr	lab_13a6		; crunch keywords into Basic tokens
					; crunch from (a5), output to (a0)
					; returns ..
					; d2 is length, d1 trashed, d0 trashed, a1 trashed
	bsr	lab_ssln		; search BASIC for temp integer line number
					; returns pointer in a0
	bcs.s	lab_12e6		; branch if not found

					; aroooogah! line # already exists! delete it
	movea.l	(a0),a1			; get start of block (next line pointer)
	move.l	sfncl,d0		; get end of block (start of functions)
	sub.l	a1,d0			; subtract start of block ( = bytes to move)
	lsr.l	#1,d0			; /2 (word move)
	subq.w	#1,d0			; adjust for DBF loop
	movea.l	a0,a2			; copy destination
lab_12b0:
	move.w	(a1)+,(a2)+		; copy word
	dbf	d0,lab_12b0		; loop until done

	move.l	a2,sfncl		; start of functions
	move.l	a2,svarl		; save start of variables
	move.l	a2,sstrl		; start of strings
	move.l	a2,sarryl		; save start of arrays
	move.l	a2,earryl		; save end of arrays

					; got new line in buffer and no existing same #
lab_12e6:
	move.b	ibuffs,d0		; get byte from start of input buffer
	beq.s	lab_1325		; if null line go do line chaining

					; got new line and it isn't empty line
	movea.l	sfncl,a1		; get start of functions (end of block to move)
	move.l	a1,a2			; copy it
	adda.w	d2,a2			; add offset to destination (line length)
	addq.w	#8,a2			; add room for pointer and line #

	move.l	a2,sfncl		; start of functions
	move.l	a2,svarl		; save start of variables
	move.l	a2,sstrl		; start of strings
	move.l	a2,sarryl		; save start of arrays
	move.l	a2,earryl		; save end of arrays
	move.l	ememl,sstorl		; copy end of mem to start of strings (clear strings)

	move.l	a1,d1			; copy end of block to move
	sub.l	a0,d1			; subtract start of block to move
	lsr.l	#1,d1			; /2 (word copy)
	subq.l	#1,d1			; correct for loop end on -1
lab_1301:
	move.w	-(a1),-(a2)		; decrement pointers and copy word
	dbf	d1,lab_1301		; decrement & loop

	movea.l	#ibuffs,a1		; source is input buffer
	movea.l	a0,a2			; copy destination
	moveq	#-1,d1			; set to allow re-chaining
	move.l	d1,(a2)+		; set next line pointer (allow re-chaining)
	move.l	itemp,(a2)+		; save line number
	lsr.w	#1,d2			; /2 (word copy)
	subq.w	#1,d2			; correct for loop end on -1
lab_1303:
	move.w	(a1)+,(a2)+		; copy word
	dbf	d2,lab_1303		; decrement & loop

	bra.s	lab_1325		; go test for end of prog

; rebuild chaining of Basic lines

lab_132e:
	addq.w	#8,a0			; point to first code byte of line, there is always
					; 1 byte + [EOL] as null entries are deleted
lab_1330:
	tst.b	(a0)+			; test byte
	bne.s	lab_1330		; loop if not [EOL]

					; was [EOL] so get next line start
	move.w	a0,d1			; past pad byte(s)
	andi.w	#1,d1			; mask odd bit
	add.w	d1,a0			; add back to ensure even
	move.l	a0,(a1)			; save next line pointer to current line
lab_1325:
	movea.l	a0,a1			; copy pointer for this line
	tst.l	(a0)			; test pointer to next line
	bne.s	lab_132e		; not end of program yet so we must
					; go and fix the pointers

	bsr	lab_1477		; reset execution to start, clear vars & flush stack
	bra	lab_127d		; now we just wait for Basic command (no "Ready")

; receive line from keyboard
					; $08 as delete key (BACKSPACE on standard keyboard)
lab_134b:
	bsr	lab_prna		; go print the character
	subq.w	#0x01,d1			; decrement the buffer index (delete)
	bra.s	lab_1359		; re-enter loop

; print "? " and get BASIC input
; return a0 pointing to the buffer start

lab_inln:
	bsr	lab_18e3		; print "?" character
	moveq	#' ,d0			; load " "
	bsr	lab_prna		; go print

; call for BASIC input (main entry point)
; return a0 pointing to the buffer start

lab_1357:
	moveq	#0x00,d1			; clear buffer index
	movea.l	#ibuffs,a0		; set buffer base pointer
lab_1359:
	jsr	v_inpt			; call scan input device
	bcc.s	lab_1359		; loop if no byte

	beq.s	lab_1359		; loop if null byte

	cmp.b	#0x07,d0			; compare with [BELL]
	beq.s	lab_1378		; branch if [BELL]

	cmp.b	#0x0d,d0			; compare with [CR]
	beq	lab_1866		; do CR/LF exit if [CR]

	tst.w	d1			; set flags on buffer index
	bne.s	lab_1374		; branch if not empty

					; next two lines ignore any non print character
					; & [SPACE] if the input buffer is empty
	cmp.b	#' +1,d0		; compare with [SP]+1
	bcs.s	lab_1359		; if < ignore character

lab_1374:
	cmp.b	#0x08,d0			; compare with [BACKSPACE] (delete last character)
	beq.s	lab_134b		; go delete last character

lab_1378:
	cmp.w	#(ibuffe-ibuffs-1),d1	; compare character count with max-1
	bcc.s	lab_138e		; skip store & do [BELL] if buffer full

	move.b	d0,(a0,d1.w)		; else store in buffer
	addq.w	#0x01,d1			; increment index
lab_137f:
	bsr	lab_prna		; go print the character
	bra.s	lab_1359		; always loop for next character

; announce buffer full

lab_138e:
	moveq	#0x07,d0			; [BELL] character into d0
	bra.s	lab_137f		; go print the [BELL] but ignore input character

; crunch keywords into Basic tokens
; crunch from (a5), output to (a0)
; returns ..
; d2 is length
; d1 trashed
; d0 trashed
; a1 trashed

; this is the improved BASIC crunch routine and is 10 to 100 times faster than the
; old list search

lab_13a6:
	moveq	#0x00,d1			; set read index
	move.w	d1,d2			; set save index
	move.b	d1,oquote		; clear open quote/DATA flag
lab_13ac:
	moveq	#0,d0			; clear word
	move.b	(a5,d1.w),d0		; get byte from input buffer
	beq.s	lab_13ec		; if null save byte then continue crunching

	cmp.b	#'_,d0			; compare with "_"
	bcc.s	lab_13ec		; if >= "_" save byte then continue crunching

	cmp.b	#'<,d0			; compare with "<"
	bcc.s	lab_13cc		; if >= "<" go crunch

	cmp.b	#'0,d0			; compare with "0"
	bcc.s	lab_13ec		; if >= "0" save byte then continue crunching

	move.b	d0,asrch		; save buffer byte as search character
	cmp.b	#0x22,d0			; is it quote character?
	beq.l	lab_1410		; branch if so (copy quoted string)

	cmp.b	#'*,d0			; compare with "*"
	bcs.s	lab_13ec		; if <= "*" save byte then continue crunching

					; crunch rest
lab_13cc:
	btst.b	#6,oquote		; test open quote/DATA token flag
	bne.s	lab_13ec		; branch if b6 of Oquote set (was DATA)
					; go save byte then continue crunching

	sub.b	#0x2a,d0			; normalise byte
	add.w	d0,d0			; *2 makes word offset (high byte=$00)
	lea	tab_chrt(pc),a1	; get keyword offset table address
	move.w	(a1,d0.w),d0		; get offset into keyword table
	bmi.s	lab_141f		; branch if no keywords for character

	lea	tab_star(pc),a1	; get keyword table address
	adda.w	d0,a1			; add keyword offset
	moveq	#-1,d3			; clear index
	move.w	d1,d4			; copy read index
lab_13d6:
	addq.w	#1,d3			; increment table index
	move.b	(a1,d3.w),d0		; get byte from table
lab_13d8:
	bmi.s	lab_13ea		; branch if token (save token and continue crunching)

	addq.w	#1,d4			; increment read index
	cmp.b	(a5,d4.w),d0		; compare byte from input buffer
	beq.s	lab_13d6		; loop if character match

	bra.s	lab_1417		; branch if no match

lab_13ea:
	move.w	d4,d1			; update read index
lab_13ec:
	move.b	d0,(a0,d2.w)		; save byte to output
	addq.w	#1,d2			; increment buffer save index
	addq.w	#1,d1			; increment buffer read index
	tst.b	d0			; set flags
	beq.s	lab_142a		; branch if was null [EOL]

					; d0 holds token or byte here
	sub.b	#0x3a,d0			; subtract ":"
	beq.s	lab_13ff		; branch if it was ":" (is now $00)

					; d0 now holds token-$3A
	cmp.b	#(tk_data-0x3a),d0	; compare with DATA token - $3A
	bne.s	lab_1401		; branch if not DATA

					; token was : or DATA
lab_13ff:
	move.b	d0,oquote		; save token-$3A ($00 for ":", TK_DATA-$3A for DATA)
lab_1401:
	sub.b	#(tk_rem-0x3a),d0	; subtract REM token offset
	bne	lab_13ac		; If wasn't REM then go crunch rest of line

	move.b	d0,asrch		; else was REM so set search for [EOL]

					; loop for REM, "..." etc.
lab_1408:
	move.b	(a5,d1.w),d0		; get byte from input buffer
	beq.s	lab_13ec		; branch if null [EOL]

	cmp.b	asrch,d0		; compare with stored character
	beq.s	lab_13ec		; branch if match (end quote, REM, :, or DATA)

					; entry for copy string in quotes, don't crunch
lab_1410:
	move.b	d0,(a0,d2.w)		; save byte to output
	addq.w	#1,d2			; increment buffer save index
	addq.w	#1,d1			; increment buffer read index
	bra.s	lab_1408		; loop

					; not found keyword this go
					; so find the end of this word in the table
lab_1417:
	move.w	d1,d4			; reset read pointer
lab_141b:
	addq.w	#1,d3			; increment keyword table pointer (flag unchanged)
	move.b	(a1,d3.w),d0		; get keyword table byte
	bpl.s	lab_141b		; if not end of keyword go do next byte

	addq.w	#1,d3			; increment keyword table pointer (flag unchanged)
	move.b	(a1,d3.w),d0		; get keyword table byte
	bne.s	lab_13d8		; go test next word if not zero byte (table end)

					; reached end of table with no match
lab_141f:
	move.b	(a5,d1.w),d0		; restore byte from input buffer
	bra.s	lab_13ec		; go save byte in output and continue crunching

					; reached [EOL]
lab_142a:
	moveq	#0,d0			; ensure longword clear
	btst	d0,d2			; test odd bit (fastest)
	beq.s	lab_142c		; branch if no bytes to fill

	move.b	d0,(a0,d2.w)		; clear next byte
	addq.w	#1,d2			; increment buffer save index
lab_142c:
	move.l	d0,(a0,d2.w)		; clear next line pointer (EOT in immediate mode)
	rts

; search Basic for temp integer line number from start of mem

lab_ssln:
	movea.l	smeml,a0		; get start of program mem

; search Basic for temp integer line number from a0
; returns Cb=0 if found
; returns a0 pointer to found or next higher (not found) line

lab_shln:
	move.l	itemp,d1		; get required line #
	bra.s	lab_scln		; go search for required line from a0

lab_145f:
	movea.l	d0,a0			; copy next line pointer
lab_scln:
	move.l	(a0)+,d0		; get next line pointer and point to line #
	beq.s	lab_145e		; is end marker so we're done, do 'no line' exit

	cmp.l	(a0),d1			; compare this line # with required line #
	bgt.s	lab_145f		; loop if required # > this #

	subq.w	#4,a0			; adjust pointer, flags not changed
	rts

lab_145e:
	subq.w	#4,a0			; adjust pointer, flags not changed
	subq.l	#1,d0			; make end program found = -1, set carry
	rts

; perform NEW

lab_new:
	bne.s	rts_005			; exit if not end of statement (do syntax error)

lab_1463:
	movea.l	smeml,a0		; point to start of program memory
	moveq	#0,d0			; clear longword
	move.l	d0,(a0)+		; clear first line, next line pointer
	move.l	a0,sfncl		; set start of functions

; reset execution to start, clear vars & flush stack

lab_1477:
	movea.l	smeml,a5		; reset BASIC execute pointer
	subq.w	#0x01,a5			; -1 (as end of previous line)

; "CLEAR" command gets here

lab_147a:
	move.l	ememl,sstorl		; save end of mem as bottom of string space
	move.l	sfncl,d0		; get start of functions
	move.l	d0,svarl		; start of variables
	move.l	d0,sstrl		; start of strings
	move.l	d0,sarryl		; set start of arrays
	move.l	d0,earryl		; set end of arrays
	bsr	lab_161a		; perform RESTORE command

; flush stack & clear continue flag

lab_1491:
	movea.l	#des_sk,a4		; reset descriptor stack pointer

	move.l	(sp)+,d0		; pull return address
	movea.l	entry_sp,sp		; flush stack
	move.l	d0,-(sp)		; restore return address

	moveq	#0,d0			; clear longword
	move.l	d0,cpntrl		; clear continue pointer
	move.b	d0,sufnxf		; clear subscript/FNX flag
rts_005:
	rts

; perform CLEAR

lab_clear:
	beq.s	lab_147a		; if no following byte go do "CLEAR"

	rts				; was following byte (go do syntax error)

; perform LIST [n][-m]

lab_list:
	bls.s	lab_14bd		; branch if next character numeric (LIST n...)
					; or if next character [NULL] (LIST)

	cmp.b	#tk_minus,d0		; compare with token for -
	bne.s	rts_005			; exit if not - (LIST -m)

					; LIST [[n][-m]]
					; this sets the n, if present, as the start & end
lab_14bd:
	bsr	lab_gfpn		; get fixed-point number into temp integer
	bsr	lab_ssln		; search BASIC for temp integer line number
					; (pointer in a0)
	bsr	lab_gbyt		; scan memory
	beq.s	lab_14d4		; branch if no more characters

					; this bit checks the - is present
	cmp.b	#tk_minus,d0		; compare with token for -
	bne.s	rts_005			; return if not "-" (will be Syntax error)

					; LIST [n]-m
					; the - was there so set m as the end value
	bsr	lab_igby		; increment & scan memory
	bsr	lab_gfpn		; get fixed-point number into temp integer
	bne.s	lab_14d4		; branch if was not zero

	moveq	#-1,d1			; set end for $FFFFFFFF
	move.l	d1,itemp		; save Itemp
lab_14d4:
	move.b	#0x00,oquote		; clear open quote flag
	bsr	lab_crlf		; print CR/LF
	movea.l	(a0)+,a1		; get next line pointer
	move.l	a1,d0			; copy to set the flags
	beq.s	rts_005			; if null all done so exit

	bsr	lab_1629		; do CRTL-C check vector

	move.l	(a0)+,d0		; get this line #
	move.l	itemp,d1		; get end line #
	beq.s	lab_14e2		; if end=0 list whole thing

	cmp.l	d0,d1			; compare this line # with end line #
	bcs.s	rts_005			; if greater all done so exit

lab_14e2:
	movem.l	a0-a1/d1-d4,-(sp)	; save registers !! work out what's needed here !!
	bsr	lab_295e		; print d0 as unsigned integer
	movem.l	(sp)+,a0-a1/d1-d4	; restore registers !! and here !!
	moveq	#0x20,d0			; space is the next character
lab_150c:
	bsr	lab_prna		; go print the character
	cmp.b	#0x22,d0			; was it " character
	bne.s	lab_1519		; branch if not

					; we're either entering or leaving quotes
	eor.b	#0xff,oquote		; toggle open quote flag
lab_1519:
	move.b	(a0)+,d0		; get byte and increment pointer
	bne.s	lab_152e		; branch if not [EOL] (go print)

					; was [EOL]
	movea.l	a1,a0			; copy next line pointer
	move.l	a0,d0			; copy to set flags
	bne.s	lab_14d4		; go do next line if not [EOT]

	rts

lab_152e:
	bpl.s	lab_150c		; just go print it if not token byte

					; else was token byte so uncrunch it (maybe)
	btst.b	#7,oquote		; test the open quote flag
	bne.s	lab_150c		; just go print character if open quote set

					; else uncrunch BASIC token
	lea	lab_keyt(pc),a2	; get keyword table address
	moveq	#0x7f,d1			; mask into d1
	and.b	d0,d1			; copy and mask token
	lsl.w	#2,d1			; *4
	lea	(a2,d1.w),a2		; get keyword entry address
	move.b	(a2)+,d0		; get byte from keyword table
	bsr	lab_prna		; go print the first character
	moveq	#0,d1			; clear d1
	move.b	(a2)+,d1		; get remaining length byte from keyword table
	bmi.s	lab_1519		; if -ve done so go get next byte

	move.w	(a2),d0			; get offset to rest
	lea	tab_star(pc),a2	; get keyword table address
	lea	(a2,d0.w),a2		; get address of rest
lab_1540:
	move.b	(a2)+,d0		; get byte from keyword table
	bsr	lab_prna		; go print the character
	dbf	d1,lab_1540		; decrement and loop if more to do

	bra.s	lab_1519		; go get next byte

; perform FOR

lab_for:
	bsr	lab_let			; go do LET
	bsr	lab_11a1		; search the stack for FOR or GOSUB activity
					; exit with z=1 if FOR else exit with z=0
					; return modified stack in a2
	bne.s	lab_1567		; branch if FOR (this variable) not found

					; FOR (this variable) was found so first
					; we dump the old one
	adda.w	#22,sp			; reset stack (dump FOR structure (-4 bytes))
lab_1567:
	addq.w	#4,sp			; dump return address
;	moveq	#28,d0			* we need 28 bytes !
;	bsr.s	lab_1212		* check room on stack for d0 bytes
	bsr	lab_snbs		; scan for next BASIC statement ([:] or [EOL])
					; returns a0 as pointer to [:] or [EOL]
	move.l	a0,-(sp)		; push onto stack
	move.l	clinel,-(sp)		; push current line onto stack

	moveq	#tk_to-0x100,d0		; set "TO" token
	bsr	lab_scca		; scan for CHR$(d0) , else syntax error/warm start
	bsr	lab_ctnm		; check if source is numeric, else type mismatch
	move.b	dtypef,-(sp)		; push FOR variable data type onto stack
	bsr	lab_evnm		; evaluate expression & check is numeric, else
					; do type mismatch

	move.l	fac1_m,-(sp)		; push TO value mantissa
	move.w	fac1_e,-(sp)		; push TO value exponent and sign

	move.l	#0x80000000,fac1_m	; set default STEP size mantissa
	move.w	#0x8100,fac1_e		; set default STEP size exponent and sign

	bsr	lab_gbyt		; scan memory
	cmp.b	#tk_step,d0		; compare with STEP token
	bne.s	lab_15b3		; jump if not "STEP"

					; was step so ....
	bsr	lab_igby		; increment & scan memory
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
lab_15b3:
	move.l	fac1_m,-(sp)		; push STEP value mantissa
	move.w	fac1_e,-(sp)		; push STEP value exponent and sign

	move.l	frnxtl,-(sp)		; push variable pointer for FOR/NEXT
	move.w	#tk_for,-(sp)		; push FOR token on stack

	bra.s	lab_15c2		; go do interpreter inner loop

lab_15dc:				; have reached [EOL]+1
	move.w	a5,d0			; copy BASIC execute pointer
	and.w	#1,d0			; and make line start address even
	add.w	d0,a5			; add to BASIC execute pointer
	move.l	(a5)+,d0		; get next line pointer
	beq	lab_1274		; if null go to immediate mode, no "BREAK" message
					; (was immediate or [EOT] marker)

	move.l	(a5)+,clinel		; save (new) current line #
lab_15f6:
	bsr	lab_gbyt		; get BASIC byte
	bsr.s	lab_15ff		; go interpret BASIC code from (a5)

; interpreter inner loop entry point

lab_15c2:
	bsr.s	lab_1629		; do CRTL-C check vector
	tst.w	clinel			; test current line #, is -ve for immediate mode
	bmi.s	lab_15d1		; branch if immediate mode

	move.l	a5,cpntrl		; save BASIC execute pointer as continue pointer
lab_15d1:
	move.b	(a5)+,d0		; get this byte & increment pointer
	beq.s	lab_15dc		; loop if [EOL]

	cmp.b	#0x3a,d0			; compare with ":"
	beq.s	lab_15f6		; loop if was statement separator

	bra	lab_sner		; else syntax error, then warm start

; interpret BASIC code from (a5)

lab_15ff:
	beq.s	rts_006			; exit if zero [EOL]

lab_1602:
	eori.b	#0x80,d0			; normalise token
	bmi	lab_let			; if not token, go do implied LET

	cmp.b	#(tk_tab-0x80),d0	; compare normalised token with TAB
	bcc	lab_sner		; branch if d0>=TAB, syntax error/warm start
					; only tokens before TAB can start a statement

	ext.w	d0			; byte to word (clear high byte)
	add.w	d0,d0			; *2
	add.w	d0,d0			; *4 (offset to longword vector)
	lea	lab_ctbl(pc),a0	; get vector table base address
	move.l	(a0,d0.w),-(sp)		; push vector
	bra	lab_igby		; get following byte & execute vector

; CTRL-C check jump. this is called as a subroutine but exits back via a jump if a
; key press is detected.

lab_1629:
;	cmpa.l	#des_sk,a4		* check discriptor stack is empty
;	bne	lab_iter		* if not do internal error

	jmp	v_ctlc			; ctrl c check vector

; if there was a key press it gets back here .....

lab_1636:
	cmp.b	#0x03,d0			; compare with CTRL-C
	bne.s	rts_006			; return if wasn't CTRL-C

; perform STOP

lab_stop:
	bcc.s	lab_163b		; branch if token follows STOP
					; else just END
; perform END

lab_end:
	move.b	#0,breakf		; clear break flag, indicate program end
lab_163b:

	cmpa.l	#ibuffe,a5		; compare execute address with buffer end
	bcs.s	lab_164f		; branch if BASIC pointer is in buffer
					; (can't continue in immediate mode)

					; else...
	move.l	a5,cpntrl		; save BASIC execute pointer as continue pointer
lab_1647:
	move.l	clinel,blinel		; save break line
lab_164f:
	move.l	(sp)+,d0		; pull return address, don't return to execute loop
	move.b	breakf,d0		; get break flag
	beq	lab_1274		; go do warm start if was program end

	lea	lab_bmsg(pc),a0	; point to "Break"
	bra	lab_1269		; print "Break" and do warm start

; perform RESTORE

lab_restore:
	bne.s	lab_resn		; branch if next character not null (RESTORE n)

lab_161a:
	movea.l	smeml,a0		; copy start of mem
lab_1624:
	subq.w	#1,a0			; -1
	move.l	a0,dptrl		; save DATA pointer
rts_006:
	rts
					; is RESTORE n
lab_resn:
	bsr	lab_gfpn		; get fixed-point number into temp integer
	cmp.l	clinel,d0		; compare current line # with required line #
	bls.s	lab_nsch		; branch if >= (start search from beginning)

	movea.l	a5,a0			; copy BASIC execute pointer
lab_ress:
	tst.b	(a0)+			; test next byte & increment pointer
	bne.s	lab_ress		; loop if not EOL

	move.w	a0,d1			; copy pointer
	and.w	#1,d1			; mask odd bit
	add.w	d1,a0			; add pointer
	bra.s	lab_gsch		; go find

					; search for line in Itemp from start of memory
lab_nsch:
	movea.l	smeml,a0		; get start of mem

					; search for line in Itemp from (a0)
lab_gsch:
	bsr	lab_shln		; search for temp integer line number from a0
					; returns Cb=0 if found
	bcs	lab_user		; go do "Undefined statement" error if not found

	bra.s	lab_1624		; else save DATA pointer & return

; perform NULL

lab_null:
	bsr	lab_gtby		; get byte parameter, result in d0 and Itemp
	move.b	d0,nullct		; save new NULL count
	rts

; perform CONT

lab_cont:
	bne	lab_sner		; if following byte exit to do syntax error

	move.l	cpntrl,d0		; get continue pointer
	beq	lab_ccer		; go do can't continue error if we can't

					; we can continue so ...
	movea.l	d0,a5			; save continue pointer as BASIC execute pointer
	move.l	blinel,clinel		; set break line as current line
	rts

; perform RUN

lab_run:
	bne.l	lab_runn		; if following byte do RUN n

	bsr	lab_1477		; execution to start, clear vars & flush stack
	move.l	a5,cpntrl		; save as continue pointer
	bra	lab_15c2		; go do interpreter inner loop
					; (can't RTS, we flushed the stack!)

lab_runn:
	bsr	lab_147a		; go do "CLEAR"
	bra.s	lab_16b0		; get n and do GOTO n

; perform DO

lab_do:
;	move.l	#5,d0			* need 5 bytes for DO ##
;	bsr.s	lab_1212		* check room on stack for A bytes
	move.l	a5,-(sp)		; push BASIC execute pointer on stack
	move.l	clinel,-(sp)		; push current line on stack
	move.w	#tk_do,-(sp)		; push token for DO on stack
doagain:
	bsr	lab_gbyt		; scan memory
	bra	lab_15c2		; go do interpreter inner loop

; perform GOSUB

lab_gosub:
;	move.l	#10,d0			* need 10 bytes for GOSUB ##
;	bsr.s	lab_1212		* check room on stack for d0 bytes
	move.l	a5,-(sp)		; push BASIC execute pointer
	move.l	clinel,-(sp)		; push current line
	move.w	#tk_gosub,-(sp)		; push token for GOSUB
lab_16b0:
	bsr	lab_gbyt		; scan memory
	bsr.s	lab_goto		; perform GOTO n
	bra	lab_15c2		; go do interpreter inner loop
					; (can't RTS, we used the stack!)

; tail of IF command

lab_1754:
	bsr	lab_gbyt		; scan memory, Cb=1 if "0"-"9"
	bcc	lab_15ff		; if not numeric interpret BASIC code from (a5)

;*	bra	lab_goto		* else do GOTO n (was numeric)

; perform GOTO

lab_goto:
	bsr	lab_gfpn		; get fixed-point number into temp integer

	movea.l	a5,a0			; copy BASIC execute pointer
lab_gots:
	tst.b	(a0)+			; test next byte & increment pointer
	bne.s	lab_gots		; loop if not EOL

	move.w	a0,d1			; past pad byte(s)
	and.w	#1,d1			; mask odd bit
	add.w	d1,a0			; add to pointer

	move.l	clinel,d0		; get current line
	bmi.s	lab_16d0		; if immediate mode start search from beginning

	cmp.l	itemp,d0		; compare wanted # with current #
	bcs.s	lab_16d4		; branch if current # < wanted #
					; (start search from here)

; search for line # in temp (Itemp) from start of mem pointer (Smeml)

lab_16d0:
	movea.l	smeml,a0		; get start of memory

; search for line # in Itemp from (a0)

lab_16d4:
	bsr	lab_shln		; search for temp integer line number from a0
					; returns Cb=0 if found
	bcs	lab_user		; if carry set go do "Undefined statement" error

	movea.l	a0,a5			; copy to basic execute pointer
	subq.w	#1,a5			; decrement pointer
	move.l	a5,cpntrl		; save as continue pointer
	rts

; perform LOOP

lab_loop:
	moveq	#0,d7			; clear top 24 bits
	move.b	d0,d7			; save following token (byte)
	bsr	lab_11a1		; search the stack for FOR or GOSUB activity
					; exit with Zb=1 if FOR else exit with Zb=0
					; return modified stack in a2

	cmp.w	#tk_do,d0		; compare with DO token
	bne	lab_lder		; branch if no matching DO

	tst.b	d7			; test saved following token
	beq.s	loopalways		; if no following token loop forever
					; (stack pointer in a2)

	sub.b	#tk_until,d7		; subtract token for UNTIL
	beq.s	dorest			; branch if was UNTIL

	subq.b	#1,d7			; decrement result
	bne	lab_sner		; if not WHILE go do syntax error & warm start
					; only if the token was WHILE will this fail

	subq.b	#1,d7			; set invert result longword
dorest:
	bsr	lab_igby		; increment & scan memory
	move.l	a2,-(sp)		; save modified stack pointer
	bsr	lab_evex		; evaluate expression
	movea.l	(sp)+,a2		; restore modified stack pointer
	tst.b	fac1_e			; test FAC1 exponent
	beq.s	docmp			; if =0 go do straight compare

	move.b	#0xff,fac1_e		; else set all bits
docmp:
	eor.b	d7,fac1_e		; EOR with invert byte
	bne.s	loopdone		; if <> 0 clear stack & back to interpreter loop

					; loop condition wasn't met so do it again
loopalways:
	move.l	2(a2),clinel		; copy DO current line low byte
	move.l	6(a2),a5		; save BASIC execute pointer low byte
	addq.w	#4,sp			; dump call to this routine
	bra	doagain			; go do DO again

					; clear stack & back to interpreter loop
loopdone:
	adda.w	#14,sp			; dump structure and call from stack
	bra.s	lab_data		; go perform DATA (find : or [EOL])

; perform RETURN

lab_return:
	bne.s	rts_007			; exit if following token (to allow syntax error)

	bsr	lab_11a1		; search the stack for FOR or GOSUB activity
					; exit with z=1 if FOR else exit with z=0
					; return modified stack in a2
	cmp.w	#tk_gosub,d0		; compare with GOSUB token
	bne	lab_rger		; branch if no matching GOSUB

	addq.w	#2,a2			; adjust for token
	movea.l	a2,sp			; dump calling addresses & token
	move.l	(sp)+,clinel		; pull current line
	move.l	(sp)+,a5		; pull BASIC execute pointer
					; now do perform "DATA" statement as we could be
					; returning into the middle of an ON <var> GOSUB
					; n,m,p,q line (the return address used by the
					; DATA statement is the one pushed before the
					; GOSUB was executed!)

; perform DATA

lab_data:
	bsr.l	lab_snbs		; scan for next BASIC statement ([:] or [EOL])
					; returns a0 as pointer to [:] or [EOL]
	movea.l	a0,a5			; skip rest of statement
rts_007:
	rts

; scan for next BASIC statement ([:] or [EOL])
; returns a0 as pointer to [:] or [EOL]

lab_snbs:
	movea.l	a5,a0			; copy BASIC execute pointer
	moveq	#0x22,d1			; set string quote character
	bra.s	lab_172d		; go do search

lab_172c:
	cmp.b	#0x3a,d0			; compare with ":"
	beq.s	rts_007a		; exit if found

	cmp.b	d1,d0			; compare current character with string quote
	beq.s	lab_1725		; if found go search for [EOL]

lab_172d:
	move.b	(a0)+,d0		; get next byte
	bne.s	lab_172c		; loop if not null [EOL]

rts_007a:
	subq.w	#1,a0			; correct pointer
	rts

lab_1723:
	cmp.b	d1,d0			; compare current character with string quote
	beq.s	lab_172d		; if found go search for ":" or [EOL]

lab_1725:
	move.b	(a0)+,d0		; get next byte
	bne.s	lab_1723		; loop if not null [EOL]

	bra.s	rts_007a		; correct pointer & return

; perform IF

lab_if:
	bsr	lab_evex		; evaluate expression
	bsr	lab_gbyt		; scan memory
	cmp.b	#tk_goto,d0		; compare with "GOTO" token
	beq.s	lab_174b		; jump if was "GOTO"

					; wasn't IF ... GOTO so must be IF ... THEN
	moveq	#tk_then-0x100,d0	; get THEN token
	bsr	lab_scca		; scan for CHR$(d0), else syntax error/warm start
lab_174b:
	move.b	fac1_e,d0		; get FAC1 exponent
	bne	lab_1754		; branch if result was non zero
					; else ....
; perform REM, skip (rest of) line

lab_rem:
	tst.b	(a5)+			; test byte & increment pointer
	bne.s	lab_rem			; loop if not EOL

	subq.w	#1,a5			; correct pointer
	rts

; perform ON

lab_on:
	bsr	lab_gtby		; get byte parameter, result in d0 and Itemp
	move.b	d0,d2			; copy byte
	bsr	lab_gbyt		; restore BASIC byte
	move.w	d0,-(sp)		; push GOTO/GOSUB token
	cmp.b	#tk_gosub,d0		; compare with GOSUB token
	beq.s	lab_176c		; branch if GOSUB

	cmp.b	#tk_goto,d0		; compare with GOTO token
	bne	lab_sner		; if not GOTO do syntax error, then warm start

; next character was GOTO or GOSUB

lab_176c:
	subq.b	#1,d2			; decrement index (byte value)
	bne.s	lab_1773		; branch if not zero

	move.w	(sp)+,d0		; pull GOTO/GOSUB token
	bra	lab_1602		; go execute it

lab_1773:
	bsr	lab_igby		; increment & scan memory
	bsr.s	lab_gfpn		; get fixed-point number into temp integer
					; (skip this n)
	cmp.b	#0x2c,d0			; compare next character with ","
	beq.s	lab_176c		; loop if ","

	move.w	(sp)+,d0		; pull GOTO/GOSUB token (run out of options)
	rts				; and exit

; get fixed-point number into temp integer
; interpret number from (a5), leave (a5) pointing to byte after #

lab_gfpn:
	moveq	#0x00,d1			; clear integer register
	move.l	d1,d0			; clear d0
	bsr	lab_gbyt		; scan memory, Cb=1 if "0"-"9", & get byte
	bcc.s	lab_1786		; return if carry clear, chr was not "0"-"9"

	move.l	d2,-(sp)		; save d2
lab_1785:
	move.l	d1,d2			; copy integer register
	add.l	d1,d1			; *2
	bcs	lab_sner		; if overflow do syntax error, then warm start

	add.l	d1,d1			; *4
	bcs	lab_sner		; if overflow do syntax error, then warm start

	add.l	d2,d1			; *1 + *4
	bcs	lab_sner		; if overflow do syntax error, then warm start

	add.l	d1,d1			; *10
	bcs	lab_sner		; if overflow do syntax error, then warm start

	sub.b	#0x30,d0			; subtract $30 from byte
	add.l	d0,d1			; add to integer register (top 24 bits always clear)
	bvs	lab_sner		; if overflow do syntax error, then warm start
					; (makes max line # 2147483647)
	bsr	lab_igby		; increment & scan memory
	bcs.s	lab_1785		; loop for next character if "0"-"9"

	move.l	(sp)+,d2		; restore d2
lab_1786:
	move.l	d1,itemp		; save Itemp
	rts

; perform DEC

lab_dec:
	move.w	#0x8180,-(sp)		; set -1 sign/exponent
	bra.s	lab_17b7		; go do DEC

; perform INC

lab_inc:
	move.w	#0x8100,-(sp)		; set 1 sign/exponent
lab_17b7:
	bsr	lab_gvar		; get var address
					; return pointer to variable in Cvaral and a0
	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	bmi	lab_tmer		; if string do "Type mismatch" error/warm start

	bne.s	lab_inci		; go do integer INC/DEC

	move.l	a0,lvarpl		; save var address
	bsr	lab_ufac		; unpack memory (a0) into FAC1
	move.l	#0x80000000,fac2_m	; set FAC2 mantissa for 1
	move.w	(sp),d0			; move exponent & sign to d0
	move.w	d0,fac2_e		; move exponent & sign to FAC2
	move.b	fac1_s,fac_sc		; make sign compare = FAC1 sign
	eor.b	d0,fac_sc		; make sign compare (FAC1_s EOR FAC2_s)
	bsr	lab_add			; add FAC2 to FAC1
	bsr	lab_pfac		; pack FAC1 into variable (Lvarpl)
lab_inct:
	bsr	lab_gbyt		; scan memory
	cmpi.b	#0x2c,d0			; compare with ","
	beq.s	lab_17b8		; continue if "," (another variable to do)

	addq.w	#2,sp			; else dump sign & exponent
	rts
					; was "," so another INCR variable to do
lab_17b8:
	bsr	lab_igby		; increment and scan memory
	bra.s	lab_17b7		; go do next var

lab_inci:
	tst.b	1(sp)			; test sign
	bne.s	lab_deci		; branch if DEC

	addq.l	#1,(a0)			; increment variable
	bra.s	lab_inct		; go scan for more

lab_deci:
	subq.l	#1,(a0)			; decrement variable
	bra.s	lab_inct		; go scan for more


; perform LET

lab_let:
	bsr	lab_gvar		; get variable address
					; return pointer to variable in Cvaral and a0
	move.l	a0,lvarpl		; save variable address
	move.b	dtypef,-(sp)		; push var data type, $80=string, $40=int, $00=float
	moveq	#tk_equal-0x100,d0	; get = token
	bsr	lab_scca		; scan for CHR$(d0), else syntax error/warm start
	bsr	lab_evex		; evaluate expression
	move.b	dtypef,d0		; copy expression data type
	move.b	(sp)+,dtypef		; pop variable data type
	rol.b	#1,d0			; set carry if expression type = string
	bsr	lab_cktm		; type match check, set C for string
	beq	lab_pfac		; if number pack FAC1 into variable Lvarpl & RET

; string LET

lab_17d5:
	movea.l	lvarpl,a2		; get pointer to variable
lab_17d6:
	movea.l	fac1_m,a0		; get descriptor pointer
	movea.l	(a0),a1			; get string pointer
;	cmp.l	smeml,a1		* compare bottom of memory with string pointer
;	bcs.s	lab_1810		* if string was in utility memory copy it

	cmp.l	sstorl,a1		; compare string memory start with string pointer
	bcs.s	lab_1811		; if it was in program memory assign value & exit

	cmpa.l	sfncl,a0		; compare functions start with descriptor pointer
	bcs.s	lab_1811		; branch if >= (string is on stack)

					; string is variable$, make space and copy string
lab_1810:
	moveq	#0,d1			; clear length
	move.w	4(a0),d1		; get string length
	movea.l	(a0),a0			; get string pointer
	bsr	lab_20c9		; copy string
	movea.l	fac1_m,a0		; get descriptor pointer back
					; clean stack & assign value to string variable
lab_1811:
	cmpa.l	a0,a4			; is string on the descriptor stack
	bne.s	lab_1813		; skip pop if not

	addq.w	#0x06,a4			; else update stack pointer
lab_1813:
	move.l	(a0)+,(a2)+		; save pointer to variable
	move.w	(a0),(a2)		; save length to variable
rts_008:
	rts

; perform GET

lab_get:
	bsr	lab_gvar		; get var address
					; return pointer to variable in Cvaral and a0
	move.l	a0,lvarpl		; save variable address as GET variable
	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	bmi.s	lab_gets		; go get string character

					; was numeric get
	bsr	inget			; get input byte
	bsr	lab_1fd0		; convert d0 to unsigned byte in FAC1
	bra	lab_pfac		; pack FAC1 into variable (Lvarpl) & return

lab_gets:
	moveq	#0x00,d1			; assume no byte
	bsr	inget			; get input byte
	bcc.s	lab_nobyte		; branch if no byte received

	moveq	#0x01,d1			; string is single byte
lab_nobyte:
	bsr	lab_2115		; make string space d1 bytes long
					; return a0 = pointer, other registers unchanged
	beq.s	lab_nost		; skip store if null string (or will write over $00)

	move.b	d0,(a0)			; save byte in string (byte IS string!)
lab_nost:
	bsr	lab_rtst		; push string on descriptor stack
					; a0 = pointer, d1 = length

	bra.s	lab_17d5		; do string LET & return

; PRINT

lab_1829:
	bsr	lab_18c6		; print string from stack
lab_182c:
	bsr	lab_gbyt		; scan memory

; perform PRINT

lab_print:
	beq.s	lab_crlf		; if nothing following just print CR/LF

lab_1831:
	beq.s	rts_008			; exit if nothing more to print

	cmp.b	#tk_tab,d0		; compare with TAB( token
	beq.s	lab_18a2		; go do TAB/SPC

	cmp.b	#tk_spc,d0		; compare with SPC( token
	beq.s	lab_18a2		; go do TAB/SPC

	cmp.b	#',,d0			; compare with ","
	beq.s	lab_188b		; go do move to next TAB mark

	cmp.b	#';,d0			; compare with ";"
	beq	lab_18bd		; if ";" continue with PRINT processing

	bsr	lab_evex		; evaluate expression
	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	bmi.s	lab_1829		; branch if string

;* replace the two lines above with this code

;*	move.b	dtypef,d0		* get data type flag, $80=string, $00=numeric
;*	bmi.s	lab_1829		* branch if string

	bsr	lab_2970		; convert FAC1 to string
	bsr	lab_20ae		; print " terminated string to FAC1 stack

; don't check fit if terminal width byte is zero

	moveq	#0,d0			; clear d0
	move.b	twidth,d0		; get terminal width byte
	beq.s	lab_185e		; skip check if zero

	sub.b	7(a4),d0		; subtract string length
	sub.b	tpos,d0			; subtract terminal position
	bcc.s	lab_185e		; branch if less than terminal width

	bsr.s	lab_crlf		; else print CR/LF
lab_185e:
	bsr.s	lab_18c6		; print string from stack
	bra.s	lab_182c		; always go continue processing line

; CR/LF return to BASIC from BASIC input handler
; leaves a0 pointing to the buffer start

lab_1866:
	move.b	#0x00,(a0,d1.w)		; null terminate input

; print CR/LF

lab_crlf:
	moveq	#0x0d,d0			; load [CR]
	bsr.s	lab_prna		; go print the character
	moveq	#0x0a,d0			; load [LF]
	bra.s	lab_prna		; go print the character & return (always branch)

lab_188b:
	move.b	tpos,d2			; get terminal position
	cmp.b	iclim,d2		; compare with input column limit
	bcs.s	lab_1898		; branch if less than Iclim

	bsr.s	lab_crlf		; else print CR/LF (next line)
	bra.s	lab_18bd		; continue with PRINT processing (branch always)

lab_1898:
	sub.b	tabsiz,d2		; subtract TAB size
	bcc.s	lab_1898		; loop if result was >= 0

	neg.b	d2			; twos complement it
	bra.s	lab_18b7		; print d2 spaces

					; do TAB/SPC
lab_18a2:
	move.w	d0,-(sp)		; save token
	bsr	lab_sgby		; increment and get byte, result in d0 and Itemp
	move.w	d0,d2			; copy byte
	bsr	lab_gbyt		; get basic byte back
	cmp.b	#0x29,d0			; is next character ")"
	bne	lab_sner		; if not do syntax error, then warm start

	move.w	(sp)+,d0		; get token back
	cmp.b	#tk_tab,d0		; was it TAB ?
	bne.s	lab_18b7		; branch if not (was SPC)

					; calculate TAB offset
	sub.b	tpos,d2			; subtract terminal position
	bcs.s	lab_18bd		; branch if result was < 0 (can't TAB backwards)

	beq.s	lab_18bd		; branch if result was = $0 (already here)

					; print d2 spaces
lab_18b7:
	tst.b	d2			; test count
	beq.s	lab_18bd		; branch if zero

	subq.b	#1,d2			; adjust for DBF loop
	moveq	#0x20,d0			; load " "
lab_18b8:
	bsr.s	lab_prna		; go print
	dbf	d2,lab_18b8		; decrement count and loop if not all done

					; continue with PRINT processing
lab_18bd:
	bsr	lab_igby		; increment & scan memory
	bra	lab_1831		; continue executing PRINT

; print null terminated string from a0

lab_18c3:
	bsr	lab_20ae		; print terminated string to FAC1/stack

; print string from stack

lab_18c6:
	bsr	lab_22b6		; pop string off descriptor stack or from memory
					; returns with d0 = length, a0 = pointer
	move.w	d0,d1			; copy length & set Z flag
	beq.s	rts_009			; exit (RTS) if null string

	subq.w	#1,d1			; -1 for BF loop
lab_18cd:
	move.b	(a0)+,d0		; get byte from string
	bsr.s	lab_prna		; go print the character
	dbf	d1,lab_18cd		; decrement count and loop if not done yet

rts_009:
	rts

; print "?" character

lab_18e3:
	moveq	#0x3f,d0			; load "?" character

; print character in d0, includes the null handler and infinite line length code
; changes no registers.

lab_prna:
	move.l	d1,-(sp)		; save d1
	cmp.b	#0x20,d0			; compare with " "
	bcs.s	lab_18f9		; branch if less, non printing character

					; don't check fit if terminal width byte is zero
	move.b	twidth,d1		; get terminal width
	bne.s	lab_18f0		; branch if not zero (not infinite length)

					; is "infinite line" so check TAB position
	move.b	tpos,d1			; get position
	sub.b	tabsiz,d1		; subtract TAB size
	bne.s	lab_18f7		; skip reset if different

	move.b	d1,tpos			; else reset position
	bra.s	lab_18f7		; go print character

lab_18f0:
	cmp.b	tpos,d1			; compare with terminal character position
	bne.s	lab_18f7		; branch if not at end of line

	move.l	d0,-(sp)		; save d0
	bsr	lab_crlf		; else print CR/LF
	move.l	(sp)+,d0		; restore d0
lab_18f7:
	addq.b	#0x01,tpos		; increment terminal position
lab_18f9:
	jsr	v_outp			; output byte via output vector
	cmp.b	#0x0d,d0			; compare with [CR]
	bne.s	lab_188a		; branch if not [CR]

					; else print nullct nulls after the [CR]
	moveq	#0x00,d1			; clear d1
	move.b	nullct,d1		; get null count
	beq.s	lab_1886		; branch if no nulls

	moveq	#0x00,d0			; load [NULL]
lab_1880:
	jsr	v_outp			; go print the character
	dbf	d1,lab_1880		; decrement count and loop if not all done

	moveq	#0x0d,d0			; restore the character
lab_1886:
	move.b	d1,tpos			; clear terminal position
lab_188a:
	move.l	(sp)+,d1		; restore d1
	rts

; handle bad input data

lab_1904:
	tst.b	imode			; test input mode flag, $00=INPUT, $98=READ
	bpl.s	lab_1913		; branch if INPUT (go do redo)

	move.l	dlinel,clinel		; save DATA line as current line
	bra	lab_sner		; do syntax error, then warm start

					; mode was INPUT
lab_1913:
	lea	lab_redo(pc),a0	; point to redo message
	bsr	lab_18c3		; print null terminated string from memory
	movea.l	cpntrl,a5		; save continue pointer as BASIC execute pointer
	rts

; perform INPUT

lab_input:
	bsr	lab_ckrn		; check not Direct (back here if ok)
	cmp.b	#0x22,d0			; compare next byte with open quote
	bne.s	lab_1934		; branch if no prompt string

	bsr	lab_1bc1		; print "..." string
	moveq	#0x3b,d0			; load d0 with ";"
	bsr	lab_scca		; scan for CHR$(d0), else syntax error/warm start
	bsr	lab_18c6		; print string from Sutill/Sutilh
					; done with prompt, now get data
lab_1934:
	bsr	lab_inln		; print "? " and get BASIC input
					; return a0 pointing to the buffer start
	moveq	#0,d0			; clear d0 (flag INPUT)
	tst.b	(a0)			; test first byte from buffer
	bne.s	lab_1953		; branch if not null input

	and	#0xfe,ccr		; was null input so clear carry to exit prog
	bra	lab_1647		; go do BREAK exit

; perform READ

lab_read:
	movea.l	dptrl,a0		; get DATA pointer
	moveq	#0x98-0x100,d0		; flag READ
lab_1953:
	move.b	d0,imode		; set input mode flag, $00=INPUT, $98=READ
	move.l	a0,rdptrl		; save READ pointer

					; READ or INPUT next variable from list
lab_195b:
	bsr	lab_gvar		; get (var) address
					; return pointer to variable in Cvaral and a0
	move.l	a0,lvarpl		; save variable address as LET variable
	move.l	a5,-(sp)		; save BASIC execute pointer
	movea.l	rdptrl,a5		; set READ pointer as BASIC execute pointer
	bsr	lab_gbyt		; scan memory
	bne.s	lab_1986		; branch if not null

					; pointer was to null entry
	tst.b	imode			; test input mode flag, $00=INPUT, $98=READ
	bmi.s	lab_19dd		; branch if READ (go find next statement)

					; mode was INPUT
	bsr	lab_18e3		; print "?" character (double ? for extended input)
	bsr	lab_inln		; print "? " and get BASIC input
					; return a0 pointing to the buffer start
	tst.b	(a0)			; test first byte from buffer
	bne.s	lab_1984		; branch if not null input

	and	#0xfe,ccr		; was null input so clear carry to exit prog
	bra	lab_1647		; go do BREAK exit

lab_1984:
	movea.l	a0,a5			; set BASIC execute pointer to buffer start
	subq.w	#1,a5			; decrement pointer
lab_1985:
	bsr	lab_igby		; increment & scan memory
lab_1986:
	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	bpl.s	lab_19b0		; branch if numeric

					; else get string
	move.b	d0,d2			; save search character
	cmp.b	#0x22,d0			; was it " ?
	beq.s	lab_1999		; branch if so

	moveq	#':,d2			; set new search character
	moveq	#',,d0			; other search character is ","
	subq.w	#1,a5			; decrement BASIC execute pointer
lab_1999:
	addq.w	#1,a5			; increment BASIC execute pointer
	move.b	d0,d3			; set second search character
	movea.l	a5,a0			; BASIC execute pointer is source

	bsr	lab_20b4		; print d2/d3 terminated string to FAC1 stack
					; d2 = Srchc, d3 = Asrch, a0 is source
	movea.l	a2,a5			; copy end of string to BASIC execute pointer
	bsr	lab_17d5		; go do string LET
	bra.s	lab_19b6		; go check string terminator

					; get numeric INPUT
lab_19b0:
	move.b	dtypef,-(sp)		; save variable data type
	bsr	lab_2887		; get FAC1 from string
	move.b	(sp)+,dtypef		; restore variable data type
	bsr	lab_pfac		; pack FAC1 into (Lvarpl)
lab_19b6:
	bsr	lab_gbyt		; scan memory
	beq.s	lab_19c2		; branch if null (last entry)

	cmp.b	#',,d0			; else compare with ","
	bne	lab_1904		; if not "," go handle bad input data

	addq.w	#1,a5			; else was "," so point to next chr
					; got good input data
lab_19c2:
	move.l	a5,rdptrl		; save read pointer for now
	movea.l	(sp)+,a5		; restore execute pointer
	bsr	lab_gbyt		; scan memory
	beq.s	lab_1a03		; if null go do extra ignored message

	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bra	lab_195b		; go INPUT next variable from list

					; find next DATA statement or do "OD" error
lab_19dd:
	bsr	lab_snbs		; scan for next BASIC statement ([:] or [EOL])
					; returns a0 as pointer to [:] or [EOL]
	movea.l	a0,a5			; add index, now = pointer to [EOL]/[EOS]
	addq.w	#1,a5			; pointer to next character
	cmp.b	#':,d0			; was it statement end?
	beq.s	lab_19f6		; branch if [:]

					; was [EOL] so find next line

	move.w	a5,d1			; past pad byte(s)
	and.w	#1,d1			; mask odd bit
	add.w	d1,a5			; add pointer
	move.l	(a5)+,d2		; get next line pointer
	beq	lab_oder		; branch if end of program

	move.l	(a5)+,dlinel		; save current DATA line
lab_19f6:
	bsr	lab_gbyt		; scan memory
	cmp.b	#tk_data,d0		; compare with "DATA" token
	beq	lab_1985		; was "DATA" so go do next READ

	bra.s	lab_19dd		; go find next statement if not "DATA"

; end of INPUT/READ routine

lab_1a03:
	movea.l	rdptrl,a0		; get temp READ pointer
	tst.b	imode			; get input mode flag, $00=INPUT, $98=READ
	bpl.s	lab_1a0e		; branch if INPUT

	move.l	a0,dptrl		; else save temp READ pointer as DATA pointer
	rts

					; we were getting INPUT
lab_1a0e:
	tst.b	(a0)			; test next byte
	bne.s	lab_1a1b		; error if not end of INPUT

	rts
					; user typed too much
lab_1a1b:
	lea	lab_imsg(pc),a0	; point to extra ignored message
	bra	lab_18c3		; print null terminated string from memory & RTS

; perform NEXT

lab_next:
	bne.s	lab_1a46		; branch if NEXT var

	movea.w	#0,a0			; else clear a0
	bra.s	lab_1a49		; branch always (no variable to search for)

; NEXT var

lab_1a46:
	bsr	lab_gvar		; get variable address
					; return pointer to variable in Cvaral and a0
lab_1a49:
	move.l	a0,frnxtl		; store variable pointer
	bsr	lab_11a1		; search the stack for FOR or GOSUB activity
					; exit with z=1 if FOR else exit with z=0
					; return modified stack in a2
	bne	lab_nfer		; if not found do next without for err/warm start

	movea.l	a2,sp			; set stack pointer (dumps return addresses)
	move.w	6(a2),fac2_e		; get STEP value exponent and sign
	move.l	8(a2),fac2_m		; get STEP value mantissa

	movea.l	frnxtl,a0		; get FOR variable pointer
	move.b	18(a2),dtypef		; restore FOR variable data type
	bsr	lab_1c19		; check type and unpack (a0)

	move.b	fac2_s,fac_sc		; save FAC2 sign as sign compare
	move.b	fac1_s,d0		; get FAC1 sign
	eor.b	d0,fac_sc		; EOR to create sign compare

	bsr	lab_add			; add STEP value to FOR variable
	move.b	18(a2),dtypef		; restore FOR variable data type (again)
	bsr	lab_pfac		; pack FAC1 into FOR variable

	move.w	12(a2),fac2_e		; get TO value exponent and sign
	move.l	14(a2),fac2_m		; get TO value mantissa

	move.b	fac2_s,fac_sc		; save FAC2 sign as sign compare
	move.b	fac1_s,d0		; get FAC1 sign
	eor.b	d0,fac_sc		; EOR to create sign compare

	bsr	lab_27fa		; compare FAC1 with FAC2 (TO value)
					; returns d0=+1 if FAC1 > FAC2
					; returns d0= 0 if FAC1 = FAC2
					; returns d0=-1 if FAC1 < FAC2

	move.w	6(a2),d1		; get STEP value exponent and sign
	eor.w	d0,d1			; EOR compare result with STEP (exponent and sign)

	tst.b	d0			; test for =
	beq.s	lab_1a90		; branch if = (loop INcomplete)

	tst.b	d1			; test result
	bpl.s	lab_1a9b		; branch if > (loop complete)

					; loop back and do it all again
lab_1a90:
	move.l	20(a2),clinel		; reset current line
	move.l	24(a2),a5		; reset BASIC execute pointer
	bra	lab_15c2		; go do interpreter inner loop

					; loop complete so carry on
lab_1a9b:
	adda.w	#28,a2			; add 28 to dump FOR structure
	movea.l	a2,sp			; copy to stack pointer
	bsr	lab_gbyt		; scan memory
	cmp.b	#0x2c,d0			; compare with ","
	bne	lab_15c2		; if not "," go do interpreter inner loop

					; was "," so another NEXT variable to do
	bsr	lab_igby		; else increment & scan memory
	bsr	lab_1a46		; do NEXT (var)

; evaluate expression & check is numeric, else do type mismatch

lab_evnm:
	bsr.s	lab_evex		; evaluate expression

; check if source is numeric, else do type mismatch

lab_ctnm:
	cmp.w	d0,d0			; required type is numeric so clear carry
	bra.s	lab_cktm		; go check type match

; check if source is string, else do type mismatch

lab_ctst:
	ori.b	#1,ccr			; required type is string so set carry

; type match check, set C for string, clear C for numeric

lab_cktm:
	btst.b	#7,dtypef		; test data type flag, don't change carry
	bne.s	lab_1aba		; branch if data type is string

					; else data type was numeric
	bcs	lab_tmer		; if required type is string do type mismatch err

	rts
					; data type was string, now check required type
lab_1aba:
	bcc	lab_tmer		; if required type is numeric do type mismatch error

	rts

; evaluate expression

lab_evex:
	subq.w	#1,a5			; decrement BASIC execute pointer
	moveq	#0,d1			; clear precedence word
	bra.s	lab_1acd		; enter loop

lab_1acc:
	move.w	d0,-(sp)		; push compare evaluation byte if branch to here
lab_1acd:
	move.w	d1,-(sp)		; push precedence word

;	moveq	#2,d0			* 2 bytes !!
;	bsr.s	lab_1212		* check room on stack for d0 bytes

	bsr	lab_gval		; get value from line
	move.b	#0x00,comp_f		; clear compare function flag
lab_1adb:
	bsr	lab_gbyt		; scan memory
lab_1ade:
	sub.b	#tk_gt,d0		; subtract token for > (lowest compare function)
	bcs.s	lab_1afa		; branch if < TK_GT

	cmp.b	#0x03,d0			; compare with ">" to "<" tokens
	bcs.s	lab_1ae0		; branch if < TK_SGN (is compare function)

	tst.b	comp_f			; test compare function flag
	bne.s	lab_1b2a		; branch if compare function

	bra	lab_1b78		; go do functions

					; was token for > = or < (d0 = 0, 1 or 2)
lab_1ae0:
	moveq	#1,d1			; set to 0000 0001
	asl.b	d0,d1			; 1 if >, 2 if =, 4 if <
	move.b	comp_f,d0		; copy old compare function flag
	move.b	d1,comp_f		; save this compare function bit
	eor.b	d0,comp_f		; EOR in the old compare function flag
	cmp.b	comp_f,d0		; compare old with new compare function flag
	bcc	lab_sner		; if <=(new comp_f) do syntax error/warm start
					; was more than one <, = or >)
	bsr	lab_igby		; increment & scan memory
	bra.s	lab_1ade		; go do next character

					; token is < ">" or > "<" tokens
lab_1afa:
	tst.b	comp_f			; test compare function flag
	bne.s	lab_1b2a		; branch if compare function

					; was <  TK_GT so is operator or lower
	add.b	#(tk_gt-tk_plus),d0	; add # of operators (+ - * / ^ AND OR EOR)
	bcc	lab_1b78		; branch if < + operator

	bne.s	lab_1b0b		; branch if not + token

	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	bmi	lab_224d		; type is string & token was +

lab_1b0b:
	mulu	#6,d0			; *6
	moveq	#0,d1			; clear longword
	move.b	d0,d1			; copy to index
lab_1b13:
	move.w	(sp)+,d0		; pull previous precedence
	lea	lab_oppt(pc),a0	; set pointer to operator table
	cmp.w	(a0,d1.w),d0		; compare with this opperator precedence
	bcc	lab_1b7d		; branch if previous precedence (d0) >=

	bsr	lab_ctnm		; check if source is numeric, else type mismatch
lab_1b1c:
	move.w	d0,-(sp)		; save precedence
lab_1b1d:
	bsr.s	lab_1b43		; get vector, set-up operator, continue evaluation
	move.w	(sp)+,d0		; restore precedence
	move.l	prstk,d1		; get stacked function pointer
	bpl.s	lab_1b3c		; branch if stacked values

	move.w	d0,d0			; copy precedence (set flags)
	beq.s	lab_1b7b		; exit if done

	bra.l	lab_1b86		; else pop FAC2 & return (do function)

					; was compare function (< = >)
lab_1b2a:
	move.b	dtypef,d0		; get data type flag
	move.b	comp_f,d1		; get compare function flag
;	lsl.b	#1,d0			* string bit flag into X bit
;	roxl.b	#1,d1			* shift compare function flag

	add.b	d0,d0			; string bit flag into X bit
	addx.b	d1,d1			; shift compare function flag

	move.b	#0,dtypef		; clear data type flag, $00=float
	move.b	d1,comp_f		; save new compare function flag
	subq.w	#1,a5			; decrement BASIC execute pointer
	moveq	#(tk_lt-tk_plus)*6,d1	; set offset to last operator entry
	bra.s	lab_1b13		; branch always

lab_1b3c:
	lea	lab_oppt(pc),a0	; point to function vector table
	cmp.w	(a0,d1.w),d0		; compare with this opperator precedence
	bcc.s	lab_1b86		; branch if d0 >=, pop FAC2 & return

	bra.s	lab_1b1c		; branch always

; get vector, set up operator then continue evaluation

lab_1b43:
	lea	lab_oppt(pc),a0	; point to operator vector table
	move.l	2(a0,d1.w),-(sp)	; put vector on stack
	bsr.s	lab_1b56		; function set up will return here, then the
					; next RTS will call the function
	move.b	comp_f,d0		; get compare function flag
	bra	lab_1acc		; continue evaluating expression

lab_1b56:
	move.b	fac1_s,d0		; get FAC1 sign (b7)
	move.w	(a0,d1.w),d1		; get precedence value
	move.l	(sp)+,ut1_pl		; copy return address to utility pointer
	move.b	d0,fac1_s		; set sign
	move.l	fac1_m,-(sp)		; push FAC1 mantissa
	move.w	fac1_e,-(sp)		; push sign and exponent
	move.l	ut1_pl,-(sp)		; push address
	rts				; return

; do functions

lab_1b78:
	moveq	#-1,d1			; flag all done
	move.w	(sp)+,d0		; pull precedence word
lab_1b7b:
	beq.s	lab_1b9d		; exit if done

lab_1b7d:
	cmp.w	#0x64,d0			; compare previous precedence with $64
	beq.s	lab_1b84		; branch if was $64 (< function can be string)

	bsr	lab_ctnm		; check if source is numeric, else type mismatch
lab_1b84:
	move.l	d1,prstk		; save current operator index

					; pop FAC2 & return
lab_1b86:
	move.w	(sp)+,d0		; pop comparison evaluation
	move.b	d0,d1			; copy comparison evaluation flag
	lsr.b	#1,d0			; shift out comparison evaluation lowest bit
	move.b	d0,cflag		; save comparison evaluation flag
	move.w	(sp)+,fac2_e		; pop exponent and sign
	move.l	(sp)+,fac2_m		; pop mantissa
	move.b	fac2_s,fac_sc		; copy FAC2 sign
	move.b	fac1_s,d0		; get FAC1 sign
	eor.b	d0,fac_sc		; EOR FAC1 sign and set sign compare

	lsr.b	#1,d1			; type bit into X and C
	rts

lab_1b9d:
	move.b	fac1_e,d0		; get FAC1 exponent
	rts

; get value from line

lab_gval:
	move.b	#0x00,dtypef		; clear data type flag, $00=float
lab_1ba4:
	bsr.l	lab_igby		; increment & scan memory
	bcs	lab_2887		; if numeric get FAC1 from string & return

	tst.b	d0			; test byte
	bmi	lab_1bd0		; if -ve go test token values

					; else is either string, number, variable or (<expr>)
	cmp.b	#'$,d0			; compare with "$"
	beq	lab_2887		; if "$" get hex number from string & return

	cmp.b	#'%,d0			; else compare with "%"
	beq	lab_2887		; if "%" get binary number from string & return

	cmp.b	#0x2e,d0			; compare with "."
	beq	lab_2887		; if so get FAC1 from string & return (e.g. .123)

					; wasn't a number so ...
	cmp.b	#0x22,d0			; compare with "
	bne.s	lab_1bf3		; if not open quote must be variable or open bracket

					; was open quote so get the enclosed string

; print "..." string to string stack

lab_1bc1:
	addq.w	#1,a5			; increment basic execute pointer (past ")
	movea.l	a5,a0			; copy basic execute pointer (string start)
	bsr	lab_20ae		; print " terminated string to stack
	movea.l	a2,a5			; restore BASIC execute pointer from temp
	rts

; get value from line .. continued
					; wasn't any sort of number so ...
lab_1bf3:
	cmp.b	#'(,d0			; compare with "("
	bne.s	lab_1c18		; if not "(" get (var), return value in FAC1 & $ flag

	addq.w	#1,a5			; increment execute pointer

; evaluate expression within parentheses

lab_1bf7:
	bsr	lab_evex		; evaluate expression

; all the 'scan for' routines return the character after the sought character

; scan for ")" , else do syntax error, then warm start

lab_1bfb:
	moveq	#0x29,d0			; load d0 with ")"
	bra.s	lab_scca

; scan for "(" , else do syntax error, then warm start

lab_1bfe:
	moveq	#0x28,d0			; load d0 with "("
	bra.s	lab_scca

; scan for "," , else do syntax error, then warm start

lab_1c01:
	moveq	#0x2c,d0			; load d0 with ","

; scan for CHR$(d0) , else do syntax error, then warm start

lab_scca:
	cmp.b	(a5),d0			; check next byte is = d0
	bne	lab_sner		; if not do syntax error/warm start

					; else get next BASIC byte

; BASIC increment and scan memory routine

lab_igby:
	addq.w	#1,a5			; increment pointer

; scan memory routine, exit with Cb = 1 if numeric character
; also skips any spaces encountered

lab_gbyt:
	move.b	(a5),d0			; get byte

	cmp.b	#0x20,d0			; compare with " "
	beq.s	lab_igby		; if " " go do next

; test current BASIC byte, exit with Cb = 1 if numeric character

lab_tbyt:
	cmp.b	#0x3a,d0			; compare with ":"
	bcc.s	rts_001			; exit if >= (not numeric, carry clear)

	subi.b	#0x30,d0			; subtract "0"
	subi.b	#0xd0,d0			; subtract -"0"
rts_001:					; carry set if byte = "0"-"9"
	rts

; set-up for - operator

lab_1c11:
	move.w	#(tk_gt-tk_plus)*6,d1	; set offset from base to - operator
lab_1c13:
	addq.w	#4,sp			; dump GVAL return address
	bra	lab_1b1d		; continue evaluating expression

; variable name set-up
; get (var), return value in FAC_1 & data type flag

lab_1c18:
	bsr	lab_gvar		; get (var) address
					; return pointer to variable in Cvaral and a0
lab_1c19:
	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	beq	lab_ufac		; if float unpack memory (a0) into FAC1 & return

	bpl.s	lab_1c1a		; if integer unpack memory (a0) into FAC1 & return

	move.l	a0,fac1_m		; save address in FAC1
	rts

lab_1c1a:
	move.l	(a0),d0			; get integer value
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & return

; get value from line .. continued
; do tokens

lab_1bd0:
	cmp.b	#tk_minus,d0		; compare with token for -
	beq.s	lab_1c11		; branch if - token (do set-up for - operator)

					; wasn't -123 so ...
	cmp.b	#tk_plus,d0		; compare with token for +
	beq	lab_1ba4		; branch if + token (+n = n so ignore leading +)

	cmp.b	#tk_not,d0		; compare with token for NOT
	bne.s	lab_1be7		; branch if not token for NOT

					; was NOT token
	move.w	#(tk_equal-tk_plus)*6,d1 ; offset to NOT function
	bra.s	lab_1c13		; do set-up for function then execute

					; wasn't +, - or NOT so ...
lab_1be7:
	cmp.b	#tk_fn,d0		; compare with token for FN
	beq	lab_201e		; if FN go evaluate FNx

					; wasn't +, -, NOT or FN so ...
	cmp.b	#tk_sgn,d0		; compare with token for SGN
	bcs	lab_sner		; if < SGN token then do syntax error

; get value from line ..
; only functions left so ...
; set up function references

lab_1c27:
	and.w	#0x7f,d0			; normalise and mask byte
	asl.w	#2,d0			; *4 (4 bytes per function address)
	move.w	d0,-(sp)		; push offset
	move.w	d0,d1			; copy offset
	bsr	lab_igby		; increment & scan memory
	cmp.w	#(tk_chrs-0x80)*4+1,d1	; compare function offset to CHR$ token offset+1
	bcs.s	lab_1c51		; branch if <HEX$ (can not be =)

; get value from line .. continued
; was HEX$, BIN$, VARPTR, LEFT$, RIGHT$ or MID$ so..

	cmp.w	#(tk_bins-0x80)*4+1,d1	; compare function offset to BIN$ token offset+1
	bcs.s	lab_bhss		; branch if <BITTST (can not be =)

	cmp.w	#(tk_vptr-0x80)*4+1,d1	; compare function offset VARPTR token offset+1
	bcs.s	lab_1c54		; branch if <LEFT$ (can not be =)

; get value from line .. continued
; was LEFT$, RIGHT$ or MID$ so..

	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_evex		; evaluate (should be string) expression
	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bsr	lab_ctst		; check source is string, else do type mismatch
	move.w	(sp)+,d7		; restore offset
	move.l	fac1_m,-(sp)		; push descriptor pointer
	move.w	d7,-(sp)		; push function offset
	bsr	lab_gtwo		; get word parameter, result in d0 and Itemp
	move.w	(sp)+,d7		; restore offset
	move.w	d0,-(sp)		; push word parameter
	move.w	d7,d0			; function offset to d0
	bra.s	lab_1c56		; go call function

; get value from line .. continued
; was BIN$ or HEX$ so ..

lab_bhss:
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	bsr	lab_gbyt		; get next BASIC byte
	moveq	#0,d1			; set default to no leading "0"s
	cmp.b	#'),d0			; compare with close bracket
	beq.s	lab_1c54		; if ")" go do rest of function

	move.l	itemp,-(sp)		; copy longword to stack (number)
	bsr	lab_scgb		; scan for "," and get byte value
	move.l	d0,d1			; copy leading 0s #
	bsr	lab_gbyt		; get next BASIC byte
	cmp.b	#'),d0			; is next character )
	bne	lab_fcer		; if not ")" do function call error/warm start

	move.l	(sp)+,itemp		; restore number form stack
	bra.s	lab_1c54		; go do rest of function

; get value from line .. continued
; was SGN() to CHR$() so..

lab_1c51:
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_1bf7		; evaluate expression within parentheses

					; enter here if VARPTR(), MAX() or MIN()
lab_1c54:
	move.w	(sp)+,d0		; get offset back
lab_1c56:
	lea	lab_ftbl(pc),a0	; pointer to functions vector table
	movea.l	(a0,d0.w),a0		; get function vector
	jsr	(a0)			; go do function vector
	bra	lab_ctnm		; check if source is numeric & RTS, else do
					; type mismatch
; perform EOR

lab_eor:
	bsr.s	getfirst		; get two values for OR, AND or EOR
					; first in d0, and Itemp, second in d2
	eor.l	d2,itemp		; EOR with first value
	move.l	itemp,d0		; get result
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & RET

; perform OR

lab_or:
	bsr.s	getfirst		; get two values for OR, AND or EOR
					; first in d0, and Itemp, second in d2
	or.l	d2,d0			; do OR
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & RET

; perform AND

lab_and:
	bsr.s	getfirst		; get two values for OR, AND or EOR
					; first in d0, and Itemp, second in d2
	and.l	d2,d0			; do AND
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & RET

; get two values for OR, AND, EOR
; first in d0, second in d2

getfirst:
	bsr	lab_evir		; evaluate integer expression (no sign check)
					; result in d0 and Itemp
	move.l	d0,d2			; copy second value
	bsr	lab_279b		; copy FAC2 to FAC1 (get 1st value in expression)
	bsr	lab_evir		; evaluate integer expression (no sign check)
					; result in d0 and Itemp
	rts

; perform NOT

lab_equal:
	bsr	lab_evir		; evaluate integer expression (no sign check)
					; result in d0 and Itemp
	not.l	d0			; bitwise invert
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & RET

; perform comparisons
; do < compare

lab_lthan:
	bsr	lab_cktm		; type match check, set C for string
	bcs.s	lab_1cae		; branch if string

					; do numeric < compare
	bsr	lab_27fa		; compare FAC1 with FAC2
					; returns d0=+1 if FAC1 > FAC2
					; returns d0= 0 if FAC1 = FAC2
					; returns d0=-1 if FAC1 < FAC2
	bra.s	lab_1cf2		; process result

					; do string < compare
lab_1cae:
	move.b	#0x00,dtypef		; clear data type, $80=string, $40=integer, $00=float
	bsr	lab_22b6		; pop string off descriptor stack, or from top of
					; string space returns d0 = length, a0 = pointer
	movea.l	a0,a1			; copy string 2 pointer
	move.l	d0,d1			; copy string 2 length
	movea.l	fac2_m,a0		; get string 1 descriptor pointer
	bsr	lab_22ba		; pop (a0) descriptor, returns with ..
					; d0 = length, a0 = pointer
	move.l	d0,d2			; copy length
	bne.s	lab_1cb5		; branch if not null string

	tst.l	d1			; test if string 2 is null also
	beq.s	lab_1cf2		; if so do string 1 = string 2

lab_1cb5:
	sub.l	d1,d2			; subtract string 2 length
	beq.s	lab_1cd5		; branch if strings = length

	bcs.s	lab_1cd4		; branch if string 1 < string 2

	moveq	#-1,d0			; set for string 1 > string 2
	bra.s	lab_1cd6		; go do character comapare

lab_1cd4:
	move.l	d0,d1			; string 1 length is compare length
	moveq	#1,d0			; and set for string 1 < string 2
	bra.s	lab_1cd6		; go do character comapare

lab_1cd5:
	move.l	d2,d0			; set for string 1 = string 2
lab_1cd6:
	subq.l	#1,d1			; adjust length for DBcc loop

					; d1 is length to compare, d0 is < = > for length
					; a0 is string 1 pointer, a1 is string 2 pointer
lab_1ce6:
	cmpm.b	(a0)+,(a1)+		; compare string bytes (1 with 2)
	dbne	d1,lab_1ce6		; loop if same and not end yet

	beq.s	lab_1cf2		; if = to here, then go use length compare

	bcc.s	lab_1cdb		; else branch if string 1 > string 2

	moveq	#-1,d0			; else set for string 1 < string 2
	bra.s	lab_1cf2		; go set result

lab_1cdb:
	moveq	#1,d0			; and set for string 1 > string 2

lab_1cf2:
	addq.l	#1,d0			; make result 0, 1 or 2
	move.l	d0,d1			; copy to d1
	moveq	#1,d0			; set d0
	rol.l	d1,d0			; make 1, 2 or 4 (result = flag bit)
	and.b	cflag,d0		; AND with comparison evaluation flag
	beq	lab_27db		; exit if not a wanted result (i.e. false)

	moveq	#-1,d0			; else set -1 (true)
	bra	lab_27db		; save d0 as integer & return


lab_1cfe:
	bsr	lab_1c01		; scan for "," , else do syntax error/warm start

; perform DIM

lab_dim:
	moveq	#-1,d1			; set "DIM" flag
	bsr.s	lab_1d10		; search for variable
	bsr	lab_gbyt		; scan memory
	bne.s	lab_1cfe		; loop and scan for "," if not null

	rts

; perform << (left shift)

lab_lshift:
	bsr.s	getpair			; get an integer and byte pair
					; byte is in d2, integer is in d0 and Itemp
	beq.s	noshift			; branch if byte zero

	cmp.b	#0x20,d2			; compare bit count with 32d
	bcc.s	toobig			; branch if >=

	asl.l	d2,d0			; shift longword
noshift:
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & RET

; perform >> (right shift)

lab_rshift:
	bsr.s	getpair			; get an integer and byte pair
					; byte is in d2, integer is in d0 and Itemp
	beq.s	noshift			; branch if byte zero

	cmp.b	#0x20,d2			; compare bit count with 32d
	bcs.s	not2big			; branch if >= (return shift)

	tst.l	d0			; test sign bit
	bpl.s	toobig			; branch if +ve

	moveq	#-1,d0			; set longword
	bra	lab_ayfc		; convert d0 to longword in FAC1 & RET

not2big:
	asr.l	d2,d0			; shift longword
	bra	lab_ayfc		; convert d0 to longword in FAC1 & RET

toobig:
	moveq	#0,d0			; clear longword
	bra	lab_ayfc		; convert d0 to longword in FAC1 & RET

; get an integer and byte pair
; byte is in d2, integer is in d0 and Itemp

getpair:
	bsr	lab_evby		; evaluate byte expression, result in d0 and Itemp
	move.b	d0,d2			; save it
	bsr	lab_279b		; copy FAC2 to FAC1 (get 1st value in expression)
	bsr	lab_evir		; evaluate integer expression (no sign check)
					; result in d0 and Itemp
	tst.b	d2			; test byte value
	rts

; check byte, return C=0 if<"A" or >"Z" or <"a" to "z">

lab_casc:
	cmp.b	#0x61,d0			; compare with "a"
	bcc.s	lab_1d83		; if >="a" go check =<"z"

; check byte, return C=0 if<"A" or >"Z"

lab_1d82:
	cmp.b	#0x5b,d0			; compare with "Z"+1
	bcs.s	lab_1d8a		; if <="Z" go check >="A"

	rts

lab_1d8a:
	sub.b	#0x41,d0			; subtract "A"
	sub.b	#0xbf,d0			; subtract $BF (restore byte)
					; carry set if byte>$40
	rts

lab_1d83:
	add.b	#0x85,d0			; add $85
	add.b	#0x7b,d0			; add "z"+1 (restore byte)
					; carry set if byte<=$7A
	rts

; search for variable
; DIM flag is in d1.b
; return pointer to variable in Cvaral and a0
; stet data type to variable type

lab_gvar:
	moveq	#0x00,d1			; set DIM flag = $00
	bsr	lab_gbyt		; scan memory (1st character)
lab_1d10:
	move.b	d1,defdim		; save DIM flag

; search for FN name entry point

lab_1d12:
	bsr.s	lab_casc		; check byte, return C=0 if<"A" or >"Z"
	bcc	lab_sner		; if not syntax error, then warm start

					; is variable name so ...
	moveq	#0x0,d1			; set index for name byte
	movea.l	#varname,a0		; pointer to variable name
	move.l	d1,(a0)			; clear variable name
	move.b	d1,dtypef		; clear data type, $80=string, $40=integer, $00=float

lab_1d2d:
	cmp.w	#0x04,d1			; done all significant characters?
	bcc.s	lab_1d2e		; if so go ignore any more

	move.b	d0,(a0,d1.w)		; save character
	addq.w	#1,d1			; increment index
lab_1d2e:
	bsr	lab_igby		; increment & scan memory (next character)
	bcs.s	lab_1d2d		; branch if character = "0"-"9" (ok)

					; character wasn't "0" to "9" so ...
	bsr.s	lab_casc		; check byte, return C=0 if<"A" or >"Z"
	bcs.s	lab_1d2d		; branch if = "A"-"Z" (ok)

					; check if string variable
	cmp.b	#'$,d0			; compare with "$"
	bne.s	lab_1d44		; branch if not string

					; type is string
	or.b	#0x80,varname+1		; set top bit of 2nd character (indicate string)
	bsr	lab_igby		; increment & scan memory
	bra.s	lab_1d45		; skip integer check

					; check if integer variable
lab_1d44:
	cmp.b	#'&,d0			; compare with "&"
	bne.s	lab_1d45		; branch if not integer

					; type is integer
	or.b	#0x80,varname+2		; set top bit of 3rd character (indicate integer)
	bsr	lab_igby		; increment & scan memory

; after we have determined the variable type we need to determine
; if it's an array of type

					; gets here with character after var name in d0
lab_1d45:
	tst.b	sufnxf			; test function name flag
	beq.s	lab_1d48		; branch if not FN or FN variable

	bpl.s	lab_1d49		; branch if FN variable

					; else was FN name
	move.l	varname,d0		; get whole function name
	moveq	#8,d1			; set step to next function size -4
	movea.l	#sfncl,a0		; get pointer to start of functions
	bra.s	lab_1d4b		; go find function

lab_1d48:
	sub.b	#'(,d0			; subtract "("
	beq	lab_1e17		; if "(" go find, or make, array

; either find or create var
; var name (1st four characters only!) is in Varname

					; variable name wasn't var( .. look for plain var
lab_1d49:
	move.l	varname,d0		; get whole variable name
lab_1d4a:
	moveq	#4,d1			; set step to next variable size -4
	movea.l	#svarl,a0		; get pointer to start of variables

	btst.l	#23,d0			; test if string name
	beq.s	lab_1d4b		; branch if not

	addq.w	#2,d1			; 10 bytes per string entry
	addq.w	#(sstrl-svarl),a0	; move to string area

lab_1d4b:
	movea.l	4(a0),a1		; get end address
	movea.l	(a0),a0			; get start address
	bra.s	lab_1d5e		; enter loop at exit check

lab_1d5d:
	cmp.l	(a0)+,d0		; compare this variable with name
	beq.s	lab_1dd7		; branch if match (found var)

	adda.l	d1,a0			; add offset to next variable
lab_1d5e:
	cmpa.l	a1,a0			; compare address with variable space end
	bne.s	lab_1d5d		; if not end go check next

					; reached end of variable mem without match
					; ... so create new variable, possibly
	cmpi.l	#lab_1c19,(sp)		; compare return address with expected
	beq	lab_uver		; if RHS get (var) go do error or return null

; This will only branch if the call was from LAB_1C18 and is only called from
; there if it is searching for a variable from the RHS of a LET a=b statement

	btst.b	#0,sufnxf		; test function search flag
	bne	lab_ufer		; if not doing DEF then do undefined function error

					; else create new variable/function
lab_1d98:
	movea.l	earryl,a2		; get end of block to move
	move.l	a2,d2			; copy end of block to move
	sub.l	a1,d2			; calculate block to move size

	movea.l	a2,a0			; copy end of block to move
	addq.l	#4,d1			; space for one variable/function + name
	adda.l	d1,a2			; add space for one variable/function
	move.l	a2,earryl		; set new array mem end
	lsr.l	#1,d2			; /2 for word copy
	beq.s	lab_1daf		; skip move if zero length block

	subq.w	#1,d2			; -1 for DFB loop
lab_1dae:
	move.w	-(a0),-(a2)		; copy word
	dbf	d2,lab_1dae		; loop until done

; get here after creating either a function, variable or string
; if function set variables start, string start, array start
; if variable set string start, array start
; if string set array start

lab_1daf:
	tst.b	sufnxf			; was it function
	bmi.s	lab_1db0		; branch if was FN

	btst.l	#23,d0			; was it string
	bne.s	lab_1db2		; branch if string

	bra.s	lab_1db1		; branch if was plain variable

lab_1db0:
	add.l	d1,svarl		; set new variable memory start
lab_1db1:
	add.l	d1,sstrl		; set new start of strings
lab_1db2:
	add.l	d1,sarryl		; set new array memory start
	move.l	d0,(a0)+		; save variable/function name
	move.l	#0x00,(a0)		; initialise variable
	btst.l	#23,d0			; was it string
	beq.s	lab_1dd7		; branch if not string

	move.w	#0x00,4(a0)		; else initialise string length

					; found a match for var ((Vrschl) = ptr)
lab_1dd7:
	move.b	#0x00,dtypef		; clear data type
	btst.l	#23,d0			; was it string
	beq.s	lab_1dd8		; branch if not string

	move.b	#0x80,dtypef		; set data type = string
	bra.s	lab_1dd9		; skip intger test

lab_1dd8:
	btst.l	#15,d0			; was it integer
	beq.s	lab_1dd9		; branch if not integer

	move.b	#0x40,dtypef		; set data type = integer
lab_1dd9:
	move.l	a0,cvaral		; save current variable/function value address
	move.b	#0x00,sufnxf		; clear FN flag byte
	rts

; set-up array pointer, Adatal, to first element in array
; set Adatal to Astrtl+2*Dimcnt+#$0A

lab_1de6:
	moveq	#0,d0			; clear d0
	move.b	dimcnt,d0		; get # of dimensions (1, 2 or 3)
	addq.l	#5,d0			; +5 (actually 10d but addq is quicker)
	add.l	d0,d0			; *2 (bytes per dimension size)
	add.l	a0,d0			; add array start pointer
	move.l	d0,adatal		; save array data pointer
	rts

; evaluate unsigned integer expression

lab_evin:
	bsr	lab_igby		; increment & scan memory
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch

; evaluate positive integer expression, result in d0 and Itemp

lab_evpi:
	tst.b	fac1_s			; test FAC1 sign (b7)
	bmi	lab_fcer		; do function call error if -ve

; evaluate integer expression, no sign check, result in d0 and Itemp

lab_evir:
	cmpi.b	#0xa0,fac1_e		; compare exponent with exponent = 2^32 (n>2^31)
	bcs	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	bne	lab_fcer		; if > do function call error, then warm start

	tst.b	fac1_s			; test sign of FAC1
	bpl.s	lab_evix		; if = and +ve then ok

	move.l	fac1_m,d0		; get mantissa
	cmp.l	#0x80000000,d0		; compare -2147483648 with mantissa
	bne	lab_fcer		; if <> do function call error, then warm start

lab_evix:
	move.l	d0,itemp		; else just set it
	rts

; find or make array

lab_1e17:
	move.w	defdim,-(sp)		; get DIM flag and data type flag (word in mem)
	moveq	#0,d1			; clear dimensions count

; now get the array dimension(s) and stack it (them) before the data type and DIM flag

lab_1e1f:
	move.w	d1,-(sp)		; save dimensions count
	move.l	varname,-(sp)		; save variable name
	bsr.s	lab_evin		; evaluate integer expression
	move.l	(sp)+,varname		; restore variable name
	move.w	(sp)+,d1		; restore dimensions count
	move.w	(sp)+,d0		; restore DIM and data type flags
	move.w	itemp+2,-(sp)		; stack this dimension size
	move.w	d0,-(sp)		; save DIM and data type flags
	addq.w	#1,d1			; increment dimensions count
	bsr	lab_gbyt		; scan memory
	cmp.b	#0x2c,d0			; compare with ","
	beq.s	lab_1e1f		; if found go do next dimension

	move.b	d1,dimcnt		; store dimensions count
	bsr	lab_1bfb		; scan for ")" , else do syntax error/warm start
	move.w	(sp)+,defdim		; restore DIM and data type flags (word in mem)
	movea.l	sarryl,a0		; get array mem start

; now check to see if we are at the end of array memory (we would be if there were
; no arrays).

lab_1e5c:
	move.l	a0,astrtl		; save as array start pointer
	cmpa.l	earryl,a0		; compare with array mem end
	beq.s	lab_1ea1		; go build array if not found

					; search for array
	move.l	(a0),d0			; get this array name
	cmp.l	varname,d0		; compare with array name
	beq.s	lab_1e8d		; array found so branch

					; no match
	movea.l	4(a0),a0		; get this array size
	adda.l	astrtl,a0		; add to array start pointer
	bra.s	lab_1e5c		; go check next array

					; found array, are we trying to dimension it?
lab_1e8d:
	tst.b	defdim			; are we trying to dimension it?
	bne	lab_dder		; if so do  double dimension error/warm start

; found the array and we're not dimensioning it so we must find an element in it

	bsr	lab_1de6		; set data pointer, Adatal, to the first element
					; in the array. Astrtl (and a0) points to the
					; start of the array
	addq.w	#8,a0			; index to dimension count
	move.w	(a0)+,d0		; get no of dimensions
	cmp.b	dimcnt,d0		; compare with dimensions count
	beq	lab_1f28		; found array so go get element

	bra	lab_wder		; else wrong so do "Wrong dimensions" error

					; array not found, so build it
lab_1ea1:
	bsr	lab_1de6		; set data pointer, Adatal, to the first element
					; in the array. Astrtl (and a0) points to the
					; start of the array
	bsr	lab_121f		; check available memory, "Out of memory" error
					; if no room, addr to check is in a0
	movea.l	astrtl,a0		; get array start pointer
	move.l	varname,d0		; get array name
	move.l	d0,(a0)+		; save array name
	moveq	#4,d1			; set 4 bytes per element
	btst.l	#23,d0			; test if string array
	beq.s	lab_1edf		; branch if not string

	moveq	#6,d1			; else 6 bytes per element
lab_1edf:
	move.l	d1,asptl		; set array data size (bytes per element)
;	moveq	#0,d1			* clear d1 (only byte is used so skip this)
	move.b	dimcnt,d1		; get dimensions count
	addq.w	#4,a0			; skip the array size now (don't know it yet!)
	move.w	d1,(a0)+		; set array's dimensions count

	tst.b	defdim			; test default DIM flag
	beq	lab_uder		; if default flag is clear then we are on the
					; LHS of = with no array so go do "Undimensioned
					; array" error.
					; now calculate the array data space size
lab_1ec0:

; If you want arrays to dimension themselves by default then comment out the test
; above and uncomment the next three code lines and the label LAB_1ED0

;	move.w	#11,d1			* set default dimension value (allow 0 to 10)
;	tst.b	defdim			* test default DIM flag
;	bne.s	lab_1ed0		* branch if b6 of Defdim is clear

	move.w	(sp)+,d1		; get dimension size
	addq.w	#1,d1			; +1 to allow 0 to n

;LAB_1ED0
	move.w	d1,(a0)+		; save to array header
	bsr	lab_1f7c		; do this dimension size (d1) * array size (Asptl)
					; result in d0
	move.l	d0,asptl		; save array data size
	subq.b	#1,dimcnt		; decrement dimensions count
	bne.s	lab_1ec0		; loop while not = 0

	adda.l	asptl,a0		; add size to first element address
	bcs	lab_omer		; if overflow go do "Out of memory" error

	bsr	lab_121f		; check available memory, "Out of memory" error
					; if no room, addr to check is in a0
	move.l	a0,earryl		; save array mem end
	moveq	#0,d0			; zero d0
	move.l	asptl,d1		; get size in bytes
	lsr.l	#1,d1			; /2 for word fill (may be odd # words)
	subq.w	#1,d1			; adjust for DBF loop
lab_1ed8:
	move.w	d0,-(a0)		; decrement pointer and clear word
	dbf	d1,lab_1ed8		; decrement & loop until low word done

	swap	d1			; swap words
	tst.w	d1			; test high word
	beq.s	lab_1f07		; exit if done

	subq.w	#1,d1			; decrement low (high) word
	swap	d1			; swap back
	bra.s	lab_1ed8		; go do a whole block

; now we need to calculate the array size by doing Earryl - Astrtl

lab_1f07:
	movea.l	astrtl,a0		; get for calculation and as pointer
	move.l	earryl,d0		; get array memory end
	sub.l	a0,d0			; calculate array size
	move.l	d0,4(a0)		; save size to array
	tst.b	defdim			; test default DIM flag
	bne.s	rts_011			; exit (RET) if this was a DIM command

					; else, find element
	addq.w	#8,a0			; index to dimension count
	move.w	(a0)+,dimcnt		; get array's dimension count

; we have found, or built, the array. now we need to find the element

lab_1f28:
	moveq	#0,d0			; clear first result
	move.l	d0,asptl		; clear array data pointer

; compare nth dimension bound (a0) with nth index (sp)+
; if greater do array bounds error

lab_1f2c:
	move.w	(a0)+,d1		; get nth dimension bound
	cmp.w	(sp),d1			; compare nth index with nth dimension bound
	ble	lab_aber		; if d1 less or = do array bounds error

; now do pointer = pointer * nth dimension + nth index

	tst.l	d0			; test pointer
	beq.s	lab_1f5a		; skip multiply if last result = null

	bsr.s	lab_1f7c		; do this dimension size (d1) * array size (Asptl)
lab_1f5a:
	moveq	#0,d1			; clear longword
	move.w	(sp)+,d1		; get nth dimension index
	add.l	d1,d0			; add index to size
	move.l	d0,asptl		; save array data pointer

	subq.b	#1,dimcnt		; decrement dimensions count
	bne.s	lab_1f2c		; loop if dimensions still to do

	move.b	#0,dtypef		; set data type to float
	moveq	#4,d1			; set for numeric array
	tst.b	varname+1		; test if string array
	bpl.s	lab_1f6a		; branch if not string

	moveq	#6,d1			; else set for string array
	move.b	#0x80,dtypef		; and set data type to string
	bra.s	lab_1f6b		; skip integer test

lab_1f6a:
	tst.b	varname+2		; test if integer array
	bpl.s	lab_1f6b		; branch if not integer

	move.b	#0x40,dtypef		; else set data type to integer
lab_1f6b:
	bsr.s	lab_1f7c		; do element size (d1) * array size (Asptl)
	adda.l	d0,a0			; add array data start pointer
	move.l	a0,cvaral		; save current variable address
rts_011:
	rts

; does d0 = (Astrtl),Y * (Asptl)
; do this dimension size (d1) * array data size (Asptl)

lab_1f7c:

; do a 16 x 32 bit multiply
; d1 holds the 16 bit multiplier
; Asptl holds the 32 bit multiplicand

; d0  bbbb  bbbb
; d1  0000  aaaa
;     ----------
; d0  rrrr  rrrr

	move.l	asptl,d0		; get result
	move.l	d0,d2			; copy it
	clr.w	d2			; clear low word
	sub.l	d2,d0			; clear high word
	swap	d2			; shift high word to low word
	mulu	d1,d0			; low result
	mulu	d1,d2			; high result
	swap	d2			; align words for test
	tst.w	d2			; must be zero
	bne	lab_omer		; if overflow go do "Out of memory" error

	add.l	d2,d0			; calculate result
	bcs	lab_omer		; if overflow go do "Out of memory" error

	rts

; perform FRE()

lab_fre:
	tst.b	dtypef			; test data type, $80=string, $40=integer, $00=float
	bpl.s	lab_1fb4		; branch if numeric

	bsr	lab_22b6		; pop string off descriptor stack, or from top of
					; string space, returns d0 = length, a0 = pointer

					; FRE(n) was numeric so do this
lab_1fb4:
	bsr	lab_garb		; go do garbage collection
	move.l	sstorl,d0		; get bottom of string space
	sub.l	earryl,d0		; subtract array mem end

; convert d0 to signed longword in FAC1

lab_ayfc:
	move.b	#0x00,dtypef		; clear data type, $80=string, $40=integer, $00=float
	move.w	#0xa000,fac1_e		; set FAC1 exponent and clear sign (b7)
	move.l	d0,fac1_m		; save FAC1 mantissa
	bpl	lab_24d0		; convert if +ve

	ori.b	#1,ccr			; else set carry
	bra	lab_24d0		; do +/- (carry is sign) & normalise FAC1

; remember if the line length is zero (infinite line) then POS(n) will return
; position MOD tabsize

; perform POS()

lab_pos:
	move.b	tpos,d0			; get terminal position

; convert d0 to unsigned byte in FAC1

lab_1fd0:
	and.l	#0xff,d0			; clear high bits
	bra.s	lab_ayfc		; convert d0 to signed longword in FAC1 & RET

; check not Direct (used by DEF and INPUT)

lab_ckrn:
	move.l	clinel,d1		; get current line #
	addq.l	#1,d1			; increment line #
	beq	lab_ider		; if 0 go do illegal direct error then warm start

	rts				; can continue so return

; perform DEF

lab_def:
	moveq	#tk_fn-0x100,d0		; get FN token
	bsr	lab_scca		; scan for CHR$(d0) , else syntax error/warm start
					; return character after d0
	move.b	#0x80,sufnxf		; set FN flag bit
	bsr	lab_1d12		; get FN name
	move.l	a0,func_l		; save function pointer

	bsr.s	lab_ckrn		; check not Direct (back here if ok)
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	move.b	#0x7e,sufnxf		; set FN variable flag bits
	bsr	lab_gvar		; get/create function variable address
					; return pointer to variable in Cvaral and a0
	moveq	#0,d0			; set zero to clear variable
	move.l	d0,(a0)+		; clear variable
	tst.b	dtypef			; test data type
	bpl.s	lab_defv		; branch if numeric variable

	move.w	d0,(a0)			; else clear string length
lab_defv:
	bsr	lab_1bfb		; scan for ")" , else do syntax error/warm start
	moveq	#tk_equal-0x100,d0	; = token
	bsr	lab_scca		; scan for CHR$(A), else syntax error/warm start
					; return character after d0
	move.l	varname,-(sp)		; push current variable name
	move.l	a5,-(sp)		; push BASIC execute pointer
	bsr	lab_data		; go perform DATA (find end of DEF FN statement)
	movea.l	func_l,a0		; get pointer
	move.l	(sp)+,(a0)		; save BASIC execute pointer to function
	move.l	(sp)+,4(a0)		; save current variable name to function
	rts

; evaluate FNx

lab_201e:
	move.b	#0x81,sufnxf		; set FN flag (find not create)
	bsr	lab_igby		; increment & scan memory
	bsr	lab_1d12		; get FN name
	move.b	dtypef,-(sp)		; push data type flag (function type)
	move.l	a0,-(sp)		; push function pointer
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_1bf7		; evaluate expression within parentheses
	movea.l	(sp)+,a0		; pop function pointer
	move.l	a0,func_l		; set function pointer
	move.b	dtypef,-(sp)		; push data type flag (function expression type)

	move.l	4(a0),d0		; get function variable name
	bsr	lab_1d4a		; go find function variable (already created)

					; now check type match for variable
	move.b	(sp)+,d0		; pop data type flag (function expression type)
	rol.b	#1,d0			; set carry if type = string
	bsr	lab_cktm		; type match check, set C for string

					; now stack the function variable value before use
	beq.s	lab_2043		; branch if not string

	cmpa.l	#des_sk_e,a4		; compare string stack pointer with max+1
	beq	lab_scer		; if no space on stack do string too complex error

	move.w	4(a0),-(a4)		; string length on descriptor stack
	move.l	(a0),-(a4)		; string address on stack
	bra.s	lab_204s		; skip var push

lab_2043:
	move.l	(a0),-(sp)		; push variable
lab_204s:
	move.l	a0,-(sp)		; push variable address
	move.b	dtypef,-(sp)		; push variable data type

	bsr.s	lab_2045		; pack function expression value into (a0)
					; (function variable)
	move.l	a5,-(sp)		; push BASIC execute pointer
	movea.l	func_l,a0		; get function pointer
	movea.l	(a0),a5			; save function execute ptr as BASIC execute ptr
	move.l	cvaral,-(sp)		; push variable address
	bsr	lab_evex		; evaluate expression
	move.l	(sp)+,cvaral		; pull variable address
	bsr	lab_gbyt		; scan memory
	bne	lab_sner		; if not [EOL] or [EOS] do syntax error/warm start

	move.l	(sp)+,a5		; restore BASIC execute pointer

; restore variable from stack and test data type

	move.b	(sp)+,d0		; pull variable data type
	movea.l	(sp)+,a0		; pull variable address
	tst.b	d0			; test variable data type
	bpl.s	lab_204t		; branch if not string

	move.l	(a4)+,(a0)		; string address from descriptor stack
	move.w	(a4)+,4(a0)		; string length from descriptor stack
	bra.s	lab_2044		; skip variable pull

lab_204t:
	move.l	(sp)+,(a0)		; restore variable from stack
lab_2044:
	move.b	(sp)+,d0		; pop data type flag (function type)
	rol.b	#1,d0			; set carry if type = string
	bsr	lab_cktm		; type match check, set C for string
	rts

lab_2045:
	tst.b	dtypef			; test data type
	bpl	lab_2778		; if numeric pack FAC1 into variable (a0) & return

	movea.l	a0,a2			; copy variable pointer
	bra	lab_17d6		; go do string LET & return


; perform STR$()

lab_strs:
	bsr	lab_ctnm		; check if source is numeric, else type mismatch
	bsr	lab_2970		; convert FAC1 to string
	addq.w	#4,sp			; skip return type check

; Scan, set up string
; print " terminated string to FAC1 stack

lab_20ae:
	moveq	#0x22,d2			; set Srchc character (terminator 1)
	move.w	d2,d3			; set Asrch character (terminator 2)

; print d2/d3 terminated string to FAC1 stack
; d2 = Srchc, d3 = Asrch, a0 is source
; a6 is temp

lab_20b4:
	moveq	#0,d1			; clear longword
	subq.w	#1,d1			; set length to -1
	movea.l	a0,a2			; copy start to calculate end
lab_20be:
	addq.w	#1,d1			; increment length
	move.b	(a0,d1.w),d0		; get byte from string
	beq.s	lab_20d0		; exit loop if null byte [EOS]

	cmp.b	d2,d0			; compare with search character (terminator 1)
	beq.s	lab_20cb		; branch if terminator

	cmp.b	d3,d0			; compare with terminator 2
	bne.s	lab_20be		; loop if not terminator 2 (or null string)

lab_20cb:
	cmp.b	#0x22,d0			; compare with "
	bne.s	lab_20d0		; branch if not "

	addq.w	#1,a2			; else increment string start (skip " at end)
lab_20d0:
	adda.l	d1,a2			; add longowrd length to make string end+1

	cmpa.l	#ram_strt,a0		; is string in ram
	bcs.s	lab_rtst		; if not go push descriptor on stack & exit
					; (could be message string from ROM)

	cmpa.l	smeml,a0		; is string in utility ram
	bcc.s	lab_rtst		; if not go push descriptor on stack & exit
					; (is in string or program space)

					; (else) copy string to string memory
lab_20c9:
	movea.l	a0,a1			; copy descriptor pointer
	move.l	d1,d0			; copy longword length
	bne.s	lab_20d8		; branch if not null string

	movea.l	d1,a0			; make null pointer
	bra.s	lab_rtst		; go push descriptor on stack & exit

lab_20d8:
	bsr.s	lab_2115		; make string space d1 bytes long
	move.l	d1,d0			; copy length again
	adda.l	d1,a0			; new string end
	adda.l	d1,a1			; old string end
	subq.w	#1,d0			; -1 for DBF loop
lab_20e0:
	move.b	-(a1),-(a0)		; copy byte (source can be odd aligned)
	dbf	d0,lab_20e0		; loop until done


; check for space on descriptor stack then ...
; put string address and length on descriptor stack & update stack pointers
; start is in a0, length is in d1

lab_rtst:
	cmpa.l	#des_sk_e,a4		; compare string stack pointer with max+1
	beq	lab_scer		; if no space on string stack ..
					; .. go do 'string too complex' error

					; push string & update pointers
	move.w	d1,-(a4)		; string length on descriptor stack
	move.l	a0,-(a4)		; string address on stack
	move.l	a4,fac1_m		; string descriptor pointer in FAC1
	move.b	#0x80,dtypef		; save data type flag, $80=string
	rts

; Build descriptor a0/d1
; make space in string memory for string d1.w long
; return pointer in a0/Sutill

lab_2115:
	tst.w	d1			; test length
	beq.s	lab_2128		; branch if user wants null string

					; make space for string d1 long
	move.w	d0,-(sp)		; save d0
	moveq	#0,d0			; clear longword
	move.b	d0,gclctd		; clear garbage collected flag (b7)
	moveq	#1,d0			; +1 to possibly round up
	and.w	d1,d0			; mask odd bit
	add.w	d1,d0			; ensure d0 is even length
	bcc.s	lab_2117		; branch if no overflow

	moveq	#1,d0			; set to allocate 65536 bytes
	swap	d0			; makes $00010000
lab_2117:
	movea.l	sstorl,a0		; get bottom of string space
	suba.l	d0,a0			; subtract string length
	cmpa.l	earryl,a0		; compare with top of array space
	bcs.s	lab_2137		; possibly do out of memory error if less

	move.l	a0,sstorl		; save bottom of string space low byte
	move.l	a0,sutill		; save string utility ptr low byte
	move.w	(sp)+,d0		; restore d0
	tst.w	d1			; set flags on length
	rts

lab_2128:
	movea.w	d1,a0			; make null pointer
	rts

lab_2137:
	tst.b	gclctd			; get garbage collected flag
	bmi	lab_omer		; do "Out of memory" error, then warm start

	bsr.s	lab_garb		; else go do garbage collection
	move.b	#0x80,gclctd		; set garbage collected flag
	bra.s	lab_2117		; go try again

; garbage collection routine

lab_garb:
	movem.l	d0-d2/a0-a2,-(sp)	; save registers
	move.l	ememl,sstorl		; start with no strings

					; re-run routine from last ending
lab_214b:
	move.l	earryl,d1		; set highest uncollected string so far
	moveq	#0,d0			; clear longword
	movea.l	d0,a1			; clear string to move pointer
	movea.l	sstrl,a0		; set pointer to start of strings
	movea.l	sarryl,a2		; set end pointer to start of arrays (end of strings)
	bra.s	lab_2176		; branch into loop at end loop test

lab_2161:
	bsr.l	lab_2206		; test and set if this is the highest string
	add.l	#10,a0			; increment to next string
lab_2176:
	cmpa.l	a0,a2			; compare pointer with with end of area
	bne.s	lab_2161		; go do next if not at end

; done strings, now do arrays.

;*	movea.l	sarryl,a0		* set pointer to start of arrays (should be there)
	movea.l	earryl,a2		; set end pointer to end of arrays
	bra.s	lab_218f		; branch into loop at end loop test

lab_217e:
	move.l	4(a0),d2		; get array size
	add.l	a0,d2			; makes start of next array

	move.l	(a0),d0			; get array name
	btst	#23,d0			; test string flag
	beq.s	lab_218b		; branch if not string

	move.w	8(a0),d0		; get # of dimensions
	add.w	d0,d0			; *2
	adda.w	d0,a0			; add to skip dimension size(s)
	adda.w	#0x0a,a0			; increment to first element
lab_2183:
	bsr.s	lab_2206		; test and set if this is the highest string
	addq.w	#6,a0			; increment to next element
	cmpa.l	d2,a0			; compare with start of next array
	bne.s	lab_2183		; go do next if not at end of array

lab_218b:
	movea.l	d2,a0			; pointer to next array
lab_218f:
	cmpa.l	a0,a2			; compare pointer with array end
	bne.s	lab_217e		; go do next if not at end

; done arrays and variables, now just the descriptor stack to do

	movea.l	a4,a0			; get descriptor stack pointer
	movea.l	#des_sk,a2		; set end pointer to end of stack
	bra.s	lab_21c4		; branch into loop at end loop test

lab_21c2:
	bsr.s	lab_2206		; test and set if this is the highest string
	addq.w	#06,a0			; increment to next string
lab_21c4:
	cmpa.l	a0,a2			; compare pointer with stack end
	bne.s	lab_21c2		; go do next if not at end

; descriptor search complete, now either exit or set-up and move string

	move.l	a1,d0			; set the flags (a1 is move string)
	beq.s	lab_21d1		; go tidy up and exit if no move

	movea.l	(a1),a0			; a0 is now string start
	moveq	#0,d1			; clear d1
	move.w	4(a1),d1		; d1 is string length
	addq.l	#1,d1			; +1
	and.b	#0xfe,d1			; make even length
	adda.l	d1,a0			; pointer is now to string end+1
	movea.l	sstorl,a2		; is destination end+1
	cmpa.l	a2,a0			; does the string need moving
	beq.s	lab_2240		; branch if not

	lsr.l	#1,d1			; word move so do /2
	subq.w	#1,d1			; -1 for DBF loop
lab_2216:
	move.w	-(a0),-(a2)		; copy word
	dbf	d1,lab_2216		; loop until done

	move.l	a0,(a1)			; save new string start
lab_2240:
	move.l	(a1),sstorl		; string start is new string mem start
	bra	lab_214b		; re-run routine from last ending
					; (but don't collect this string)

lab_21d1:
	movem.l	(sp)+,d0-d2/a0-a2	; restore registers
rts_012:
	rts

;  test and set if this is the highest string

lab_2206:
	move.l	(a0),d0			; get this string pointer
	beq.s	rts_012			; exit if null string

	cmp.l	d0,d1			; compare with highest uncollected string so far
	bcc.s	rts_012			; exit if <= with highest so far

	cmp.l	sstorl,d0		; compare with bottom of string space
	bcc.s	rts_012			; exit if >= bottom of string space

	moveq	#0,d0			; clear d0
	move.w	4(a1),d0		; d0 is string length
	neg.l	d0			; make -ve
	and.b	#0xfe,d0			; make -ve even length
	add.l	sstorl,d0		; add string store to -ve length
	cmp.l	(a0),d0			; compare with string address
	beq.s	lab_2212		; if = go move string store pointer down

	move.l	(a0),d1			; highest = current
	movea.l	a0,a1			; string to move = current
	rts

lab_2212:
	move.l	d0,sstorl		; set new string store start
	rts

; concatenate
; add strings, string descriptor 1 is in FAC1_m, string 2 is in line

lab_224d:
	move.l	fac1_m,-(sp)		; stack descriptor pointer for string 1

	bsr	lab_gval		; get value from line
	bsr	lab_ctst		; check if source is string, else do type mismatch

	movea.l	fac1_m,a1		; copy descriptor pointer 2
	movea.l	(sp),a0			; copy descriptor pointer 1
	moveq	#0,d1			; clear longword length
	move.w	4(a0),d1		; get length 1
	add.w	4(a1),d1		; add length 2
	bcs	lab_sler		; if overflow go do 'string too long' error

	bsr	lab_2115		; make space d1 bytes long
	move.l	a0,fac2_m		; save new string start pointer
	movea.l	(sp),a0			; copy descriptor pointer 1 from stack
	bsr.s	lab_229c		; copy string from descriptor a0 to Sutill
					; return with a0 = pointer, d1 = length

	movea.l	fac1_m,a0		; get descriptor pointer for string 2
	bsr.s	lab_22ba		; pop (a0) descriptor, returns with ..
					; a0 = pointer, d0 = length
	bsr.s	lab_229e		; copy string d0 bytes long from a0 to Sutill
					; return with a0 = pointer, d1 = length

	movea.l	(sp)+,a0		; get descriptor pointer for string 1
	bsr.s	lab_22ba		; pop (a0) descriptor, returns with ..
					; d0 = length, a0 = pointer

	movea.l	fac2_m,a0		; retreive result string pointer
	move.l	sutill,d1		; copy end
	sub.l	a0,d1			; subtract start = length
	bsr	lab_rtst		; push string on descriptor stack
					; a0 = pointer, d1 = length
	bra	lab_1adb		; continue evaluation

; copy string from descriptor (a0) to Sutill
; return with a0 = pointer, d1 = length

lab_229c:
	move.w	4(a0),d0		; get length
	movea.l	(a0),a0			; get string pointer
lab_229e:
	movea.l	sutill,a1		; get destination pointer
	move.w	d0,d1			; copy and check length
	beq.s	rts_013			; skip copy if null

	move.l	a1,-(sp)		; save destination string pointer
	subq.w	#1,d0			; subtract for DBF loop
lab_22a0:
	move.b	(a0)+,(a1)+		; copy byte
	dbf	d0,lab_22a0		; loop if not done

	move.l	a1,sutill		; update Sutill to end of copied string
	movea.l	(sp)+,a0		; restore destination string pointer
rts_013:
	rts

; evaluate string, returns d0 = length, a0 = pointer

lab_evst:
	bsr	lab_ctst		; check if source is string, else do type mismatch

; pop string off descriptor stack, or from top of string space
; returns with d0 = length, a0 = pointer

lab_22b6:
	movea.l	fac1_m,a0		; get descriptor pointer

; pop (a0) descriptor off stack or from string space
; returns with d0 = length, a0 = pointer

lab_22ba:
	movem.l	a1/d1,-(sp)		; save other regs
	cmpa.l	a0,a4			; is string on the descriptor stack
	bne.s	lab_22bd		; skip pop if not

	addq.w	#0x06,a4			; else update stack pointer
lab_22bd:
	moveq	#0,d0			; clear string length longword
	movea.l	(a0)+,a1		; get string address
	move.w	(a0)+,d0		; get string length

	cmpa.l	a0,a4			; was it on the descriptor stack
	bne.s	lab_22e6		; branch if it wasn't

	cmpa.l	sstorl,a1		; compare string address with bottom of string space
	bne.s	lab_22e6		; branch if <>

	moveq	#1,d1			; mask for odd bit
	and.w	d0,d1			; AND length
	add.l	d0,d1			; make it fit word aligned length

	add.l	d1,sstorl		; add to bottom of string space
lab_22e6:
	movea.l	a1,a0			; copy to a0
	movem.l	(sp)+,a1/d1		; restore other regs
	rts

; perform CHR$()

lab_chrs:
	bsr	lab_evby		; evaluate byte expression, result in d0 and Itemp
	moveq	#1,d1			; string is single byte
	bsr	lab_2115		; make string space d1 bytes long
					; return a0/Sutill = pointer, others unchanged
	move.b	d0,(a0)			; save byte in string (byte IS string!)
	addq.w	#4,sp			; skip return type check
	bra	lab_rtst		; push string on descriptor stack
					; a0 = pointer, d1 = length

; perform LEFT$()

lab_left:
	bsr.s	lab_236f		; pull string data & word parameter from stack
					; return pointer in a0, word in d0. destroys d1
	exg	d0,d1			; offset in d0, word in d1
	tst.l	d1			; test returned length
	beq.s	lab_231c		; branch if null return

	moveq	#0,d0			; clear start offset
	cmp.w	4(a0),d1		; compare word parameter with string length
	bcs.s	lab_231c		; branch if string length > word parameter

	bra.s	lab_2316		; go copy whole string

; perform RIGHT$()

lab_right:
	bsr.s	lab_236f		; pull string data & word parameter from stack
					; return pointer in a0, word in d0. destroys d1
	move.l	d0,d1			; copy word (and clear high word)
	beq.s	lab_231c		; branch if null return

	move.w	4(a0),d0		; get string length
	sub.l	d1,d0			; subtract word
	bcc.s	lab_231c		; branch if string length > word parameter

					; else copy whole string
lab_2316:
	moveq	#0,d0			; clear start offset
	move.w	4(a0),d1		; else make parameter = length

; get here with ...
;   a0  - points to descriptor
; 4(a0) - is string length
;   d0  - is offset from string start
;   d1  - is required string length

lab_231c:
	move.l	a0,-(sp)		; save string descriptor pointer
	bsr	lab_2115		; make string space d1 bytes long
					; return a0/Sutill = pointer, others unchanged
	movea.l	(sp)+,a0		; restore string descriptor pointer
	move.l	d0,-(sp)		; save start offset (longword)
	bsr.s	lab_22ba		; pop (a0) descriptor, returns with ..
					; d0 = length, a0 = pointer
	adda.l	(sp)+,a0		; adjust pointer to start of wanted string
	move.w	d1,d0			; length to d0
	bsr	lab_229e		; store string d0 bytes long from (a0) to (Sutill)
					; return with a0 = pointer, d1 = length
	bra	lab_rtst		; push string on descriptor stack
					; a0 = pointer, d1 = length

; perform MID$()

lab_mids:
	moveq	#0,d7			; clear longword
	subq.w	#1,d7			; set default length = 65535
	bsr	lab_gbyt		; scan memory
	cmp.b	#0x29,d0			; compare with ")"
	beq.s	lab_2358		; branch if = ")" (skip second byte get)

	bsr	lab_1c01		; find "," - else do syntax error/warm start
	bsr	lab_gtwo		; get word parameter, result in d0 and Itemp
	move.l	d0,d7			; copy length
lab_2358:
	bsr.s	lab_236f		; pull string data & byte parameter from stack
					; return pointer in a0, word in d0. destroys d1
	moveq	#0,d1			; null length
	subq.l	#1,d0			; decrement start index
	bmi	lab_fcer		; if was null do function call error, then warm start

	cmp.w	4(a0),d0		; compare string length with start index
	bcc.s	lab_231c		; if start not in string do null string (d1=0)

	move.l	d7,d1			; get length back
	add.w	d0,d7			; d7 now = MID$() end
	bcs.s	lab_2368		; already too long so do RIGHT$ equivalent

	cmp.w	4(a0),d7		; compare string length with start index + length
	bcs.s	lab_231c		; if end in string go do string

lab_2368:
	move.w	4(a0),d1		; get string length
	sub.w	d0,d1			; subtract start offset
	bra.s	lab_231c		; go do string (effectively RIGHT$)

; pull string data & word parameter from stack
; return pointer in a0, word in d0. destroys d1

lab_236f:
	bsr	lab_1bfb		; scan for ")" , else do syntax error/warm start
	move.l	(sp)+,d1		; pull return address
	addq	#4,sp			; skip type check on exit
	moveq	#0,d0			; clear longword
	move.w	(sp)+,d0		; pull word parameter
	move.l	(sp)+,a0		; pull string pointer
	move.l	d1,-(sp)		; push return address
	rts

; perform LCASE$()

lab_lcase:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	move.l	d0,d1			; copy length and set flags
	beq.s	nostring		; branch if null string

	move.w	d0,d2			; copy for counter
	subq.l	#1,d2			; subtract for DBF loop
lc_loop:
	move.b	(a0,d2.w),d0		; get byte from string
	bsr	lab_1d82		; is character "A" to "Z"
	bcc.s	noucase			; branch if not upper case alpha

	ori.b	#0x20,d0			; convert upper to lower case
	move.b	d0,(a0,d2.w)		; save byte back to string
noucase:
	dbf	d2,lc_loop		; decrement and loop if not all done

	bra.s	nostring		; tidy up & exit (branch always)

; perform UCASE$()

lab_ucase:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	move.l	d0,d1			; copy length and set flags
	beq.s	nostring		; branch if null string

	move.w	d0,d2			; copy for counter
	subq.l	#1,d2			; subtract for DBF loop
uc_loop:
	move.b	(a0,d2.w),d0		; get byte from string
	bsr	lab_casc		; is character "a" to "z" (or "A" to "Z")
	bcc.s	nolcase			; branch if not alpha

	andi.b	#0xdf,d0			; convert lower to upper case
	move.b	d0,(a0,d2.w)		; save byte back to string
nolcase:
	dbf	d2,uc_loop		; decrement and loop if not all done

nostring:
	addq.w	#4,sp			; dump RTS address (skip numeric type check)
	bra	lab_rtst		; push string on descriptor stack
					; a0 = pointer, d1 = length

; perform SADD()

lab_sadd:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	move.l	a0,d0			; copy string address
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & return

; perform LEN()

lab_lens:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	bra	lab_1fd0		; convert d0 to unsigned byte in FAC1 & return

; perform ASC()

lab_asc:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	beq	lab_fcer		; if null do function call error, then warm start

	move.b	(a0),d0			; get first character byte
	bra	lab_1fd0		; convert d0 to unsigned byte in FAC1 & return

; scan for "," and get byte, else do Syntax error then warm start

lab_scgb:
	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bra.s	lab_gtby		; get byte parameter, result in d0 and Itemp & RET

; increment and get byte, result in d0 and Itemp

lab_sgby:
	bsr	lab_igby		; increment & scan memory

; get byte parameter, result in d0 and Itemp

lab_gtby:
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch

; evaluate byte expression, result in d0 and Itemp

lab_evby:
	bsr	lab_evpi		; evaluate positive integer expression
					; result in d0 and Itemp
	move.l	d0,d1			; copy result
	and.l	#0xffffff00,d1		; check top 24 bits
	bne	lab_fcer		; if <> 0 do function call error/warm start

	rts

; get word parameter, result in d0 and Itemp

lab_gtwo:
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_evpi		; evaluate positive integer expression
					; result in d0 and Itemp
	swap	d0			; copy high word to low word
	tst.w	d0			; set flags
	bne	lab_fcer		; if <> 0 do function call error/warm start

	swap	d0			; copy high word to low word
	rts

; perform VAL()

lab_val:
	bsr	lab_evst		; evaluate string, returns d0 = length, a0 = pointer
	tst.w	d0			; check length
	beq.s	lab_valz		; string was null so set result = $00
					; clear FAC1 exponent & sign & return

	movea.l	a5,a6			; save BASIC execute pointer
	movea.l	a0,a5			; copy string pointer to execute pointer
	adda.l	d0,a0			; string end+1
	move.b	(a0),d0			; get byte from string+1
	move.w	d0,-(sp)		; save it
	move.l	a0,-(sp)		; save address
	move.b	#0,(a0)			; null terminate string
	bsr	lab_gbyt		; scan memory
	bsr	lab_2887		; get FAC1 from string
	movea.l	(sp)+,a0		; restore pointer
	move.w	(sp)+,d0		; pop byte
	move.b	d0,(a0)			; restore to memory
	movea.l	a6,a5			; restore BASIC execute pointer
	rts

lab_valz:
	move.w	d0,fac1_e		; clear FAC1 exponent & sign
	rts

; get two parameters for POKE or WAIT, first parameter in a0, second in d0

lab_gadb:
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	move.l	d0,-(sp)		; copy to stack
	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bsr.s	lab_gtby		; get byte parameter, result in d0 and Itemp
	movea.l	(sp)+,a0		; pull address
	rts

; get two parameters for DOKE or WAITW, first parameter in a0, second in d0

lab_gadw:
	bsr.s	lab_gead		; get even address (for word/long memory actions)
					; address returned in d0 and on the stack
	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_evir		; evaluate integer expression
					; result in d0 and Itemp
	swap	d0			; swap words
	tst.w	d0			; test high word
	beq.s	lab_xgadw		; exit if null

	addq.w	#1,d0			; increment word
	bne	lab_fcer		; if <> 0 do function call error/warm start

lab_xgadw:
	swap	d0			; swap words back
	movea.l	(sp)+,a0		; pull address
	rts

; get even address (for word or longword memory actions)
; address returned in d0 and on the stack
; does address error if the address is odd

lab_gead:
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	btst	#0,d0			; test low bit of longword
	bne	lab_ader		; if address is odd do address error/warm start

	movea.l	(sp)+,a0		; copy return address
	move.l	d0,-(sp)		; address on stack
	move.l	a0,-(sp)		; put return address back
	rts

; perform PEEK()

lab_peek:
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	movea.l	d0,a0			; copy to address register
	move.b	(a0),d0			; get byte
	bra	lab_1fd0		; convert d0 to unsigned byte in FAC1 & return

; perform POKE

lab_poke:
	bsr.s	lab_gadb		; get two parameters for POKE or WAIT
					; first parameter in a0, second in d0
	move.b	d0,(a0)			; put byte in memory
	rts

; perform DEEK()

lab_deek:
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	btst	#0,d0			; test low bit of longword
	bne	lab_ader		; if address is odd do address error/warm start

	exg	d0,a0			; copy to address register
	moveq	#0,d0			; clear top bits
	move.w	(a0),d0			; get word
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & return

; perform LEEK()

lab_leek:
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	btst	#0,d0			; test low bit of longword
	bne	lab_ader		; if address is odd do address error/warm start

	exg	d0,a0			; copy to address register
	move.l	(a0),d0			; get word
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & return

; perform DOKE

lab_doke:
	bsr.s	lab_gadw		; get two parameters for DOKE or WAIT
					; first parameter in a0, second in d0
	move.w	d0,(a0)			; put word in memory
	rts

; perform LOKE

lab_loke:
	bsr.s	lab_gead		; get even address
					; address returned in d0 and on the stack
	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_evir		; evaluate integer value (no sign check)
	movea.l	(sp)+,a0		; pull address
	move.l	d0,(a0)			; put longword in memory
rts_015:
	rts

; perform SWAP

lab_swap:
	bsr	lab_gvar		; get var1 address
					; return pointer to variable in Cvaral and a0
	movea.l	a0,a3			; copy address
	move.b	dtypef,d3		; get data type, $80=string, $40=inetger $00=float

	bsr	lab_1c01		; scan for "," , else do syntax error/warm start
	bsr	lab_gvar		; get var2 address (pointer in Cvaral/h)
					; return pointer to variable in Cvaral and a0
	cmp.b	dtypef,d3		; compare with var2 data type
	bne	lab_tmer		; if not both the same type do "Type mismatch"
					; error then warm start

	move.l	(a0),d0			; get var2
	move.l	(a3),(a0)		; copy var1 to var2
	move.l	d0,(a3)			; save var2 to var1

	tst.b	d3			; check data type
	bpl.s	rts_015			; exit if not string

	move.w	4(a0),d0		; get string 2 length
	move.w	4(a3),4(a0)		; copy string 1 length to string 2 length
	move.w	d0,4(a3)		; save string 2 length to string 1 length
	rts

; perform CALL

lab_call:
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	pea	lab_gbyt(pc)		; put return address on stack
	movea.l	d0,a0			; address into address register
	jmp	(a0)			; do indirect jump to user routine

; if the called routine exits correctly then it will return via the get byte routine.
; this will then get the next byte for the interpreter and return

; perform WAIT

lab_wait:
	bsr	lab_gadb		; get two parameters for POKE or WAIT
					; first parameter in a0, second in d0
	move.l	a0,-(sp)		; save address
	move.w	d0,-(sp)		; save byte
	moveq	#0,d2			; clear mask
	bsr	lab_gbyt		; scan memory
	beq.s	lab_2441		; skip if no third argument

	bsr	lab_scgb		; scan for "," & get byte,
					; else do syntax error/warm start
	move.l	d0,d2			; copy mask
lab_2441:
	move.w	(sp)+,d1		; get byte
	movea.l	(sp)+,a0		; get address
lab_2445:
	move.b	(a0),d0			; read memory byte
	eor.b	d2,d0			; EOR with second argument (mask)
	and.b	d1,d0			; AND with first argument (byte)
	beq.s	lab_2445		; loop if result is zero

	rts

; perform subtraction, FAC1 from FAC2

lab_subtract:
	eori.b	#0x80,fac1_s		; complement FAC1 sign
	move.b	fac2_s,fac_sc		; copy FAC2 sign byte

	move.b	fac1_s,d0		; get FAC1 sign byte
	eor.b	d0,fac_sc		; EOR with FAC2 sign

	bra.s	lab_add			; go add FAC2 to FAC1

; add 0.5 to FAC1

lab_244e:
	lea	lab_2a96(pc),a0	; set 0.5 pointer

; perform addition, add (a0) to FAC1

lab_246c:
	bsr	lab_264d		; unpack memory (a0) into FAC2

; add FAC2 to FAC1

lab_add:
	move.b	fac1_e,d0		; get exponent
	beq	lab_279b		; FAC1 was zero so copy FAC2 to FAC1 & return

					; FAC1 is non zero
	movea.l	#fac2_m,a0		; set pointer1 to FAC2 mantissa
	move.b	fac2_e,d0		; get FAC2 exponent
	beq.s	rts_016			; exit if zero

	sub.b	fac1_e,d0		; subtract FAC1 exponent
	beq.s	lab_24a8		; branch if = (go add mantissa)

	bcs.s	lab_249c		; branch if FAC2 < FAC1

					; FAC2 > FAC1
	move.w	fac2_e,fac1_e		; copy sign and exponent of FAC2
	neg.b	d0			; negate exponent difference (make diff -ve)
	subq.w	#8,a0			; pointer1 to FAC1

lab_249c:
	neg.b	d0			; negate exponent difference (make diff +ve)
	move.l	d1,-(sp)		; save d1
	cmp.b	#32,d0			; compare exponent diff with 32
	blt.s	lab_2467		; branch if range >= 32

	moveq	#0,d1			; clear d1
	bra.s	lab_2468		; go clear smaller mantissa

lab_2467:
	move.l	(a0),d1			; get FACx mantissa
	lsr.l	d0,d1			; shift d0 times right
lab_2468:
	move.l	d1,(a0)			; save it back
	move.l	(sp)+,d1		; restore d1

					; exponents are equal now do mantissa add/subtract
lab_24a8:
	tst.b	fac_sc			; test sign compare (FAC1 EOR FAC2)
	bmi.s	lab_24f8		; if <> go do subtract

	move.l	fac2_m,d0		; get FAC2 mantissa
	add.l	fac1_m,d0		; add FAC1 mantissa
	bcc.s	lab_24f7		; save and exit if no carry (FAC1 is normal)

	roxr.l	#1,d0			; else shift carry back into mantissa
	addq.b	#1,fac1_e		; increment FAC1 exponent
	bcs	lab_ofer		; if carry do overflow error & warm start

lab_24f7:
	move.l	d0,fac1_m		; save mantissa
rts_016:
	rts
					; signs are different
lab_24f8:
	movea.l	#fac1_m,a1		; pointer 2 to FAC1
	cmpa.l	a0,a1			; compare pointers
	bne.s	lab_24b4		; branch if <>

	addq.w	#8,a1			; else pointer2 to FAC2

					; take smaller from bigger (take sign of bigger)
lab_24b4:
	move.l	(a1),d0			; get larger mantissa
	move.l	(a0),d1			; get smaller mantissa
	move.l	d0,fac1_m		; save larger mantissa
	sub.l	d1,fac1_m		; subtract smaller

; do +/- (carry is sign) & normalise FAC1

lab_24d0:
	bcc.s	lab_24d5		; branch if result is +ve

					; erk! subtract wrong way round, negate everything
	eori.b	#0xff,fac1_s		; complement FAC1 sign
	neg.l	fac1_m			; negate FAC1 mantissa

; normalise FAC1

lab_24d5:
	move.l	fac1_m,d0		; get mantissa
	bmi.s	lab_24da		; mantissa is normal so just exit

	bne.s	lab_24d9		; mantissa is not zero so go normalise FAC1

	move.w	d0,fac1_e		; else make FAC1 = +zero
	rts

lab_24d9:
	move.l	d1,-(sp)		; save d1
	move.l	d0,d1			; mantissa to d1
	moveq	#0,d0			; clear d0
	move.b	fac1_e,d0		; get exponent byte
	beq.s	lab_24d8		; if exponent is zero then clean up and exit
lab_24d6:
	add.l	d1,d1			; shift mantissa (ADD is quicker for single shift)
	dbmi	d0,lab_24d6		; decrement exponent and loop if mantissa and
					; exponent +ve

	tst.w	d0			; test exponent
	beq.s	lab_24d8		; if exponent is zero make FAC1 zero

	bpl.s	lab_24d7		; if exponent is >zero go save FAC1

	moveq	#1,d0			; else set for zero after correction
lab_24d7:
	subq.b	#1,d0			; adjust exponent for loop
	move.l	d1,fac1_m		; save normalised mantissa
lab_24d8:
	move.l	(sp)+,d1		; restore d1
	move.b	d0,fac1_e		; save corrected exponent
lab_24da:
	rts

; perform LOG()

lab_log:
	tst.b	fac1_s			; test sign
	bmi	lab_fcer		; if -ve do function call error/warm start

	moveq	#0,d7			; clear d7
	move.b	d7,fac_sc		; clear sign compare
	move.b	fac1_e,d7		; get exponent
	beq	lab_fcer		; if 0 do function call error/warm start

	sub.l	#0x81,d7			; normalise exponent
	move.b	#0x81,fac1_e		; force a value between 1 and 2
	move.l	fac1_m,d6		; copy mantissa

	move.l	#0x80000000,fac2_m	; set mantissa for 1
	move.w	#0x8100,fac2_e		; set exponent for 1
	bsr	lab_add			; find arg+1
	moveq	#0,d0			; setup for calc skip
	move.w	d0,fac2_e		; set FAC1 for zero result
	add.l	d6,d6			; shift 1 bit out
	move.l	d6,fac2_m		; put back FAC2
	beq.s	lab_lonn		; if 0 skip calculation

	move.w	#0x8000,fac2_e		; set exponent for .5
	bsr	lab_divide		; do (arg-1)/(arg+1)
	tst.b	fac1_e			; test exponent
	beq.s	lab_lonn		; if 0 skip calculation

	move.b	fac1_e,d1		; get exponent
	sub.b	#0x82,d1			; normalise and two integer bits
	neg.b	d1			; negate for shift
;*	cmp.b	#$1f,d1			* will mantissa vanish?
;*	bgt.s	lab_dunno		* if so do ???

	move.l	fac1_m,d0		; get mantissa
	lsr.l	d1,d0			; shift in two integer bits

; d0 = arg
; d0 = x, d1 = y
; d2 = x1, d3 = y1
; d4 = shift count
; d5 = loop count
; d6 = z
; a0 = table pointer

	moveq	#0,d6			; z = 0
	move.l	#1<<30,d1		; y = 1
	movea.l	#tab_hthet,a0		; pointer to hyperbolic tangent table
	moveq	#30,d5			; loop 31 times
	moveq	#1,d4			; set shift count
	bra.s	lab_locc		; entry point for loop

lab_laad:
	asr.l	d4,d2			; x1 >> i
	sub.l	d2,d1			; y = y - x1
	add.l	(a0),d6			; z = z + tanh(i)
lab_locc:
	move.l	d0,d2			; x1 = x
	move.l	d1,d3			; y1 = Y
	asr.l	d4,d3			; y1 >> i
	bcc.s	lab_lolp

	addq.l	#1,d3
lab_lolp:
	sub.l	d3,d0			; x = x - y1
	bpl.s	lab_laad		; branch if > 0

	move.l	d2,d0			; get x back
	addq.w	#4,a0			; next entry
	addq.l	#1,d4			; next i
	lsr.l	#1,d3			; /2
	beq.s	lab_locx		; branch y1 = 0

	dbf	d5,lab_lolp		; decrement and loop if not done

					; now sort out the result
lab_locx:
	add.l	d6,d6			; *2
	move.l	d6,d0			; setup for d7 = 0
lab_lonn:
	move.l	d0,d4			; save cordic result
	moveq	#0,d5			; set default exponent sign
	tst.l	d7			; check original exponent sign
	beq.s	lab_loxo		; branch if original was 0

	bpl.s	lab_loxp		; branch if was +ve

	neg.l	d7			; make original exponent +ve
	moveq	#0x80-0x100,d5		; make sign -ve
lab_loxp:
	move.b	d5,fac1_s		; save original exponent sign
	swap	d7			; 16 bit shift
	lsl.l	#8,d7			; easy first part
	moveq	#0x88-0x100,d5		; start with byte
lab_lone:
	subq.l	#1,d5			; decrement exponent
	add.l	d7,d7			; shift mantissa
	bpl.s	lab_lone		; loop if not normal

lab_loxo:
	move.l	d7,fac1_m		; save original exponent as mantissa
	move.b	d5,fac1_e		; save exponent for this
	move.l	#0xb17217f8,fac2_m	; LOG(2) mantissa
	move.w	#0x8000,fac2_e		; LOG(2) exponent & sign
	move.b	fac1_s,fac_sc		; make sign compare = FAC1 sign
	bsr.s	lab_multiply		; do multiply
	move.l	d4,fac2_m		; save cordic result
	beq.s	lab_lowz		; branch if zero

	move.w	#0x8200,fac2_e		; set exponent & sign
	move.b	fac1_s,fac_sc		; clear sign compare
	bsr	lab_add			; and add for final result

lab_lowz:
	rts

; multiply FAC1 by FAC2

lab_multiply:
	movem.l	d0-d4,-(sp)		; save registers
	tst.b	fac1_e			; test FAC1 exponent
	beq	lab_muuf		; if exponent zero go make result zero

	move.b	fac2_e,d0		; get FAC2 exponent
	beq	lab_muuf		; if exponent zero go make result zero

	move.b	fac_sc,fac1_s		; sign compare becomes sign

	add.b	fac1_e,d0		; multiply exponents by adding
	bcc.s	lab_mnoc		; branch if no carry

	sub.b	#0x80,d0			; normalise result
	bcc	lab_ofer		; if no carry do overflow

	bra.s	lab_madd		; branch

					; no carry for exponent add
lab_mnoc:
	sub.b	#0x80,d0			; normalise result
	bcs.l	lab_muuf		; return zero if underflow

lab_madd:
	move.b	d0,fac1_e		; save exponent

					; d1 (FAC1) x d2 (FAC2)
	move.l	fac1_m,d1		; get FAC1 mantissa
	move.l	fac2_m,d2		; get FAC2 mantissa

	move.w	d1,d4			; copy low word FAC1
	move.l	d1,d0			; copy long word FAC1
	swap	d0			; high word FAC1 to low word FAC1
	move.w	d0,d3			; copy high word FAC1

	mulu	d2,d1			; low word FAC2 x low word FAC1
	mulu	d2,d0			; low word FAC2 x high word FAC1
	swap	d2			; high word FAC2 to low word FAC2
	mulu	d2,d4			; high word FAC2 x low word FAC1
	mulu	d2,d3			; high word FAC2 x high word FAC1

; done multiply, now add partial products

;                 d1 =                          aaaa  ----  FAC2_L x FAC1_L
;                 d0 =                    bbbb  aaaa        FAC2_L x FAC1_H
;                 d4 =                    bbbb  aaaa        FAC2_H x FAC1_L
;                 d3 =              cccc  bbbb              FAC2_H x FAC1_H
;                 product =         mmmm  mmmm

	add.l	#0x8000,d1		; round up lowest word
	clr.w	d1			; clear low word, don't need it
	swap	d1			; align high word
	add.l	d0,d1			; add FAC2_L x FAC1_H (can't be carry)
lab_muf1:
	add.l	d4,d1			; now add intermediate (FAC2_H x FAC1_L)
	bcc.s	lab_muf2		; branch if no carry

	add.l	#0x10000,d3		; else correct result
lab_muf2:
	add.l	#0x8000,d1		; round up low word
	clr.w	d1			; clear low word
	swap	d1			; align for final add
	add.l	d3,d1			; add FAC2_H x FAC1_H, result
	bmi.s	lab_muf3		; branch if normalisation not needed

	add.l	d1,d1			; shift mantissa
	subq.b	#1,fac1_e		; adjust exponent
	beq.s	lab_muuf		; branch if underflow

lab_muf3:
	move.l	d1,fac1_m		; save mantissa
lab_muex:
	movem.l	(sp)+,d0-d4		; restore registers
	rts
					; either zero or underflow result
lab_muuf:
	moveq	#0,d0			; quick clear
	move.l	d0,fac1_m		; clear mantissa
	move.w	d0,fac1_e		; clear sign and exponent
	bra.s	lab_muex		; restore regs & exit

; unpack memory (a0) into FAC2, trashes d0

lab_264d:
	move.l	(a0),d0			; get value
	swap	d0			; exponent and sign to bits 0-15
	move.w	d0,fac2_e		; save FAC2 exponent & sign
	move.b	d0,fac_sc		; save sign as sign compare
	or.b	#0x80,d0			; restore MSb
	swap	d0			; swap words back

	asl.l	#8,d0			; shift exponent & clear guard byte
	move.l	d0,fac2_m		; move into FAC2

	move.b	fac1_s,d0		; get FAC1 sign
	eor.b	d0,fac_sc		; make sign compare (FAC1_s EOR FAC2_s)

	rts

; multiply by 10

lab_269e:
	tst.b	fac1_e			; test exponent byte
	beq.s	x10exit			; exit if zero

	move.l	d0,-(sp)		; save d0
	move.l	fac1_m,d0		; get FAC1
	lsr.l	#2,d0			; /4
	bcc.s	x10nornd		; if no carry don't round up

	addq.l	#1,d0			; round up least bit, there won't be any carry
x10nornd:
	add.l	fac1_m,d0		; add FAC1 (x1.125)
	bcc.s	x10exp			; branch if no carry

	roxr.l	#1,d0			; shift carry back in
	addq.b	#1,fac1_e		; increment exponent
	bcs	lab_ofer		; branch if overflow

x10exp:
	move.l	d0,fac1_m		; save new mantissa
	addq.b	#3,fac1_e		; correct exponent ( 8 x 1.125 = 10 )
	bcs	lab_ofer		; branch if overflow

	move.l	(sp)+,d0		; restore d0
x10exit:
	rts

; convert a0 and do (a0)/FAC1

lab_26ca:
	bsr	lab_264d		; unpack memory (a0) into FAC2, trashes d0

; do FAC2/FAC1, result in FAC1
; fast hardware version

lab_divide:
	move.l	d7,-(sp)		; save d7
	moveq	#0,d0			; clear FAC2 exponent
	move.l	d0,d2			; clear FAC1 exponent

	move.b	fac1_e,d2		; get FAC1 exponent
	beq	lab_dzer		; if zero go do /0 error

	move.b	fac2_e,d0		; get FAC2 exponent
	beq.s	lab_div0		; if zero return zero

	sub.w	d2,d0			; get result exponent by subtracting
	add.w	#0x80,d0			; correct 16 bit exponent result

	move.b	fac_sc,fac1_s		; sign compare is result sign

; now to do 32/32 bit mantissa divide

	clr.b	flag			; clear 'flag' byte
	move.l	fac1_m,d3		; get FAC1 mantissa
	move.l	fac2_m,d4		; get FAC2 mantissa
	cmp.l	d3,d4			; compare FAC2 with FAC1 mantissa
	beq.s	lab_man1		; set mantissa result = 1 if equal

	bcs.s	ac1gtac2		; branch if FAC1 > FAC2

	sub.l	d3,d4			; subtract FAC1 from FAC2 (result now must be <1)
	addq.b	#3,flag			; FAC2>FAC1 so set 'flag' byte
ac1gtac2:
	bsr.s	lab_32_16		; do 32/16 divide
	swap	d1			; move 16 bit result to high word
	move.l	d2,d4			; copy remainder longword
	bsr.s	lab_3216		; do 32/16 divide again (skip copy d4 to d2)
	divu	d5,d2			; now divide remainder to make guard word
	move.b	flag,d7			; now normalise, get flag byte back
	beq.s	lab_divx		; skip add if null

; else result was >1 so we need to add 1 to result mantissa and adjust exponent

	lsr.b	#1,d7			; shift 1 into eXtend
	roxr.l	#1,d1			; shift extend result >>
	roxr.w	#1,d2			; shift extend guard word >>
	addq.b	#1,d0			; adjust exponent

; now round result to 32 bits

lab_divx:
	add.w	d2,d2			; guard bit into eXtend bit
	bcc.s	l_divrnd		; branch if guard=0

	addq.l	#1,d1			; add guard to mantissa
	bcc.s	l_divrnd		; branch if no overflow

lab_set1:
	roxr.l	#1,d1			; shift extend result >>
	addq.w	#1,d0			; adjust exponent

					; test for over/under flow
l_divrnd:
	move.w	d0,d3			; copy exponent
	bmi.l	lab_div0		; if -ve return zero

	andi.w	#0xff00,d3		; mask low byte
	bne	lab_ofer		; branch if overflow

					; move result into FAC1
lab_xdiv:
	move.l	(sp)+,d7		; restore d7
	move.b	d0,fac1_e		; save result exponent
	move.l	d1,fac1_m		; save result mantissa
	rts

; FAC1 mantissa = FAC2 mantissa so set result mantissa

lab_man1:
	moveq	#1,d1			; set bit
	lsr.l	d1,d1			; bit into eXtend
	bra.s	lab_set1		; set mantissa, adjust exponent and exit

; result is zero

lab_div0:
	moveq	#0,d0			; zero exponent & sign
	move.l	d0,d1			; zero mantissa
	bra	lab_xdiv		; exit divide

; divide 16 bits into 32, AB/Ex
;
; d4              AAAA  BBBB		* 32 bit numerator
; d3              EEEE  xxxx		* 16 bit denominator
;
; returns -
;
; d1              xxxx  DDDD		* 16 bit result
; d2                    HHHH  IIII	* 32 bit remainder

lab_32_16:
	move.l	d4,d2			; copy FAC2 mantissa          (AB)
lab_3216:
	move.l	d3,d5			; copy FAC1 mantissa          (EF)
	clr.w	d5			; clear low word d1           (Ex)
	swap	d5			; swap high word to low word  (xE)
	divu	d5,d4			; do FAC2/FAC1 high word      (AB/E)
	bvc.s	lab_lt_1		; if no overflow DIV was ok

	moveq	#-1,d4			; else set default value

; done the divide, now check the result, we have ...

; d3              EEEE  FFFF		* denominator copy
; d5        0000  EEEE			* denominator high word
; d2              AAAA  BBBB		* numerator copy
; d4              MMMM  DDDD		* result MOD and DIV

lab_lt_1:
	move.w	d4,d6			; copy 16 bit result
	move.w	d4,d1			; copy 16 bit result again

; we now have ..
; d3              EEEE  FFFF		* denominator copy
; d5        0000  EEEE			* denominator high word
; d6              xxxx  DDDD		* result DIV copy
; d1              xxxx  DDDD		* result DIV copy
; d2              AAAA  BBBB		* numerator copy
; d4              MMMM  DDDD		* result MOD and DIV

; now multiply out 32 bit denominator by 16 bit result
; QRS = AB*D

	mulu	d3,d6			; FFFF * DDDD =       rrrr  SSSS
	mulu	d5,d4			; EEEE * DDDD = QQQQ  rrrr

; we now have ..
; d3              EEEE  FFFF		* denominator copy
; d5        0000  EEEE			* denominator high word
; d6                    rrrr  SSSS	* 48 bit result partial low
; d1              xxxx  DDDD		* result DIV copy
; d2              AAAA  BBBB		* numerator copy
; d4              QQQQ  rrrr		* 48 bit result partial

	move.w	d6,d7			; copy low word of low multiply

; d7                    xxxx  SSSS	* 48 bit result partial low

	clr.w	d6			; clear low word of low multiply
	swap	d6			; high word of low multiply to low word

; d6              0000  rrrr		* high word of 48 bit result partial low

	add.l	d6,d4

; d4              QQQQ  RRRR		* 48 bit result partial high longword

	moveq	#0,d6			; clear to extend numerator to 48 bits

; now do GHI = AB0 - QRS (which is the remainder)

	sub.w	d7,d6			; low word subtract

; d6                    xxxx  IIII	* remainder low word

	subx.l	d4,d2			; high longword subtract

; d2              GGGG  HHHH		* remainder high longword

; now if we got the divide correct then the remainder high longword will be +ve

	bpl.s	l_ddiv			; branch if result is ok (<needed)

; remainder was -ve so DDDD is too big

lab_remm:
	subq.w	#1,d1			; adjust DDDD

; d3                    xxxx  FFFF	* denominator copy
; d6                    xxxx  IIII	* remainder low word

	add.w	d3,d6			; add EF*1 low remainder low word

; d5              0000  EEEE		* denominator high word
; d2              GGGG  HHHH		* remainder high longword

	addx.l	d5,d2			; add extend EF*1 to remainder high longword
	bmi.s	lab_remm		; loop if result still too big

; all done and result correct or <

l_ddiv:
	swap	d2			; remainder mid word to high word

; d2              HHHH  GGGG		* (high word /should/ be $0000)

	move.w	d6,d2			; remainder in high word

; d2                    HHHH  IIII	* now is 32 bit remainder
; d1              xxxx  DDDD		* 16 bit result

	rts

; unpack memory (a0) into FAC1

lab_ufac:
	move.l	(a0),d0			; get packed value
	swap	d0			; exponent and sign into least significant word
	move.w	d0,fac1_e		; save exponent and sign
	or.w	#0x80,d0			; set MSb
	swap	d0			; byte order back to normal

	asl.l	#8,d0			; shift exponent & clear guard byte
	move.l	d0,fac1_m		; move into FAC1

	move.b	fac1_e,d0		; get FAC1 exponent
	rts

; set numeric variable, pack FAC1 into Lvarpl

lab_pfac:
	move.l	a0,-(sp)		; save pointer
	movea.l	lvarpl,a0		; get destination pointer
	btst	#6,dtypef		; test data type
	beq.s	lab_277c		; branch if floating

	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	move.l	d0,(a0)			; save in var
	move.l	(sp)+,a0		; restore pointer
	rts

; normalise round and pack FAC1 into (a0)

lab_2778:
	move.l	a0,-(sp)		; save pointer
lab_277c:
	bsr	lab_24d5		; normalise FAC1
	bsr.s	lab_27ba		; round FAC1
	move.l	fac1_m,d0		; get FAC1 mantissa
	ror.l	#8,d0			; align 24/32 bit mantissa
	swap	d0			; exponent/sign into 0-15
	and.w	#0x7f,d0			; clear exponent and sign bit
	andi.b	#0x80,fac1_s		; clear non sign bits in sign
	or.w	fac1_e,d0		; OR in exponent and sign
	swap	d0			; move exponent and sign  back to 16-31
	move.l	d0,(a0)			; store in destination
	move.l	(sp)+,a0		; restore pointer
	rts

; copy FAC2 to FAC1

lab_279b:
	move.w	fac2_e,fac1_e		; copy exponent & sign
	move.l	fac2_m,fac1_m		; copy mantissa
	rts

; round FAC1

lab_27ba:
	move.b	fac1_e,d0		; get FAC1 exponent
	beq.s	lab_27c4		; branch if zero

	move.l	fac1_m,d0		; get FAC1
	add.l	#0x80,d0			; round to 24 bit
	bcc.s	lab_27c3		; branch if no overflow

	roxr.l	#1,d0			; shift FAC1 mantissa
	addq.b	#1,fac1_e		; correct exponent
	bcs	lab_ofer		; if carry do overflow error & warm start

lab_27c3:
	and.b	#0x00,d0			; clear guard byte
	move.l	d0,fac1_m		; save back to FAC1
	rts

lab_27c4:
	move.b	d0,fac1_s		; make zero always +ve
rts_017:
	rts

; get FAC1 sign
; return d0=-1,C=1/-ve d0=+1,C=0/+ve

lab_27ca:
	moveq	#0,d0			; clear d0
	move.b	fac1_e,d0		; get FAC1 exponent
	beq.s	rts_017			; exit if zero (already correct SGN(0)=0)

; return d0=-1,C=1/-ve d0=+1,C=0/+ve
; no = 0 check

lab_27ce:
	move.b	fac1_s,d0		; else get FAC1 sign (b7)

; return d0=-1,C=1/-ve d0=+1,C=0/+ve
; no = 0 check, sign in d0

lab_27d0:
	ext.w	d0			; make word
	ext.l	d0			; make longword
	asr.l	#8,d0			; move sign bit through byte to carry
	bcs.s	rts_017			; exit if carry set

	moveq	#1,d0			; set result for +ve sign
	rts

; perform SGN()

lab_sgn:
	bsr.s	lab_27ca		; get FAC1 sign
					; return d0=-1/-ve d0=+1/+ve

; save d0 as integer longword

lab_27db:
	move.l	d0,fac1_m		; save FAC1 mantissa
	move.w	#0xa000,fac1_e		; set FAC1 exponent & sign
	add.l	d0,d0			; top bit into carry
	bra	lab_24d0		; do +/- (carry is sign) & normalise FAC1

; perform ABS()

lab_abs:
	move.b	#0,fac1_s		; clear FAC1 sign
	rts

; compare FAC1 with (a0)
; returns d0=+1 if FAC1 > FAC2
; returns d0= 0 if FAC1 = FAC2
; returns d0=-1 if FAC1 < FAC2

lab_27f8:
	bsr	lab_264d		; unpack memory (a0) into FAC2, trashes d0

; compare FAC1 with FAC2
; returns d0=+1 if FAC1 > FAC2
; returns d0= 0 if FAC1 = FAC2
; returns d0=-1 if FAC1 < FAC2

lab_27fa:
	move.b	fac2_e,d1		; get FAC2 exponent
	beq.s	lab_27ca		; branch if FAC2 exponent=0 & get FAC1 sign
					; d0=-1,C=1/-ve d0=+1,C=0/+ve

	move.b	fac_sc,d0		; get FAC sign compare
	bmi.s	lab_27ce		; if signs <> do return d0=-1,C=1/-ve
					; d0=+1,C=0/+ve & return

	move.b	fac1_s,d0		; get FAC1 sign
	cmp.b	fac1_e,d1		; compare FAC1 exponent with FAC2 exponent
	bne.s	lab_2828		; branch if different

	move.l	fac2_m,d1		; get FAC2 mantissa
	cmp.l	fac1_m,d1		; compare mantissas
	beq.s	lab_282f		; exit if mantissas equal

; gets here if number <> FAC1

lab_2828:
	bcs.s	lab_282e		; branch if FAC1 > FAC2

	eori.b	#0x80,d0			; else toggle FAC1 sign
lab_282e:
	bra.s	lab_27d0		; return d0=-1,C=1/-ve d0=+1,C=0/+ve

lab_282f:
	moveq	#0,d0			; clear result
rts_018:
	rts

; convert FAC1 floating-to-fixed
; result in d0 and Itemp

lab_2831:
	move.l	d1,-(sp)		; save d1
	move.l	fac1_m,d0		; copy mantissa
	move.b	fac1_e,d1		; get FAC1 exponent
	cmp.b	#0x81,d1			; compare with min
	bcs.s	lab_287f		; if <1 go clear FAC1 & return

	sub.b	#0xa0,d1			; compare maximum integer range exponent
	bne.s	lab_2844		; if not $A0, go test is less

	tst.b	fac1_s			; test FAC1 sign
	bpl.s	lab_2845		; branch if FAC1 +ve

					; FAC1 was -ve and exponent is $A0
	cmp.l	#0x80000000,d0		; compare with max -ve
	beq.s	lab_2845		; branch if max -ve

lab_2844:
	bcc	lab_ofer		; do overflow if too big

lab_2845:
	neg.b	d1			; convert -ve to +ve
	lsr.l	d1,d0			; shift integer

	tst.b	fac1_s			; test FAC1 sign (b7)
	bpl.s	lab_2846		; branch if FAC1 +ve

	neg.l	d0			; negate integer value
lab_2846:
	move.l	d0,itemp
	move.l	(sp)+,d1		; restore d1
	rts
					; set zero result
lab_287f:
	moveq	#0,d0			; clear result
	bra.s	lab_2846		; go save & exit

; perform INT()

lab_int:
	cmp.b	#0xa0,fac1_e		; compare FAC1 exponent with max int
	bcc.s	rts_018			; exit if >= (too big for fractional part!)

	move.w	#0xa000,d3		; set exponent for result
	move.b	fac1_s,d3		; get FAC1 sign
	move.b	#0,fac1_s		; make +ve
	bsr.s	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	move.w	d3,fac1_e		; set sign and exponent
	move.l	d0,fac1_m		; set mantissa
	bra	lab_24d5		; normalise FAC1 & return

; print " in line [LINE #]"

lab_2953:
	lea	lab_lmsg(pc),a0	; point to " in line " message
	bsr	lab_18c3		; print null terminated string

					; Print Basic line #
	move.l	clinel,d0		; get current line

; print d0 as unsigned integer

lab_295e:
	bsr.s	lab_2966		; convert 32 bit d0 to unsigned string (a0)
	bra	lab_18c3		; print null terminated string from memory & RET

; convert d0 to unsigned ASCII string result in (a0)

lab_2966:
	movea.l	#bin2dec,a1		; get table address
	moveq	#0,d1			; table index
	movea.l	#usdss,a0		; output string start
	move.l	d1,d2			; output string index
lab_2967:
	move.l	(a1,d1.w),d3		; get table value
	beq.s	lab_2969		; exit if end marker

	moveq	#'0-1,d4		; set character to "0"-1
lab_2968:
	addq.w	#1,d4			; next numeric character
	sub.l	d3,d0			; subtract table value
	bpl.s	lab_2968		; not overdone so loop

	add.l	d3,d0			; correct value
	move.b	d4,(a0,d2.w)		; character out to string
	addq.w	#4,d1			; increment table pointer
	addq.w	#1,d2			; increment output string pointer
	bra.s	lab_2967		; loop

lab_2969:
	add.b	#'0,d0			; make last character
	move.b	d0,(a0,d2.w)		; character out to string
	subq.w	#1,a0			; decrement a0 (allow simple loop)

					; now find non zero start of string
lab_296a:
	addq.w	#1,a0			; increment a0 (we know this will never carry to b16)
	cmpa.l	#(bhsend-1),a0		; are we at end
	beq.s	rts_019			; branch if so

	cmpi.b	#'0,(a0)		; is character "0" ?
	beq.s	lab_296a		; loop if so

rts_019:
	rts

;*lab_xxxx  ##
;*	bsr.s	lab_2831		* convert FAC1 floating-to-fixed
					; result in d0 and Itemp
;* now check not 0 and go print it ...

; convert FAC1 to ASCII string result in (a0)
; STR$() function enters here

; d0 is character out
; d1 is save index
; d2 is gash

; a0 is output pointer

lab_2970:
	bsr	lab_27ba		; round FAC1
	movea.l	#decss,a1		; set output string start

;* insert code here to test the numeric type and do integer if needed ##
;*	btst	#6,d0			* test the integer bit
;*	bne.s	lab_xxxx		* branch if integer

	move.b	#' ,(a1)		; character = " " (assume +ve)
	bclr.b	#7,fac1_s		; test and clear FAC1 sign (b7)
	beq.s	lab_2978		; branch if +ve

	move.b	#0x2d,(a1)		; else character = "-"
lab_2978:
	move.b	fac1_e,d2		; get FAC1 exponent
	bne.s	lab_2989		; branch if FAC1<>0

					; exponent was $00 so FAC1 is 0
	moveq	#'0,d0			; set character = "0"
	moveq	#1,d1			; set output string index
	bra	lab_2a89		; save last character, [EOT] & exit

					; FAC1 is some non zero value
lab_2989:
	move.b	#0,numexp		; clear number exponent count
	cmp.b	#0x81,d2			; compare FAC1 exponent with $81 (>1.00000)

	bcc.s	lab_299c		; branch if FAC1=>1

	move.l	#0xf4240000,fac2_m	; 1000000 mantissa
	move.w	#0x9400,fac2_e		; 1000000 exponent & sign
	move.b	fac1_s,fac_sc		; make FAC1 sign sign compare
	bsr	lab_multiply		; do FAC2*FAC1

	move.b	#0xfa,numexp		; set number exponent count (-6)
lab_299c:
	lea	lab_294b(pc),a0	; set pointer to 999999.4375
					; (max before scientific notation)
	bsr	lab_27f8		; compare FAC1 with (a0)
					; returns d0=+1 if FAC1 > FAC2
					; returns d0= 0 if FAC1 = FAC2
					; returns d0=-1 if FAC1 < FAC2
	beq.s	lab_29c3		; exit if FAC1 = (a0)

	bpl.s	lab_29b9		; go do /10 if FAC1 > (a0)

					; FAC1 < (a0)
lab_29a7:
	lea	lab_2947(pc),a0	; set pointer to 99999.9375
	bsr	lab_27f8		; compare FAC1 with (a0)
					; returns d0=+1 if FAC1 > FAC2
					; returns d0= 0 if FAC1 = FAC2
					; returns d0=-1 if FAC1 < FAC2
	beq.s	lab_29b2		; branch if FAC1 = (a0) (allow decimal places)

	bpl.s	lab_29c0		; branch if FAC1 > (a0) (no decimal places)

					; FAC1 <= (a0)
lab_29b2:
	bsr	lab_269e		; multiply FAC1 by 10
	subq.b	#1,numexp		; decrement number exponent count
	bra.s	lab_29a7		; go test again

lab_29b9:
	move.w	fac1_e,fac2_e		; copy exponent & sign from FAC1 to FAC2
	move.l	fac1_m,fac2_m		; copy FAC1 mantissa to FAC2 mantissa
	move.b	fac1_s,fac_sc		; save FAC1_s as sign compare

	move.l	#0xcccccccd,fac1_m	; 1/10 mantissa
	move.w	#0x7d00,fac1_e		; 1/10 exponent & sign
	bsr	lab_multiply		; do FAC2*FAC1, effectively divide by 10 but faster

	addq.b	#1,numexp		; increment number exponent count
	bra.s	lab_299c		; go test again (branch always)

					; now we have just the digits to do
lab_29c0:
	bsr	lab_244e		; add 0.5 to FAC1 (round FAC1)
lab_29c3:
	bsr	lab_2831		; convert FAC1 floating-to-fixed
					; result in d0 and Itemp
	moveq	#0x01,d2			; set default digits before dp = 1
	move.b	numexp,d0		; get number exponent count
	add.b	#7,d0			; allow 6 digits before point
	bmi.s	lab_29d9		; if -ve then 1 digit before dp

	cmp.b	#0x08,d0			; d0>=8 if n>=1E6
	bcc.s	lab_29d9		; branch if >= $08

					; < $08
	subq.b	#1,d0			; take 1 from digit count
	move.b	d0,d2			; copy byte
	moveq	#0x02,d0			; set exponent adjust
lab_29d9:
	moveq	#0,d1			; set output string index
	subq.b	#2,d0			; -2
	move.b	d0,expcnt		; save exponent adjust
	move.b	d2,numexp		; save digits before dp count
	move.b	d2,d0			; copy digits before dp count
	beq.s	lab_29e4		; branch if no digits before dp

	bpl.s	lab_29f7		; branch if digits before dp

lab_29e4:
	addq.l	#1,d1			; increment index
	move.b	#'.,(a1,d1.w)		; save to output string

	tst.b	d2			; test digits before dp count
	beq.s	lab_29f7		; branch if no digits before dp

	addq.l	#1,d1			; increment index
	move.b	#'0,(a1,d1.w)		; save to output string
lab_29f7:
	moveq	#0,d2			; clear index (point to 100,000)
	moveq	#0x80-0x100,d0		; set output character
lab_29fb:
	lea	lab_2a9a(pc),a0	; get base of table
	move.l	(a0,d2.w),d3		; get table value
lab_29fd:
	addq.b	#1,d0			; increment output character
	add.l	d3,itemp		; add to (now fixed) mantissa
	btst	#7,d0			; set test sense (z flag only)
	bcs.s	lab_2a18		; did carry so has wrapped past zero

	beq.s	lab_29fd		; no wrap and +ve test so try again

	bra.s	lab_2a1a		; found this digit

lab_2a18:
	bne.s	lab_29fd		; wrap and -ve test so try again

lab_2a1a:
	bcc.s	lab_2a21		; branch if +ve test result

	neg.b	d0			; negate number
	add.b	#0x0b,d0			; and effectively subtract from 11d
lab_2a21:
	add.b	#0x2f,d0			; add "0"-1 to result
	addq.w	#4,d2			; increment index to next less power of ten
	addq.w	#1,d1			; increment output string index
	move.b	d0,d3			; copy character to d3
	and.b	#0x7f,d3			; mask out top bit
	move.b	d3,(a1,d1.w)		; save to output string
	sub.b	#1,numexp		; decrement # of characters before the dp
	bne.s	lab_2a3b		; branch if still characters to do

					; else output the point
	addq.l	#1,d1			; increment index
	move.b	#'.,(a1,d1.w)		; save to output string
lab_2a3b:
	and.b	#0x80,d0			; mask test sense bit
	eori.b	#0x80,d0			; invert it
	cmp.b	#0x18,d2			; compare table index with max+4
	bne.s	lab_29fb		; loop if not max

					; now remove trailing zeroes
lab_2a4b:
	move.b	(a1,d1.w),d0		; get character from output string
	subq.l	#1,d1			; decrement output string index
	cmp.b	#'0,d0			; compare with "0"
	beq.s	lab_2a4b		; loop until non "0" character found

	cmp.b	#'.,d0			; compare with "."
	beq.s	lab_2a58		; branch if was dp

					; else restore last character
	addq.l	#1,d1			; increment output string index
lab_2a58:
	move.b	#'+,2(a1,d1.w)		; save character "+" to output string
	tst.b	expcnt			; test exponent count
	beq.s	lab_2a8c		; if zero go set null terminator & exit

					; exponent isn't zero so write exponent
	bpl.s	lab_2a68		; branch if exponent count +ve

	move.b	#'-,2(a1,d1.w)		; save character "-" to output string
	neg.b	expcnt			; convert -ve to +ve
lab_2a68:
	move.b	#'E,1(a1,d1.w)		; save character "E" to output string
	move.b	expcnt,d2		; get exponent count
	moveq	#0x2f,d0			; one less than "0" character
lab_2a74:
	addq.b	#1,d0			; increment 10's character
	sub.b	#0x0a,d2			; subtract 10 from exponent count
	bcc.s	lab_2a74		; loop while still >= 0

	add.b	#0x3a,d2			; add character ":" ($30+$0A, result is 10-value)
	move.b	d0,3(a1,d1.w)		; save 10's character to output string
	move.b	d2,4(a1,d1.w)		; save 1's character to output string
	move.b	#0,5(a1,d1.w)		; save null terminator after last character
	bra.s	lab_2a91		; go set string pointer (a0) and exit

lab_2a89:
	move.b	d0,(a1,d1.w)		; save last character to output string
lab_2a8c:
	move.b	#0,1(a1,d1.w)		; save null terminator after last character
lab_2a91:
	movea.l	a1,a0			; set result string pointer (a0)
	rts

lab_poon:
	move.l	#0x80000000,fac1_m	; 1 mantissa
	move.w	#0x8100,fac1_e		; 1 exonent & sign
	rts

lab_poze:
	moveq	#0,d0			; clear longword
	move.l	d0,fac1_m		; 0 mantissa
	move.w	d0,fac1_e		; 0 exonent & sign
	rts

; Perform power function
; The number is in FAC2, the power is in FAC1
; no longer trashes Itemp

lab_power:
	tst.b	fac1_e			; test power
	beq.s	lab_poon		; if zero go return 1

	tst.b	fac2_e			; test number
	beq.s	lab_poze		; if zero go return 0

	move.b	fac2_s,-(sp)		; save number sign
	bpl.s	lab_powp		; power of positive number

	moveq	#0,d1			; clear d1
	move.b	d1,fac2_s		; make sign +ve

					; number sign was -ve, must have integer power
					; or do 'function call' error
	move.b	fac1_e,d1		; get power exponent
	sub.w	#0x80,d1			; normalise to .5
	bls	lab_fcer		; if 0<power<1 then do 'function call' error

					; now shift all the integer bits out
	move.l	fac1_m,d0		; get power mantissa
	asl.l	d1,d0			; shift mantissa
	bne	lab_fcer		; if power<>INT(power) then do 'function call' error

	bcs.s	lab_powp		; if integer value odd then leave result -ve

	move.b	d0,(sp)			; save result sign +ve
lab_powp:
	move.l	fac1_m,-(sp)		; save power mantissa
	move.w	fac1_e,-(sp)		; save power sign & exponent

	bsr	lab_279b		; copy number to FAC1
	bsr	lab_log			; find log of number

	move.w	(sp)+,d0		; get power sign & exponent
	move.l	(sp)+,fac2_m		; get power mantissa
	move.w	d0,fac2_e		; save sign & exponent to FAC2
	move.b	d0,fac_sc		; save sign as sign compare
	move.b	fac1_s,d0		; get FAC1 sign
	eor.b	d0,fac_sc		; make sign compare (FAC1_s EOR FAC2_s)

	bsr	lab_multiply		; multiply by power
	bsr.s	lab_exp			; find exponential
	move.b	(sp)+,fac1_s		; restore number sign
	rts

; Ffp ABS/NEG - make absolute or -ve equivalent of FAC1

	tst.b	fac1_s			; test sign byte
	beq.s	rts_020			; exit if +ve

; do - FAC1

lab_gthan:
	tst.b	fac1_e			; test for non zero FAC1
	beq.s	rts_020			; branch if null

	eori.b	#0x80,fac1_s		; (else) toggle FAC1 sign bit
rts_020:
	rts

					; return +1
lab_ex1:
	move.l	#0x80000000,fac1_m	; +1 mantissa
	move.w	#0x8100,fac1_e		; +1 sign & exponent
	rts
					; do over/under flow
lab_exou:
	tst.b	fac1_s			; test sign
	bpl	lab_ofer		; was +ve so do overflow error

					; else underflow so return zero
	moveq	#0,d0			; clear longword
	move.l	d0,fac1_m		; 0 mantissa
	move.w	d0,fac1_e		; 0 sign & exponent
	rts
					; fraction was zero so do 2^n
lab_exof:
	move.l	#0x80000000,fac1_m	; +n mantissa
	move.b	#0,fac1_s		; clear sign
	add.b	#0x80,d1			; adjust exponent
	move.b	d1,fac1_e		; save exponent
	rts

; perform EXP()   (x^e)
; valid input range is -88 to +88

lab_exp:
	move.b	fac1_e,d0		; get exponent
	beq.s	lab_ex1			; return 1 for zero in

	cmp.b	#0x64,d0			; compare exponent with min
	bcs.s	lab_ex1			; if smaller just return 1

;*	movem.l	d1-d6/a0,-(sp)		* save the registers
	move.b	#0,cosout		; flag +ve number
	move.l	fac1_m,d1		; get mantissa
	cmp.b	#0x87,d0			; compare exponent with max
	bhi.s	lab_exou		; go do over/under flow if greater

	bne.s	lab_excm		; branch if less

					; else is 2^7
	cmp.l	#0xb00f33c7,d1		; compare mantissa with n*2^7 max
	bcc.s	lab_exou		; if => go over/underflow

lab_excm:
	tst.b	fac1_s			; test sign
	bpl.s	lab_exps		; branch if arg +ve

	move.b	#0xff,cosout		; flag +ve number
	move.b	#0,fac1_s		; take absolute value
lab_exps:
					; now do n/LOG(2)
	move.l	#0xb8aa3b29,fac2_m	; 1/LOG(2) mantissa
	move.w	#0x8100,fac2_e		; 1/LOG(2) exponent & sign
	move.b	#0,fac_sc		; we know they're both +ve
	bsr	lab_multiply		; effectively divide by log(2)

					; max here is +/- 127
					; now separate integer and fraction
	move.b	#0,tpower		; clear exponent add byte
	move.b	fac1_e,d5		; get exponent
	sub.b	#0x80,d5			; normalise
	bls.s	lab_esml		; branch if < 1 (d5 is 0 or -ve)

					; result is > 1
	move.l	fac1_m,d0		; get mantissa
	move.l	d0,d1			; copy it
	move.l	d5,d6			; copy normalised exponent

	neg.w	d6			; make -ve
	add.w	#32,d6			; is now 32-d6
	lsr.l	d6,d1			; just integer bits
	move.b	d1,tpower		; set exponent add byte

	lsl.l	d5,d0			; shift out integer bits
	beq	lab_exof		; fraction is zero so do 2^n

	move.l	d0,fac1_m		; fraction to FAC1
	move.w	#0x8000,fac1_e		; set exponent & sign

					; multiple was < 1
lab_esml:
	move.l	#0xb17217f8,fac2_m	; LOG(2) mantissa
	move.w	#0x8000,fac2_e		; LOG(2) exponent & sign
	move.b	#0,fac_sc		; clear sign compare
	bsr	lab_multiply		; multiply by log(2)

	move.l	fac1_m,d0		; get mantissa
	move.b	fac1_e,d5		; get exponent
	sub.w	#0x82,d5			; normalise and -2 (result is -1 to -30)
	neg.w	d5			; make +ve
	lsr.l	d5,d0			; shift for 2 integer bits

; d0 = arg
; d6 = x, d1 = y
; d2 = x1, d3 = y1
; d4 = shift count
; d5 = loop count
					; now do cordic set-up
	moveq	#0,d1			; y = 0
	move.l	#kfctseed,d6		; x = 1 with jkh inverse factored out
	movea.l	#tab_hthet,a0		; pointer to hyperbolic arctan table
	moveq	#0,d4			; clear shift count

					; cordic loop, shifts 4 and 13 (and 39
					; if it went that far) need to be repeated
	moveq	#3,d5			; 4 loops
	bsr.s	lab_excc		; do loops 1 through 4
	subq.w	#4,a0			; do table entry again
	subq.l	#1,d4			; do shift count again
	moveq	#9,d5			; 10 loops
	bsr.s	lab_excc		; do loops 4 (again) through 13
	subq.w	#4,a0			; do table entry again
	subq.l	#1,d4			; do shift count again
	moveq	#18,d5			; 19 loops
	bsr.s	lab_excc		; do loops 13 (again) through 31

					; now get the result
	tst.b	cosout			; test sign flag
	bpl.s	lab_expl		; branch if +ve

	neg.l	d1			; do -y
	neg.b	tpower			; do -exp
lab_expl:
	moveq	#0x83-0x100,d0		; set exponent
	add.l	d1,d6			; y = y +/- x
	bmi.s	lab_exrn		; branch if result normal

lab_exnn:
	subq.l	#1,d0			; decrement exponent
	add.l	d6,d6			; shift mantissa
	bpl.s	lab_exnn		; loop if not normal

lab_exrn:
	move.l	d6,fac1_m		; save exponent result
	add.b	tpower,d0		; add integer part
	move.b	d0,fac1_e		; save exponent
;*	movem.l	(sp)+,d1-d6/a0		* restore registers
	rts

					; cordic loop
lab_excc:
	addq.l	#1,d4			; increment shift count
	move.l	d6,d2			; x1 = x
	asr.l	d4,d2			; x1 >> n
	move.l	d1,d3			; y1 = y
	asr.l	d4,d3			; y1 >> n
	tst.l	d0			; test arg
	bmi.s	lab_exad		; branch if -ve

	add.l	d2,d1			; y = y + x1
	add.l	d3,d6			; x = x + y1
	sub.l	(a0)+,d0		; arg = arg - atnh(a0)
	dbf	d5,lab_excc		; decrement and loop if not done

	rts

lab_exad:
	sub.l	d2,d1			; y = y - x1
	sub.l	d3,d6			; x = x + y1
	add.l	(a0)+,d0		; arg = arg + atnh(a0)
	dbf	d5,lab_excc		; decrement and loop if not done

	rts

; RND(n), 31 bit version. make n=0 for 5th next number in sequence or n<>0 to get
; 5th next number in sequence after seed n. Taking the 5th next number is slower
; but helps hide the shift & add nature of this generator.

lab_rnd:
	move.b	fac1_e,d0		; get FAC1 exponent
	beq.s	nextprn			; do next random # if zero

					; else get seed into random number store
	movea.l	#prnlword,a0		; set PRNG pointer
	bsr	lab_2778		; pack FAC1 into (a0)

nextprn:
	moveq	#4,d2			; do this 5 times
	move.l	prnlword,d0		; get current
ninc0:
	moveq	#0,d1			; clear bit count
	ror.l	#2,d0			; bit 31 -> carry
	bcc.s	ninc1			; skip increment if =0

	addq.b	#1,d1			; else increment bit count
ninc1:
	ror.l	#3,d0			; bit 28 -> carry
	bcc.s	ninc2			; skip increment if =0

	addq.b	#1,d1			; else increment bit count
ninc2:
	rol.l	#5,d0			; restore PRNG longword
	roxr.b	#1,d1			; EOR bit into Xb
	roxr.l	#1,d0			; shift bit to most significant
	dbf	d2,ninc0		; loop 5 times

	move.l	d0,prnlword		; save back to seed word
	move.l	d0,fac1_m		; save to FAC1

	move.w	#0x8000,fac1_e		; set the exponent and clear the sign
	bra	lab_24d5		; normalise FAC1 & return

; cordic TAN(x) routine, TAN(x) = SIN(x)/COS(x)
; x = angle in radians

lab_tan:
	bsr.s	lab_sccc		; go do SIN/COS cordic compute
	bsr	lab_24d5		; normalise FAC1
	move.w	fac1_e,fac2_e		; copy exponent & sign from FAC1 to FAC2
	move.l	fac1_m,fac2_m		; copy FAC1 mantissa to FAC2 mantissa
	move.l	d1,fac1_m		; get COS(x) mantissa
	move.b	d3,fac1_e		; get COS(x) exponent
	beq	lab_ofer		; do overflow if COS = 0

	bsr	lab_24d5		; normalise FAC1
	bra	lab_divide		; do FAC2/FAC1 & return (FAC_sc set by SIN COS calc)

; cordic SIN(x), COS(x) routine
; x = angle in radians

lab_cos:
	move.l	#0xc90fdaa3,fac2_m	; pi/2 mantissa (b2 is set so COS(PI/2)=0)
	move.w	#0x8100,fac2_e		; pi/2 exponent and sign
	move.b	fac1_s,fac_sc		; sign = FAC1 sign (b7)
	bsr	lab_add			; add FAC2 to FAC1, adjust for COS(x)
lab_sin:
	bsr.s	lab_sccc		; go do SIN/COS cordic compute
	bra	lab_24d5		; normalise FAC1 & return

; SIN/COS cordic calculator

lab_sccc:
	move.b	#0,cosout		; set needed result

	move.l	#0xa2f9836f,fac2_m	; 1/pi mantissa (LSB is rounded up so SIN(PI)=0)
	move.w	#0x7f00,fac2_e		; 1/pi exponent & sign
	move.b	fac1_s,fac_sc		; sign = FAC1 sign (b7)
	bsr	lab_multiply		; multiply by 1/pi

	move.b	fac1_e,d0		; get FAC1 exponent
	beq	lab_scze		; branch if zero

	movea.l	#tab_snco,a0		; point to constants table
	move.l	fac1_m,d6		; get FAC1 mantissa
	subq.b	#1,d0			; 2 radians in 360 degrees so /2
	beq	lab_scze		; branch if zero

	sub.b	#0x80,d0			; normalise exponent
	bmi.s	lab_scl0		; branch if < 1

					; X is > 1
	cmp.b	#0x20,d0			; is it >= 2^32
	bcc	lab_scze		; may as well do zero

	lsl.l	d0,d6			; shift out integer part bits
	beq	lab_scze		; no fraction so go do zero

	bra.s	lab_cord		; go test quadrant and adjust

					; x is < 1
lab_scl0:
	neg.b	d0			; make +ve
	cmp.b	#0x1e,d0			; is it <= 2^-30
	bcc.s	lab_scze		; may as well do zero

	lsr.l	d0,d6			; shift out <= 2^-32 bits

; cordic calculator, arguament in d6
; table pointer in a0, returns in d0-d3

lab_cord:
	move.b	fac1_s,fac_sc		; copy as sign compare for TAN
	add.l	d6,d6			; shift 0.5 bit into carry
	bcc.s	lab_ltpf		; branch if less than 0.5

	eori.b	#0xff,fac1_s		; toggle result sign
lab_ltpf:
	add.l	d6,d6			; shift 0.25 bit into carry
	bcc.s	lab_ltpt		; branch if less than 0.25

	eori.b	#0xff,cosout		; toggle needed result
	eori.b	#0xff,fac_sc		; toggle sign compare for TAN

lab_ltpt:
	lsr.l	#2,d6			; shift the bits back (clear integer bits)
	beq.s	lab_scze		; no fraction so go do zero

					; set start values
	moveq	#1,d5			; set bit count
	move.l	-4(a0),d0		; get multiply constant (1st itteration d0)
	move.l	d0,d1			; 1st itteration d1
	sub.l	(a0)+,d6		; 1st always +ve so do 1st step
	bra.s	mainloop		; jump into routine

subloop:
	sub.l	(a0)+,d6		; z = z - arctan(i)/2pi
	sub.l	d3,d0			; x = x - y1
	add.l	d2,d1			; y = y + x1
	bra.s	nexta			; back to main loop

mainloop:
	move.l	d0,d2			; x1 = x
	asr.l	d5,d2			; / (2 ^ i)
	move.l	d1,d3			; y1 = y
	asr.l	d5,d3			; / (2 ^ i)
	tst.l	d6			; test sign (is 2^0 bit)
	bpl.s	subloop			; go do subtract if > 1

	add.l	(a0)+,d6		; z = z + arctan(i)/2pi
	add.l	d3,d0			; x = x + y1
	sub.l	d2,d1			; y = y + x1
nexta:
	addq.l	#1,d5			; i = i + 1
	cmp.l	#0x1e,d5			; check end condition
	bne.s	mainloop		; loop if not all done

					; now untangle output value
	moveq	#0x81-0x100,d2		; set exponent for 0 to .99 rec.
	move.l	d2,d3			; copy it for cos output
outloop:
	tst.b	cosout			; did we want cos output?
	bmi.s	subexit			; if so skip

	exg	d0,d1			; swap SIN and COS mantissas
	exg	d2,d3			; swap SIN and COS exponents
subexit:
	move.l	d0,fac1_m		; set result mantissa
	move.b	d2,fac1_e		; set result exponent
rts_021:
	rts

					; set values for 0/1
lab_scze:
	moveq	#0x81-0x100,d2		; set exponent for 1.0
	moveq	#0,d3			; set exponent for 0.0
	move.l	#0x80000000,d0		; mantissa for 1.0
	move.l	d3,d1			; mantissa for 0.0
	bra.s	outloop			; go output it

; perform ATN()

lab_atn:
	move.b	#0,cosout		; set needed result
	move.b	fac1_e,d0		; get FAC1 exponent
	cmp.b	#0x81,d0			; compare exponent with 1
	bcs.s	lab_atlo		; branch if FAC1<1

	lea	lab_259c(pc),a0	; set 1 pointer
	bsr	lab_26ca		; convert a0 and do (a0)/FAC1
	move.b	#0xff,cosout		; set needed result
lab_atlo:
	move.l	fac1_m,d0		; get FAC1 mantissa
	add.b	fac1_e,d1		; get FAC1 exponent (always <= 1)
	ext.w	d1			; make word
	ext.l	d1			; make word
	neg.l	d1			; change to +ve
	addq.l	#2,d1			; +2
	cmp.b	#11,d1			; compare with 2^-11
	bcc.s	rts_021			; x = ATN(x) so skip calc

	lsr.l	d1,d0			; shift in two integer part bits
	beq.s	lab_scze		; zero so go do zero

	movea.l	#tab_atnc,a0		; pointer to arctan table
	moveq	#0,d6			; Z = 0
	move.l	#1<<30,d1		; y = 1
	moveq	#29,d5			; loop 30 times
	moveq	#1,d4			; shift counter
	bra.s	lab_atcd		; enter loop

lab_atnp:
	asr.l	d4,d2			; x1 / 2^i
	add.l	d2,d1			; y = y + x1
	add.l	(a0),d6			; z = z + atn(i)
lab_atcd:
	move.l	d0,d2			; x1 = x
	move.l	d1,d3			; y1 = y
	asr.l	d4,d3			; y1 / 2^i
lab_catn:
	sub.l	d3,d0			; x = x - y1
	bpl.s	lab_atnp		; branch if x >= 0

	move.l	d2,d0			; else get x back
	addq.w	#4,a0			; increment pointer
	addq.l	#1,d4			; increment i
	asr.l	#1,d3			; y1 / 2^i
	dbf	d5,lab_catn		; decrement and loop if not done

	move.b	#0x82,fac1_e		; set new exponent
	move.l	d6,fac1_m		; save mantissa
	bsr	lab_24d5		; normalise FAC1

	tst.b	cosout			; was it > 1 ?
	bpl	rts_021			; branch if not

	move.b	fac1_s,d7		; get sign
	move.b	#0,fac1_s		; clear sign
	move.l	#0xc90fdaa2,fac2_m	; set -(pi/2)
	move.w	#0x8180,fac2_e		; set exponent and sign
	move.b	#0xff,fac_sc		; set sign compare
	bsr	lab_add			; perform addition, FAC2 to FAC1
	move.b	d7,fac1_s		; restore sign
	rts

; perform BITSET

lab_bitset:
	bsr	lab_gadb		; get two parameters for POKE or WAIT
					; first parameter in a0, second in d0
	cmp.b	#0x08,d0			; only 0 to 7 are allowed
	bcc	lab_fcer		; branch if > 7

	moveq	#0x02,d1			; set value
	asr.b	#1,d1			; set Xb and value
	roxl.b	d0,d1			; move set bit
	or.b	(a0),d1			; OR with byte
	move.b	d1,(a0)			; save byte
	rts

; perform BITCLR

lab_bitclr:
	bsr	lab_gadb		; get two parameters for POKE or WAIT
					; first parameter in a0, second in d0
	cmp.b	#0x08,d0			; only 0 to 7 are allowed
	bcc	lab_fcer		; branch if > 7

	moveq	#0xff-0x100,d1		; set value
	asl.b	#1,d1			; set Xb and value
	roxl.b	d0,d1			; move cleared bit
	and.b	(a0),d1			; AND with byte
	move.b	d1,(a0)			; save byte
	rts

; perform BITTST()

lab_btst:
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_gadb		; get two parameters for POKE or WAIT
					; first parameter in a0, second in d0
	cmp.b	#0x08,d0			; only 0 to 7 are allowed
	bcc	lab_fcer		; branch if > 7

	move.l	d0,d1			; copy bit # to test
	bsr	lab_gbyt		; get next BASIC byte
	cmp.b	#'),d0			; is next character ")"
	bne	lab_sner		; if not ")" go do syntax error, then warm start

	bsr	lab_igby		; update execute pointer (to character past ")")
	moveq	#0,d0			; set the result as zero
	btst	d1,(a0)			; test bit
	beq	lab_27db		; branch if zero (already correct)

	moveq	#-1,d0			; set for -1 result
	bra	lab_27db		; go do SGN tail

; perform BIN$()
; # of leading 0s is in d1, the number is in Itemp

lab_bins:
	cmp.b	#0x21,d1			; max + 1
	bcc	lab_fcer		; exit if too big ( > or = )

	move.l	itemp,d0		; get number back
	moveq	#0x1f,d2			; bit count-1
	movea.l	#binss,a0		; point to string
	moveq	#0x30,d4			; "0" character for ADDX
nextb1:
	moveq	#0,d3			; clear byte
	lsr.l	#1,d0			; shift bit into Xb
	addx.b	d4,d3			; add carry and character to zero
	move.b	d3,(a0,d2.w)		; save character to string
	dbf	d2,nextb1		; decrement and loop if not done

; this is the exit code and is also used by HEX$()

endbhs:
	move.b	#0,bhsend		; null terminate the string
	tst.b	d1			; test # of characters
	beq.s	nextb2			; go truncate string

	neg.l	d1			; make -ve
	add.l	#bhsend,d1		; effectively (end-length)
	movea.l	d1,a0			; move to pointer
	bra.s	binpr			; go print string

; truncate string to remove leading "0"s

nextb2:
	move.b	(a0),d0			; get byte
	beq.s	binpr			; if null then end of string so add 1 and print it

	cmp.b	#'0,d0			; compare with "0"
	bne.s	gopr			; if not "0" then go print string from here

	addq.w	#1,a0			; else increment pointer
	bra.s	nextb2			; loop always

; make fixed length output string - ignore overflows!

binpr:
	cmpa.l	#bhsend,a0		; are we at the string end
	bne.s	gopr			; branch if not

	subq.w	#1,a0			; else need at least one zero
gopr:
	bsr	lab_igby		; update execute pointer (to character past ")")
	addq.w	#4,sp			; bypass type check on exit
	bra	lab_20ae		; print " terminated string to FAC1, stack & RET

; perform HEX$()

lab_hexs:
	cmp.b	#0x09,d1			; max + 1
	bcc	lab_fcer		; exit if too big ( > or = )

	move.l	itemp,d0		; get number back
	moveq	#0x07,d2			; nibble count-1
	movea.l	#hexss,a0		; point to string
	moveq	#0x30,d4			; "0" character for ABCD
nexth1:
	move.b	d0,d3			; copy lowest byte
	ror.l	#4,d0			; shift nibble into 0-3
	and.b	#0x0f,d3			; just this nibble
	move.b	d3,d5			; copy it
	add.b	#0xf6,d5			; set extend bit
	abcd	d4,d3			; decimal add extend and character to zero
	move.b	d3,(a0,d2.w)		; save character to string
	dbf	d2,nexth1		; decrement and loop if not done

	bra.s	endbhs			; go process string

; ctrl-c check routine. includes limited "life" byte save for INGET routine
; now also the code that checks to see if an interrupt has occurred

vec_cc:
	tst.b	ccflag			; check [CTRL-C] check flag
	bne.s	rts_022			; exit if inhibited

	jsr	v_inpt			; scan input device
	bcc.s	lab_fba0		; exit if buffer empty

	move.b	d0,ccbyte		; save received byte
	move.b	#0x20,ccnull		; set "life" timer for bytes countdown
	bra	lab_1636		; return to BASIC

lab_fba0:
	tst.b	ccnull			; get countdown byte
	beq.s	rts_022			; exit if finished

	subq.b	#1,ccnull		; else decrement countdown
rts_022:
	rts

; get byte from input device, no waiting
; returns with carry set if byte in A

inget:
	jsr	v_inpt			; call scan input device
	bcs.s	lab_fb95		; if byte go reset timer

	move.b	ccnull,d0		; get countdown
	beq.s	rts_022			; exit if empty

	move.b	ccbyte,d0		; get last received byte
lab_fb95:
	move.b	#0x00,ccnull		; clear timer because we got a byte
	ori.b	#1,ccr			; set carry, flag we got a byte
	rts

; perform MAX()

lab_max:
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch error/warm start
lab_maxn:
	bsr.s	lab_phfa		; push FAC1, evaluate expression,
					; pull FAC2 & compare with FAC1
	bpl.s	lab_maxn		; branch if no swap to do

	bsr	lab_279b		; copy FAC2 to FAC1
	bra.s	lab_maxn		; go do next

; perform MIN()

lab_min:
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch
lab_minn:
	bsr.s	lab_phfa		; push FAC1, evaluate expression,
					; pull FAC2 & compare with FAC1
	bmi.s	lab_minn		; branch if no swap to do

	beq.s	lab_minn		; branch if no swap to do

	bsr	lab_279b		; copy FAC2 to FAC1
	bra.s	lab_minn		; go do next (branch always)

; exit routine. don't bother returning to the loop code
; check for correct exit, else so syntax error

lab_mmec:
	cmp.b	#'),d0			; is it end of function?
	bne	lab_sner		; if not do MAX MIN syntax error

	addq.w	#4,sp			; dump return address
	bsr	lab_igby		; update BASIC execute pointer (to chr past ")")
	rts

; check for next, evaluate & return or exit
; this is the routine that does most of the work

lab_phfa:
	bsr	lab_gbyt		; get next BASIC byte
	cmp.b	#',,d0			; is there more ?
	bne.s	lab_mmec		; if not go do end check

	move.w	fac1_e,-(sp)		; push exponent and sign
	move.l	fac1_m,-(sp)		; push mantissa

	bsr	lab_igby		; scan & get next BASIC byte (after ",")
	bsr	lab_evnm		; evaluate expression & check is numeric,
					; else do type mismatch

					; pop FAC2 (MAX/MIN expression so far)
	move.l	(sp)+,fac2_m		; pop mantissa
	move.w	(sp)+,fac2_e		; pop exponent and sign

	move.b	fac2_s,fac_sc		; save FAC2 sign as sign compare
	move.b	fac1_s,d0		; get FAC1 sign
	eor.b	d0,fac_sc		; EOR to create sign compare

	bra	lab_27fa		; compare FAC1 with FAC2 & return
					; returns d0=+1 if FAC1 > FAC2
					; returns d0= 0 if FAC1 = FAC2
					; returns d0=-1 if FAC1 < FAC2

; perform WIDTH

lab_wdth:
	cmp.b	#',,d0			; is next byte ","
	beq.s	lab_tbsz		; if so do tab size

	bsr	lab_gtby		; get byte parameter, result in d0 and Itemp
	tst.b	d0			; test result
	beq.s	lab_nstt		; branch if set for infinite line

	cmp.b	#0x10,d0			; else make min width = 16d
	bcs	lab_fcer		; if less do function call error & exit

; this next compare ensures that we can't exit WIDTH via an error leaving the
; tab size greater than the line length.

	cmp.b	tabsiz,d0		; compare with tab size
	bcc.s	lab_nstt		; branch if >= tab size

	move.b	d0,tabsiz		; else make tab size = terminal width
lab_nstt:
	move.b	d0,twidth		; set the terminal width
	bsr	lab_gbyt		; get BASIC byte back
	beq.s	wexit			; exit if no following

	cmp.b	#',,d0			; else is it ","
	bne	lab_sner		; if not do syntax error

lab_tbsz:
	bsr	lab_sgby		; increment and get byte, result in d0 and Itemp
	tst.b	d0			; test TAB size
	bmi	lab_fcer		; if >127 do function call error & exit

	cmp.b	#1,d0			; compare with min-1
	bcs	lab_fcer		; if <=1 do function call error & exit

	move.b	twidth,d1		; set flags for width
	beq.s	lab_svtb		; skip check if infinite line

	cmp.b	twidth,d0		; compare TAB with width
	bgt	lab_fcer		; branch if too big

lab_svtb:
	move.b	d0,tabsiz		; save TAB size

; calculate tab column limit from TAB size. The Iclim is set to the last tab
; position on a line that still has at least one whole tab width between it
; and the end of the line.

wexit:
	move.b	twidth,d0		; get width
	beq.s	lab_wdlp		; branch if infinite line

	cmp.b	tabsiz,d0		; compare with tab size
	bcc.s	lab_wdlp		; branch if >= tab size

	move.b	d0,tabsiz		; else make tab size = terminal width
lab_wdlp:
	sub.b	tabsiz,d0		; subtract tab size
	bcc.s	lab_wdlp		; loop while no borrow

	add.b	tabsiz,d0		; add tab size back
	add.b	tabsiz,d0		; add tab size back again

	neg.b	d0			; make -ve
	add.b	twidth,d0		; subtract remainder from width
	move.b	d0,iclim		; save tab column limit
rts_023:
	rts

; perform SQR()

; d0 is number to find the root of
; d1 is the root result
; d2 is the remainder
; d3 is a counter
; d4 is temp

lab_sqr:
	tst.b	fac1_s			; test FAC1 sign
	bmi	lab_fcer		; if -ve do function call error

	tst.b	fac1_e			; test exponent
	beq.s	rts_023			; exit if zero

	movem.l	d1-d4,-(sp)		; save registers
	move.l	fac1_m,d0		; copy FAC1
	moveq	#0,d2			; clear remainder
	move.l	d2,d1			; clear root

	moveq	#0x1f,d3			; $1F for DBF, 64 pairs of bits to
					; do for a 32 bit result
	btst	#0,fac1_e		; test exponent odd/even
	bne.s	lab_sqe2		; if odd only 1 shift first time

lab_sqe1:
	add.l	d0,d0			; shift highest bit of number ..
	addx.l	d2,d2			; .. into remainder .. never overflows
	add.l	d1,d1			; root = root * 2 .. never overflows
lab_sqe2:
	add.l	d0,d0			; shift highest bit of number ..
	addx.l	d2,d2			; .. into remainder .. never overflows

	move.l	d1,d4			; copy root
	add.l	d4,d4			; 2n
	addq.l	#1,d4			; 2n+1

	cmp.l	d4,d2			; compare 2n+1 to remainder
	bcs.s	lab_sqns		; skip sub if remainder smaller

	sub.l	d4,d2			; subtract temp from remainder
	addq.l	#1,d1			; increment root
lab_sqns:
	dbf	d3,lab_sqe1		; loop if not all done

	move.l	d1,fac1_m		; save result mantissa
	move.b	fac1_e,d0		; get exponent (d0 is clear here)
	sub.w	#0x80,d0			; normalise
	lsr.w	#1,d0			; /2
	bcc.s	lab_sqna		; skip increment if carry clear

	addq.w	#1,d0			; add bit zero back in (allow for half shift)
lab_sqna:
	add.w	#0x80,d0			; re-bias to $80
	move.b	d0,fac1_e		; save it
	movem.l	(sp)+,d1-d4		; restore registers
	bra	lab_24d5		; normalise FAC1 & return

; perform VARPTR()

lab_varptr:
	bsr	lab_1bfe		; scan for "(" , else do syntax error/warm start
	bsr	lab_gvar		; get var address
					; return pointer to variable in Cvaral and a0
	bsr	lab_1bfb		; scan for ")" , else do syntax error/warm start
	move.l	cvaral,d0		; get var address
	bra	lab_ayfc		; convert d0 to signed longword in FAC1 & return

; perform PI

lab_pi:
	move.b	#0x00,dtypef		; clear data type flag, $00=float
	move.l	#0xc90fdaa2,fac1_m	; pi mantissa (32 bit)
	move.w	#0x8200,fac1_e		; pi exponent and sign
	rts

; perform TWOPI

lab_twopi:
	move.b	#0x00,dtypef		; clear data type flag, $00=float
	move.l	#0xc90fdaa2,fac1_m	; 2pi mantissa (32 bit)
	move.w	#0x8300,fac1_e		; 2pi exponent and sign
	rts

; get ASCII string equivalent into FAC1 as integer32 or float

; entry is with a5 pointing to the first character of the string
; exit with a5 pointing to the first character after the string

; d0 is character
; d1 is mantissa
; d2 is partial and table mantissa
; d3 is mantissa exponent (decimal & binary)
; d4 is decimal exponent

; get FAC1 from string
; this routine now handles hex and binary values from strings
; starting with "$" and "%" respectively

lab_2887:
	movem.l	d1-d5,-(sp)		; save registers
	moveq	#0x00,d1			; clear temp accumulator
	move.l	d1,d3			; set mantissa decimal exponent count
	move.l	d1,d4			; clear decimal exponent
	move.b	d1,fac1_s		; clear sign byte
	move.b	d1,dtypef		; set float data type
	move.b	d1,expneg		; clear exponent sign
	bsr	lab_gbyt		; get first byte back
	bcs.s	lab_28fe		; go get floating if 1st character numeric

	cmp.b	#'-,d0			; or is it -ve number
	bne.s	lab_289a		; branch if not

	move.b	#0xff,fac1_s		; set sign byte
	bra.s	lab_289c		; now go scan & check for hex/bin/int

lab_289a:
					; first character wasn't numeric or -
	cmp.b	#'+,d0			; compare with '+'
	bne.s	lab_289d		; branch if not '+' (go check for '.'/hex/bin/int)

lab_289c:
					; was "+" or "-" to start, so get next character
	bsr	lab_igby		; increment & scan memory
	bcs.s	lab_28fe		; branch if numeric character

lab_289d:
	cmp.b	#'.,d0			; else compare with '.'
	beq	lab_2904		; branch if '.'

					; code here for hex/binary/integer numbers
	cmp.b	#'$,d0			; compare with '$'
	beq	lab_chex		; branch if '$'

	cmp.b	#'%,d0			; else compare with '%'
	beq	lab_cbin		; branch if '%'

;*	cmp.b	#'&',d0			* else compare with '&'
;*	beq.s	lab_cint		* branch if '&' go do integer get ##

; ##	bra	lab_sner		* not #.$%& so do error
	bra	lab_2y01		; not #.$%& so return 0

lab_28fd:
	bsr	lab_igby		; get next character
	bcc.s	lab_2902		; exit loop if not a digit

lab_28fe:
	bsr	d1x10			; multiply d1 by 10 and add character
	bcc.s	lab_28fd		; loop for more if no overflow

lab_28ff:
					; overflowed mantissa, count 10s exponent
	addq.l	#1,d3			; increment mantissa decimal exponent count
	bsr	lab_igby		; get next character
	bcs.s	lab_28ff		; loop while numeric character

					; done overflow, now flush fraction or do E
	cmp.b	#'.,d0			; else compare with '.'
	bne.s	lab_2901		; branch if not '.'

lab_2900:
					; flush remaining fractional digits
	bsr	lab_igby		; get next character
	bcs	lab_2900		; loop while numeric character

lab_2901:
					; done number, only (possible) exponent remains
	cmp.b	#'E,d0			; else compare with 'E'
	bne.s	lab_2y01		; if not 'E' all done, go evaluate

					; process exponent
	bsr	lab_igby		; get next character
	bcs.s	lab_2x04		; branch if digit

	cmp.b	#'-,d0			; or is it -ve number
	beq.s	lab_2x01		; branch if so

	cmp.b	#tk_minus,d0		; or is it -ve number
	bne.s	lab_2x02		; branch if not

lab_2x01:
	move.b	#0xff,expneg		; set exponent sign
	bra.s	lab_2x03		; now go scan & check exponent

lab_2x02:
	cmp.b	#'+,d0			; or is it +ve number
	beq.s	lab_2x03		; branch if so

	cmp.b	#tk_plus,d0		; or is it +ve number
	bne	lab_sner		; wasn't - + TK_MINUS TK_PLUS or # so do error

lab_2x03:
	bsr	lab_igby		; get next character
	bcc.s	lab_2y01		; if not digit all done, go evaluate
lab_2x04:
	mulu	#10,d4			; multiply decimal exponent by 10
	and.l	#0xff,d0			; mask character
	sub.b	#'0,d0			; convert to value
	add.l	d0,d4			; add to decimal exponent
	cmp.b	#48,d4			; compare with decimal exponent limit+10
	ble.s	lab_2x03		; loop if no overflow/underflow

lab_2x05:
					; exponent value has overflowed
	bsr	lab_igby		; get next character
	bcs.s	lab_2x05		; loop while numeric digit

	bra.s	lab_2y01		; all done, go evaluate

lab_2902:
	cmp.b	#'.,d0			; else compare with '.'
	beq.s	lab_2904		; branch if was '.'

	bra.s	lab_2901		; branch if not '.' (go check/do 'E')

lab_2903:
	subq.l	#1,d3			; decrement mantissa decimal exponent
lab_2904:
					; was dp so get fractional part
	bsr	lab_igby		; get next character
	bcc.s	lab_2901		; exit loop if not a digit (go check/do 'E')

	bsr	d1x10			; multiply d1 by 10 and add character
	bcc.s	lab_2903		; loop for more if no overflow

	bra.s	lab_2900		; else go flush remaining fractional part

lab_2y01:
					; now evaluate result
	tst.b	expneg			; test exponent sign
	bpl.s	lab_2y02		; branch if sign positive

	neg.l	d4			; negate decimal exponent
lab_2y02:
	add.l	d3,d4			; add mantissa decimal exponent
	moveq	#32,d3			; set up max binary exponent
	tst.l	d1			; test mantissa
	beq.s	lab_rtn0		; if mantissa=0 return 0

	bmi.s	lab_2y04		; branch if already mormalised

	subq.l	#1,d3			; decrement bianry exponent for DBMI loop
lab_2y03:
	add.l	d1,d1			; shift mantissa
	dbmi	d3,lab_2y03		; decrement & loop if not normalised

					; ensure not too big or small
lab_2y04:
	cmp.l	#38,d4			; compare decimal exponent with max exponent
	bgt	lab_ofer		; if greater do overflow error and warm start

	cmp.l	#-38,d4			; compare decimal exponent with min exponent
	blt.s	lab_ret0		; if less just return zero

	neg.l	d4			; negate decimal exponent to go right way
	muls	#6,d4			; 6 bytes per entry
	move.l	a0,-(sp)		; save register
	lea	lab_p_10(pc),a0	; point to table
	move.b	1(a0,d4.w),fac2_e	; copy exponent for multiply
	move.l	2(a0,d4.w),fac2_m	; copy table mantissa
	move.l	(sp)+,a0		; restore register

	eori.b	#0x80,d3			; normalise input exponent
	move.l	d1,fac1_m		; save input mantissa
	move.b	d3,fac1_e		; save input exponent
	move.b	fac1_s,fac_sc		; set sign as sign compare

	movem.l	(sp)+,d1-d5		; restore registers
	bra	lab_multiply		; go multiply input by table

lab_ret0:
	moveq	#0,d1			; clear mantissa
lab_rtn0:
	move.l	d1,d3			; clear exponent
	move.b	d3,fac1_e		; save exponent
	move.l	d1,fac1_m		; save mantissa
	movem.l	(sp)+,d1-d5		; restore registers
	rts

; $ for hex add-on

; gets here if the first character was "$" for hex
; get hex number

lab_chex:
	move.b	#0x40,dtypef		; set integer numeric data type
	moveq	#32,d3			; set up max binary exponent
lab_chxx:
	bsr	lab_igby		; increment & scan memory
	bcs.s	lab_ishn		; branch if numeric character

	or.b	#0x20,d0			; case convert, allow "A" to "F" and "a" to "f"
	sub.b	#'a,d0			; subtract "a"
	bcs.s	lab_chx3		; exit if <"a"

	cmp.b	#0x06,d0			; compare normalised with $06 (max+1)
	bcc.s	lab_chx3		; exit if >"f"

	add.b	#0x3a,d0			; convert to nibble+"0"
lab_ishn:
	bsr.s	d1x16			; multiply d1 by 16 and add character
	bcc.s	lab_chxx		; loop for more if no overflow

					; overflowed mantissa, count 16s exponent
lab_chx1:
	addq.l	#4,d3			; increment mantissa exponent count
	bvs	lab_ofer		; do overflow error if overflowed

	bsr	lab_igby		; get next character
	bcs.s	lab_chx1		; loop while numeric character

	or.b	#0x20,d0			; case convert, allow "A" to "F" and "a" to "f"
	sub.b	#'a,d0			; subtract "a"
	bcs.s	lab_chx3		; exit if <"a"

	cmp.b	#0x06,d0			; compare normalised with $06 (max+1)
	bcs.s	lab_chx1		; loop if <="f"

					; now return value
lab_chx3:
	tst.l	d1			; test mantissa
	beq.s	lab_rtn0		; if mantissa=0 return 0

	bmi.s	lab_exxf		; branch if already mormalised

	subq.l	#1,d3			; decrement bianry exponent for DBMI loop
lab_chx2:
	add.l	d1,d1			; shift mantissa
	dbmi	d3,lab_chx2		; decrement & loop if not normalised

lab_exxf:
	eori.b	#0x80,d3			; normalise exponent
	move.b	d3,fac1_e		; save exponent
	move.l	d1,fac1_m		; save mantissa
	movem.l	(sp)+,d1-d5		; restore registers
rts_024:
	rts

; % for binary add-on

; gets here if the first character was  "%" for binary
; get binary number

lab_cbin:
	move.b	#0x40,dtypef		; set integer numeric data type
	moveq	#32,d3			; set up max binary exponent
lab_cbxn:
	bsr	lab_igby		; increment & scan memory
	bcc.s	lab_chx3		; if not numeric character go return value

	cmp.b	#'2,d0			; compare with "2" (max+1)
	bcc.s	lab_chx3		; if >="2" go return value

	move.l	d1,d2			; copy value
	bsr.s	d1x02			; multiply d1 by 2 and add character
	bcc.s	lab_cbxn		; loop for more if no overflow

					; overflowed mantissa, count 2s exponent
lab_cbx1:
	addq.l	#1,d3			; increment mantissa exponent count
	bvs	lab_ofer		; do overflow error if overflowed

	bsr	lab_igby		; get next character
	bcc.s	lab_chx3		; if not numeric character go return value

	cmp.b	#'2,d0			; compare with "2" (max+1)
	bcs.s	lab_cbx1		; loop if <"2"

	bra.s	lab_chx3		; if not numeric character go return value

; half way decent times 16 and times 2 with overflow checks

d1x16:
	move.l	d1,d2			; copy value
	add.l	d2,d2			; times two
	bcs.s	rts_024			; return if overflow

	add.l	d2,d2			; times four
	bcs.s	rts_024			; return if overflow

	add.l	d2,d2			; times eight
	bcs.s	rts_024			; return if overflow

d1x02:
	add.l	d2,d2			; times sixteen (ten/two)
	bcs.s	rts_024			; return if overflow

; now add in new digit

	and.l	#0xff,d0			; mask character
	sub.b	#'0,d0			; convert to value
	add.l	d0,d2			; add to result
	bcs.s	rts_024			; return if overflow (should never ever do this ##)

	move.l	d2,d1			; copy result
	rts

; half way decent times 10 with overflow checks

d1x10:
	move.l	d1,d2			; copy value
	add.l	d2,d2			; times two
	bcs.s	rts_025			; return if overflow

	add.l	d2,d2			; times four
	bcs.s	rts_025			; return if overflow

	add.l	d1,d2			; times five
	bcc.s	d1x02			; do times two and add in new digit if ok

rts_025:
	rts

;************************************************************************************

; token values needed for BASIC

tk_end		=	0x80
tk_for		=	tk_end+1	; $81 * FOR token
tk_next		=	tk_for+1	; $82
tk_data		=	tk_next+1	; $83 * DATA token
tk_input	=	tk_data+1	; $84
tk_dim		=	tk_input+1	; $85
tk_read		=	tk_dim+1	; $86
tk_let		=	tk_read+1	; $87
tk_dec		=	tk_let+1	; $88
tk_goto		=	tk_dec+1	; $89 * GOTO token
tk_run		=	tk_goto+1	; $8A
tk_if		=	tk_run+1	; $8B
tk_restore	=	tk_if+1		; $8C
tk_gosub	=	tk_restore+1	; $8D * GOSUB token
tk_return	=	tk_gosub+1	; $8E
tk_rem		=	tk_return+1	; $8F * REM token
tk_stop		=	tk_rem+1	; $90
tk_on		=	tk_stop+1	; $91 * ON token
tk_null		=	tk_on+1		; $92
tk_inc		=	tk_null+1	; $93
tk_wait		=	tk_inc+1	; $94
tk_load		=	tk_wait+1	; $95
tk_save		=	tk_load+1	; $96
tk_def		=	tk_save+1	; $97
tk_poke		=	tk_def+1	; $98
tk_doke		=	tk_poke+1	; $99
tk_loke		=	tk_doke+1	; $9A
tk_call		=	tk_loke+1	; $9B
tk_do		=	tk_call+1	; $9C * DO token
tk_loop		=	tk_do+1		; $9D
tk_print	=	tk_loop+1	; $9E * PRINT token
tk_cont		=	tk_print+1	; $9F
tk_list		=	tk_cont+1	; $A0
tk_clear	=	tk_list+1	; $A1 * CLEAR token
tk_new		=	tk_clear+1	; $A2
tk_width	=	tk_new+1	; $A3
tk_get		=	tk_width+1	; $A4
tk_swap		=	tk_get+1	; $A5
tk_bitset	=	tk_swap+1	; $A6
tk_bitclr	=	tk_bitset+1	; $A7
tk_tab		=	tk_bitclr+1	; $A8 * TAB token
tk_to		=	tk_tab+1	; $A9 * TO token
tk_fn		=	tk_to+1		; $AA * FN token
tk_spc		=	tk_fn+1		; $AB * SPC token
tk_then		=	tk_spc+1	; $AC * THEN token
tk_not		=	tk_then+1	; $AD * NOT token
tk_step		=	tk_not+1	; $AE * STEP token
tk_until	=	tk_step+1	; $AF * UNTIL token
tk_while	=	tk_until+1	; $B0
tk_plus		=	tk_while+1	; $B1 * + token
tk_minus	=	tk_plus+1	; $B2 * - token
tk_mult		=	tk_minus+1	; $B3
tk_div		=	tk_mult+1	; $B4
tk_power	=	tk_div+1	; $B5
tk_and		=	tk_power+1	; $B6
tk_eor		=	tk_and+1	; $B7
tk_or		=	tk_eor+1	; $B8
tk_rshift	=	tk_or+1		; $B9
tk_lshift	=	tk_rshift+1	; $BA
tk_gt		=	tk_lshift+1	; $BB * > token
tk_equal	=	tk_gt+1		; $BC * = token
tk_lt		=	tk_equal+1	; $BD * < token
tk_sgn		=	tk_lt+1		; $BE * SGN token
tk_int		=	tk_sgn+1	; $BF
tk_abs		=	tk_int+1	; $C0
tk_usr		=	tk_abs+1	; $C1
tk_fre		=	tk_usr+1	; $C2
tk_pos		=	tk_fre+1	; $C3
tk_sqr		=	tk_pos+1	; $C4
tk_rnd		=	tk_sqr+1	; $C5
tk_log		=	tk_rnd+1	; $C6
tk_exp		=	tk_log+1	; $C7
tk_cos		=	tk_exp+1	; $C8
tk_sin		=	tk_cos+1	; $C9
tk_tan		=	tk_sin+1	; $CA
tk_atn		=	tk_tan+1	; $CB
tk_peek		=	tk_atn+1	; $CC
tk_deek		=	tk_peek+1	; $CD
tk_leek		=	tk_deek+1	; $CE
tk_sadd		=	tk_leek+1	; $CF
tk_len		=	tk_sadd+1	; $D0
tk_strs		=	tk_len+1	; $D1
tk_val		=	tk_strs+1	; $D2
tk_asc		=	tk_val+1	; $D3
tk_ucases	=	tk_asc+1	; $D4
tk_lcases	=	tk_ucases+1	; $D5
tk_chrs		=	tk_lcases+1	; $D6 * CHR$ token
tk_hexs		=	tk_chrs+1	; $D7
tk_bins		=	tk_hexs+1	; $D8 * BIN$ token
tk_bittst	=	tk_bins+1	; $D9
tk_max		=	tk_bittst+1	; $DA
tk_min		=	tk_max+1	; $DB
tk_pi		=	tk_min+1	; $DC
tk_twopi	=	tk_pi+1		; $DD
tk_vptr		=	tk_twopi+1	; $DE * VARPTR token
tk_lefts	=	tk_vptr+1	; $DF
tk_rights	=	tk_lefts+1	; $E0
tk_mids		=	tk_rights+1	; $E1

;***********************************************************************************

; binary to unsigned decimal table

bin2dec:
	.long	0x3b9aca00		; 1000000000
	.long	0x05f5e100		; 100000000
	.long	0x00989680		; 10000000
	.long	0x000f4240		; 1000000
	.long	0x000186a0		; 100000
	.long	0x00002710		; 10000
	.long	0x000003e8		; 1000
	.long	0x00000064		; 100
	.long	0x0000000a		; 10
lab_1d96:
	.long	0x00000000		; 0 end marker

lab_rsed:
	.long	0x312e3130		; 825110832

	.word	255			; 10**38
	.long	0x96769951
	.word	251			; 10**37
	.long	0xf0bdc21b
	.word	248			; 10**36
	.long	0xc097ce7c
	.word	245			; 10**35
	.long	0x9a130b96
	.word	241			; 10**34
	.long	0xf684df57
	.word	238			; 10**33
	.long	0xc5371912
	.word	235			; 10**32
	.long	0x9dc5ada8
	.word	231			; 10**31
	.long	0xfc6f7c40
	.word	228			; 10**30
	.long	0xc9f2c9cd
	.word	225			; 10**29
	.long	0xa18f07d7
	.word	222			; 10**28
	.long	0x813f3979
	.word	218			; 10**27
	.long	0xcecb8f28
	.word	215			; 10**26
	.long	0xa56fa5ba
	.word	212			; 10**25
	.long	0x84595161
	.word	208			; 10**24
	.long	0xd3c21bcf
	.word	205			; 10**23
	.long	0xa968163f
	.word	202			; 10**22
	.long	0x87867832
	.word	198			; 10**21
	.long	0xd8d726b7
	.word	195			; 10**20
	.long	0xad78ebc6
	.word	192			; 10**19
	.long	0x8ac72305
	.word	188			; 10**18
	.long	0xde0b6b3a
	.word	185			; 10**17
	.long	0xb1a2bc2f
	.word	182			; 10**16
	.long	0x8e1bc9bf
	.word	178			; 10**15
	.long	0xe35fa932
	.word	175			; 10**14
	.long	0xb5e620f5
	.word	172			; 10**13
	.long	0x9184e72a
	.word	168			; 10**12
	.long	0xe8d4a510
	.word	165			; 10**11
	.long	0xba43b740
	.word	162			; 10**10
	.long	0x9502f900
	.word	158			; 10**9
	.long	0xee6b2800
	.word	155			; 10**8
	.long	0xbebc2000
	.word	152			; 10**7
	.long	0x98968000
	.word	148			; 10**6
	.long	0xf4240000
	.word	145			; 10**5
	.long	0xc3500000
	.word	142			; 10**4
	.long	0x9c400000
	.word	138			; 10**3
	.long	0xfa000000
	.word	135			; 10**2
	.long	0xc8000000
	.word	132			; 10**1
	.long	0xa0000000
lab_p_10:
	.word	129			; 10**0
	.long	0x80000000
	.word	125			; 10**-1
	.long	0xcccccccd
	.word	122			; 10**-2
	.long	0xa3d70a3d
	.word	119			; 10**-3
	.long	0x83126e98
	.word	115			; 10**-4
	.long	0xd1b71759
	.word	112			; 10**-5
	.long	0xa7c5ac47
	.word	109			; 10**-6
	.long	0x8637bd06
	.word	105			; 10**-7
	.long	0xd6bf94d6
	.word	102			; 10**-8
	.long	0xabcc7712
	.word	99			; 10**-9
	.long	0x89705f41
	.word	95			; 10**-10
	.long	0xdbe6fecf
	.word	92			; 10**-11
	.long	0xafebff0c
	.word	89			; 10**-12
	.long	0x8cbccc09
	.word	85			; 10**-13
	.long	0xe12e1342
	.word	82			; 10**-14
	.long	0xb424dc35
	.word	79			; 10**-15
	.long	0x901d7cf7
	.word	75			; 10**-16
	.long	0xe69594bf
	.word	72			; 10**-17
	.long	0xb877aa32
	.word	69			; 10**-18
	.long	0x9392ee8f
	.word	65			; 10**-19
	.long	0xec1e4a7e
	.word	62			; 10**-20
	.long	0xbce50865
	.word	59			; 10**-21
	.long	0x971da050
	.word	55			; 10**-22
	.long	0xf1c90081
	.word	52			; 10**-23
	.long	0xc16d9a01
	.word	49			; 10**-24
	.long	0x9abe14cd
	.word	45			; 10**-25
	.long	0xf79687ae
	.word	42			; 10**-26
	.long	0xc6120625
	.word	39			; 10**-27
	.long	0x9e74d1b8
	.word	35			; 10**-28
	.long	0xfd87b5f3
	.word	32			; 10**-29
	.long	0xcad2f7f5
	.word	29			; 10**-30
	.long	0xa2425ff7
	.word	26			; 10**-31
	.long	0x81ceb32c
	.word	22			; 10**-32
	.long	0xcfb11ead
	.word	19			; 10**-33
	.long	0xa6274bbe
	.word	16			; 10**-34
	.long	0x84ec3c98
	.word	12			; 10**-35
	.long	0xd4ad2dc0
	.word	9			; 10**-36
	.long	0xaa242499
	.word	6			; 10**-37
	.long	0x881cea14
	.word	2			; 10**-38
	.long	0xd9c7dced

; table of constants for cordic SIN/COS/TAN calculations
; constants are un normalised fractions and are atn(2^-i)/2pi

	.long	0x4dba76d4		; SIN/COS multiply constant
tab_snco:
	.long	0x20000000		; atn(2^0)/2pi
	.long	0x12e4051e		; atn(2^1)/2pi
	.long	0x09fb385c		; atn(2^2)/2pi
	.long	0x051111d5		; atn(2^3)/2pi
	.long	0x028b0d44		; atn(2^4)/2pi
	.long	0x0145d7e2		; atn(2^5)/2pi
	.long	0x00a2f61f		; atn(2^6)/2pi
	.long	0x00517c56		; atn(2^7)/2pi
	.long	0x0028be54		; atn(2^8)/2pi
	.long	0x00145f2f		; atn(2^9)/2pi
	.long	0x000a2f99		; atn(2^10)/2pi
	.long	0x000517cd		; atn(2^11)/2pi
	.long	0x00028be7		; atn(2^12)/2pi
	.long	0x000145f4		; atn(2^13)/2pi
	.long	0x0000a2fa		; atn(2^14)/2pi
	.long	0x0000517d		; atn(2^15)/2pi
	.long	0x000028bf		; atn(2^16)/2pi
	.long	0x00001460		; atn(2^17)/2pi
	.long	0x00000a30		; atn(2^18)/2pi
	.long	0x00000518		; atn(2^19)/2pi
	.long	0x0000028c		; atn(2^20)/2pi
	.long	0x00000146		; atn(2^21)/2pi
	.long	0x000000a3		; atn(2^22)/2pi
	.long	0x00000052		; atn(2^23)/2pi
	.long	0x00000029		; atn(2^24)/2pi
	.long	0x00000015		; atn(2^25)/2pi
	.long	0x0000000b		; atn(2^26)/2pi
	.long	0x00000006		; atn(2^27)/2pi
	.long	0x00000003		; atn(2^28)/2pi
	.long	0x00000002		; atn(2^29)/2pi
	.long	0x00000001		; atn(2^30)/2pi
	.long	0x00000001		; atn(2^31)/2pi

; table of constants for cordic ATN calculation
; constants are normalised to two integer bits and are atn(2^-i)

;	.long	0x3243f6a9		* atn(2^0) (not used)
tab_atnc:
	.long	0x1dac6705		; atn(2^-1)
	.long	0x0fadbafd		; atn(2^-2)
	.long	0x07f56ea7		; atn(2^-3)
	.long	0x03feab77		; atn(2^-4)
	.long	0x01ffd55c		; atn(2^-5)
	.long	0x00fffaab		; atn(2^-6)
	.long	0x007fff55		; atn(2^-7)
	.long	0x003fffeb		; atn(2^-8)
	.long	0x001ffffd		; atn(2^-9)
	.long	0x00100000		; atn(2^-10)
	.long	0x00080000		; atn(2^-11)
	.long	0x00040000		; atn(2^-12)
	.long	0x00020000		; atn(2^-13)
	.long	0x00010000		; atn(2^-14)
	.long	0x00008000		; atn(2^-15)
	.long	0x00004000		; atn(2^-16)
	.long	0x00002000		; atn(2^-17)
	.long	0x00001000		; atn(2^-18)
	.long	0x00000800		; atn(2^-19)
	.long	0x00000400		; atn(2^-20)
	.long	0x00000200		; atn(2^-21)
	.long	0x00000100		; atn(2^-22)
	.long	0x00000080		; atn(2^-23)
	.long	0x00000040		; atn(2^-24)
	.long	0x00000020		; atn(2^-25)
	.long	0x00000010		; atn(2^-26)
	.long	0x00000008		; atn(2^-27)
	.long	0x00000004		; atn(2^-28)
	.long	0x00000002		; atn(2^-29)
	.long	0x00000001		; atn(2^-30)
	.long	0x00000000		; atn(2^-31)
	.long	0x00000000		; atn(2^-32)

; constants are normalised to n integer bits and are tanh(2^-i)
n	=	2
tab_hthet:
	.long	0x8c9f53d0>>n		; atnh(2^-1)   .549306144
	.long	0x4162bbe8>>n		; atnh(2^-2)   .255412812
	.long	0x202b1238>>n		; atnh(2^-3)
	.long	0x10055888>>n		; atnh(2^-4)
	.long	0x0800aac0>>n		; atnh(2^-5)
	.long	0x04001550>>n		; atnh(2^-6)
	.long	0x020002a8>>n		; atnh(2^-7)
	.long	0x01000050>>n		; atnh(2^-8)
	.long	0x00800008>>n		; atnh(2^-9)
	.long	0x00400000>>n		; atnh(2^-10)
	.long	0x00200000>>n		; atnh(2^-11)
	.long	0x00100000>>n		; atnh(2^-12)
	.long	0x00080000>>n		; atnh(2^-13)
	.long	0x00040000>>n		; atnh(2^-14)
	.long	0x00020000>>n		; atnh(2^-15)
	.long	0x00010000>>n		; atnh(2^-16)
	.long	0x00008000>>n		; atnh(2^-17)
	.long	0x00004000>>n		; atnh(2^-18)
	.long	0x00002000>>n		; atnh(2^-19)
	.long	0x00001000>>n		; atnh(2^-20)
	.long	0x00000800>>n		; atnh(2^-21)
	.long	0x00000400>>n		; atnh(2^-22)
	.long	0x00000200>>n		; atnh(2^-23)
	.long	0x00000100>>n		; atnh(2^-24)
	.long	0x00000080>>n		; atnh(2^-25)
	.long	0x00000040>>n		; atnh(2^-26)
	.long	0x00000020>>n		; atnh(2^-27)
	.long	0x00000010>>n		; atnh(2^-28)
	.long	0x00000008>>n		; atnh(2^-29)
	.long	0x00000004>>n		; atnh(2^-30)
	.long	0x00000002>>n		; atnh(2^-31)
	.long	0x00000001>>n		; atnh(2^-32)

kfctseed =	0x9a8f4441>>n		; $26A3D110

; command vector table

lab_ctbl:
	.long	lab_end			; END
	.long	lab_for			; FOR
	.long	lab_next		; NEXT
	.long	lab_data		; DATA
	.long	lab_input		; INPUT
	.long	lab_dim			; DIM
	.long	lab_read		; READ
	.long	lab_let			; LET
	.long	lab_dec			; DEC
	.long	lab_goto		; GOTO
	.long	lab_run			; RUN
	.long	lab_if			; IF
	.long	lab_restore		; RESTORE
	.long	lab_gosub		; GOSUB
	.long	lab_return		; RETURN
	.long	lab_rem			; REM
	.long	lab_stop		; STOP
	.long	lab_on			; ON
	.long	lab_null		; NULL
	.long	lab_inc			; INC
	.long	lab_wait		; WAIT
	.long	v_load			; LOAD
	.long	v_save			; SAVE
	.long	lab_def			; DEF
	.long	lab_poke		; POKE
	.long	lab_doke		; DOKE
	.long	lab_loke		; LOKE
	.long	lab_call		; CALL
	.long	lab_do			; DO
	.long	lab_loop		; LOOP
	.long	lab_print		; PRINT
	.long	lab_cont		; CONT
	.long	lab_list		; LIST
	.long	lab_clear		; CLEAR
	.long	lab_new			; NEW
	.long	lab_wdth		; WIDTH
	.long	lab_get			; GET
	.long	lab_swap		; SWAP
	.long	lab_bitset		; BITSET
	.long	lab_bitclr		; BITCLR

; action addresses for functions

lab_ftxx:
lab_ftbl =	lab_ftxx-(tk_sgn-0x80)*4	; offset for table start

	.long	lab_sgn			; SGN()
	.long	lab_int			; INT()
	.long	lab_abs			; ABS()
	.long	usrjmp			; USR()
	.long	lab_fre			; FRE()
	.long	lab_pos			; POS()
	.long	lab_sqr			; SQR()
	.long	lab_rnd			; RND()
	.long	lab_log			; LOG()
	.long	lab_exp			; EXP()
	.long	lab_cos			; COS()
	.long	lab_sin			; SIN()
	.long	lab_tan			; TAN()
	.long	lab_atn			; ATN()
	.long	lab_peek		; PEEK()
	.long	lab_deek		; DEEK()
	.long	lab_leek		; LEEK()
	.long	lab_sadd		; SADD()
	.long	lab_lens		; LEN()
	.long	lab_strs		; STR$()
	.long	lab_val			; VAL()
	.long	lab_asc			; ASC()
	.long	lab_ucase		; UCASE$()
	.long	lab_lcase		; LCASE$()
	.long	lab_chrs		; CHR$()
	.long	lab_hexs		; HEX$()
	.long	lab_bins		; BIN$()
	.long	lab_btst		; BITTST()
	.long	lab_max			; MAX()
	.long	lab_min			; MIN()
	.long	lab_pi			; PI
	.long	lab_twopi		; TWOPI
	.long	lab_varptr		; VARPTR()
	.long	lab_left		; LEFT$()
	.long	lab_right		; RIGHT$()
	.long	lab_mids		; MID$()

; hierarchy and action addresses for operator

lab_oppt:
	.word	0x0079			; +
	.long	lab_add
	.word	0x0079			; -
	.long	lab_subtract
	.word	0x007b			; *
	.long	lab_multiply
	.word	0x007b			; /
	.long	lab_divide
	.word	0x007f			; ^
	.long	lab_power
	.word	0x0050			; AND
	.long	lab_and
	.word	0x0046			; EOR
	.long	lab_eor
	.word	0x0046			; OR
	.long	lab_or
	.word	0x0056			; >>
	.long	lab_rshift
	.word	0x0056			; <<
	.long	lab_lshift
	.word	0x007d			; >
	.long	lab_gthan		; used to evaluate -n
	.word	0x005a			; =
	.long	lab_equal		; used to evaluate NOT
	.word	0x0064			; <
	.long	lab_lthan

					; numeric PRINT constants
lab_2947:
	.long	0x91434ff8		; 99999.9375 (max value with at least one decimal)
lab_294b:
	.long	0x947423f7		; 999999.4375 (max value before sci notation)
					; misc constants

lab_259c:
	.long	0x81000000		; 1.000000, used for ATN
lab_2a96:
	.long	0x80000000		; 0.5, used for float rounding

; This table is used in converting numbers to ASCII.
; first four entries for expansion to 9.25 digits

;	.long	0xc4653600		* -1000000000
;	.long	0x05f5e100		* 100000000
;	.long	0xff676980		* -10000000
;	.long	0x000f4240		* 1000000
lab_2a9a:
	.long	0xfffe7960		; -100000
	.long	0x00002710		; 10000
	.long	0xfffffc18		; -1000
	.long	0x00000064		; 100
	.long	0xfffffff6		; -10
	.long	0x00000001		; 1

; new keyword tables

; offsets to keyword tables

tab_chrt:
	.word	tab_star-tab_star	; "*" $2A
	.word	tab_plus-tab_star	; "+" $2B
	.word	-1			; "," $2C no keywords
	.word	tab_mnus-tab_star	; "-" $2D
	.word	-1			; "." $2E no keywords
	.word	tab_slas-tab_star	; "/" $2F
	.word	-1			; "0" $30 no keywords
	.word	-1			; "1" $31 no keywords
	.word	-1			; "2" $32 no keywords
	.word	-1			; "3" $33 no keywords
	.word	-1			; "4" $34 no keywords
	.word	-1			; "5" $35 no keywords
	.word	-1			; "6" $36 no keywords
	.word	-1			; "7" $37 no keywords
	.word	-1			; "8" $38 no keywords
	.word	-1			; "9" $39 no keywords
	.word	-1			; ";" $3A no keywords
	.word	-1			; ":" $3B no keywords
	.word	tab_less-tab_star	; "<" $3C
	.word	tab_equl-tab_star	; "=" $3D
	.word	tab_more-tab_star	; ">" $3E
	.word	tab_qest-tab_star	; "?" $3F
	.word	-1			; "@" $40 no keywords
	.word	tab_asca-tab_star	; "A" $41
	.word	tab_ascb-tab_star	; "B" $42
	.word	tab_ascc-tab_star	; "C" $43
	.word	tab_ascd-tab_star	; "D" $44
	.word	tab_asce-tab_star	; "E" $45
	.word	tab_ascf-tab_star	; "F" $46
	.word	tab_ascg-tab_star	; "G" $47
	.word	tab_asch-tab_star	; "H" $48
	.word	tab_asci-tab_star	; "I" $49
	.word	-1			; "J" $4A no keywords
	.word	-1			; "K" $4B no keywords
	.word	tab_ascl-tab_star	; "L" $4C
	.word	tab_ascm-tab_star	; "M" $4D
	.word	tab_ascn-tab_star	; "N" $4E
	.word	tab_asco-tab_star	; "O" $4F
	.word	tab_ascp-tab_star	; "P" $50
	.word	-1			; "Q" $51 no keywords
	.word	tab_ascr-tab_star	; "R" $52
	.word	tab_ascs-tab_star	; "S" $53
	.word	tab_asct-tab_star	; "T" $54
	.word	tab_ascu-tab_star	; "U" $55
	.word	tab_ascv-tab_star	; "V" $56
	.word	tab_ascw-tab_star	; "W" $57
	.word	-1			; "X" $58 no keywords
	.word	-1			; "Y" $59 no keywords
	.word	-1			; "Z" $5A no keywords
	.word	-1			; "[" $5B no keywords
	.word	-1			; "\" $5C no keywords
	.word	-1			; "]" $5D no keywords
	.word	tab_powr-tab_star	; "^" $5E

; Table of Basic keywords for LIST command
; [byte]first character,[byte]remaining length -1
; [word]offset from table start

lab_keyt:
	.byte	'E,1
	.word	key_end-tab_star	; END
	.byte	'F,1
	.word	key_for-tab_star	; FOR
	.byte	'N,2
	.word	key_next-tab_star	; NEXT
	.byte	'D,2
	.word	key_data-tab_star	; DATA
	.byte	'I,3
	.word	key_input-tab_star	; INPUT
	.byte	'D,1
	.word	key_dim-tab_star	; DIM
	.byte	'R,2
	.word	key_read-tab_star	; READ
	.byte	'L,1
	.word	key_let-tab_star	; LET
	.byte	'D,1
	.word	key_dec-tab_star	; DEC
	.byte	'G,2
	.word	key_goto-tab_star	; GOTO
	.byte	'R,1
	.word	key_run-tab_star	; RUN
	.byte	'I,0
	.word	key_if-tab_star		; IF
	.byte	'R,5
	.word	key_restore-tab_star	; RESTORE
	.byte	'G,3
	.word	key_gosub-tab_star	; GOSUB
	.byte	'R,4
	.word	key_return-tab_star	; RETURN
	.byte	'R,1
	.word	key_rem-tab_star	; REM
	.byte	'S,2
	.word	key_stop-tab_star	; STOP
	.byte	'O,0
	.word	key_on-tab_star		; ON
	.byte	'N,2
	.word	key_null-tab_star	; NULL
	.byte	'I,1
	.word	key_inc-tab_star	; INC
	.byte	'W,2
	.word	key_wait-tab_star	; WAIT
	.byte	'L,2
	.word	key_load-tab_star	; LOAD
	.byte	'S,2
	.word	key_save-tab_star	; SAVE
	.byte	'D,1
	.word	key_def-tab_star	; DEF
	.byte	'P,2
	.word	key_poke-tab_star	; POKE
	.byte	'D,2
	.word	key_doke-tab_star	; DOKE
	.byte	'L,2
	.word	key_loke-tab_star	; LOKE
	.byte	'C,2
	.word	key_call-tab_star	; CALL
	.byte	'D,0
	.word	key_do-tab_star		; DO
	.byte	'L,2
	.word	key_loop-tab_star	; LOOP
	.byte	'P,3
	.word	key_print-tab_star	; PRINT
	.byte	'C,2
	.word	key_cont-tab_star	; CONT
	.byte	'L,2
	.word	key_list-tab_star	; LIST
	.byte	'C,3
	.word	key_clear-tab_star	; CLEAR
	.byte	'N,1
	.word	key_new-tab_star	; NEW
	.byte	'W,3
	.word	key_width-tab_star	; WIDTH
	.byte	'G,1
	.word	key_get-tab_star	; GET
	.byte	'S,3
	.word	key_swap-tab_star	; SWAP
	.byte	'B,4
	.word	key_bitset-tab_star	; BITSET
	.byte	'B,4
	.word	key_bitclr-tab_star	; BITCLR
	.byte	'T,2
	.word	key_tab-tab_star	; TAB(

	.byte	'T,0
	.word	key_to-tab_star		; TO
	.byte	'F,0
	.word	key_fn-tab_star		; FN
	.byte	'S,2
	.word	key_spc-tab_star	; SPC(
	.byte	'T,2
	.word	key_then-tab_star	; THEN
	.byte	'N,1
	.word	key_not-tab_star	; NOT
	.byte	'S,2
	.word	key_step-tab_star	; STEP
	.byte	'U,3
	.word	key_until-tab_star	; UNTIL
	.byte	'W,3
	.word	key_while-tab_star	; WHILE

	.byte	'+,-1
	.word	key_plus-tab_star	; +
	.byte	'-,-1
	.word	key_minus-tab_star	; -
	.byte	'*,-1
	.word	key_mult-tab_star	; *
	.byte	'/,-1
	.word	key_div-tab_star	; /
	.byte	'^,-1
	.word	key_power-tab_star	; ^
	.byte	'A,1
	.word	key_and-tab_star	; AND
	.byte	'E,1
	.word	key_eor-tab_star	; EOR
	.byte	'O,0
	.word	key_or-tab_star		; OR
	.byte	'>,0
	.word	key_rshift-tab_star	; >>
	.byte	'<,0
	.word	key_lshift-tab_star	; <<
	.byte	'>,-1
	.word	key_gt-tab_star		; >
	.byte	'=,-1
	.word	key_equal-tab_star	; =
	.byte	'<,-1
	.word	key_lt-tab_star		; <

	.byte	'S,1
	.word	key_sgn-tab_star	; SGN
	.byte	'I,1
	.word	key_int-tab_star	; INT
	.byte	'A,1
	.word	key_abs-tab_star	; ABS
	.byte	'U,1
	.word	key_usr-tab_star	; USR
	.byte	'F,1
	.word	key_fre-tab_star	; FRE
	.byte	'P,1
	.word	key_pos-tab_star	; POS
	.byte	'S,1
	.word	key_sqr-tab_star	; SQR
	.byte	'R,1
	.word	key_rnd-tab_star	; RND
	.byte	'L,1
	.word	key_log-tab_star	; LOG
	.byte	'E,1
	.word	key_exp-tab_star	; EXP
	.byte	'C,1
	.word	key_cos-tab_star	; COS
	.byte	'S,1
	.word	key_sin-tab_star	; SIN
	.byte	'T,1
	.word	key_tan-tab_star	; TAN
	.byte	'A,1
	.word	key_atn-tab_star	; ATN
	.byte	'P,2
	.word	key_peek-tab_star	; PEEK
	.byte	'D,2
	.word	key_deek-tab_star	; DEEK
	.byte	'L,2
	.word	key_leek-tab_star	; LEEK
	.byte	'S,2
	.word	key_sadd-tab_star	; SADD
	.byte	'L,1
	.word	key_len-tab_star	; LEN
	.byte	'S,2
	.word	key_strs-tab_star	; STR$
	.byte	'V,1
	.word	key_val-tab_star	; VAL
	.byte	'A,1
	.word	key_asc-tab_star	; ASC
	.byte	'U,4
	.word	key_ucases-tab_star	; UCASE$
	.byte	'L,4
	.word	key_lcases-tab_star	; LCASE$
	.byte	'C,2
	.word	key_chrs-tab_star	; CHR$
	.byte	'H,2
	.word	key_hexs-tab_star	; HEX$
	.byte	'B,2
	.word	key_bins-tab_star	; BIN$
	.byte	'B,4
	.word	key_bittst-tab_star	; BITTST
	.byte	'M,1
	.word	key_max-tab_star	; MAX
	.byte	'M,1
	.word	key_min-tab_star	; MIN
	.byte	'P,0
	.word	key_pi-tab_star		; PI
	.byte	'T,3
	.word	key_twopi-tab_star	; TWOPI
	.byte	'V,4
	.word	key_vptr-tab_star	; VARPTR
	.byte	'L,3
	.word	key_lefts-tab_star	; LEFT$
	.byte	'R,4
	.word	key_rights-tab_star	; RIGHT$
	.byte	'M,2
	.word	key_mids-tab_star	; MID$

; BASIC error messages

lab_baer:
	.long	lab_nf			; $00 NEXT without FOR
	.long	lab_sn			; $04 syntax
	.long	lab_rg			; $08 RETURN without GOSUB
	.long	lab_od			; $0C out of data
	.long	lab_fc			; $10 function call
	.long	lab_ov			; $14 overflow
	.long	lab_om			; $18 out of memory
	.long	lab_us			; $1C undefined statement
	.long	lab_bs			; $20 array bounds
	.long	lab_dd			; $24 double dimension array
	.long	lab_d0			; $28 divide by 0
	.long	lab_id			; $2C illegal direct
	.long	lab_tm			; $30 type mismatch
	.long	lab_ls			; $34 long string
	.long	lab_st			; $38 string too complex
	.long	lab_cn			; $3C continue error
	.long	lab_uf			; $40 undefined function
	.long	lab_ld			; $44 LOOP without DO
	.long	lab_uv			; $48 undefined variable
	.long	lab_ua			; $4C undimensioned array
	.long	lab_wd			; $50 wrong dimensions
	.long	lab_ad			; $54 address
;	.long	lab_it			* $58 internal

lab_nf:	.byte	'N,'E,'X,'T,' ,'w,'i,'t,'h,'o,'u,'t,' ,'F,'O,'R,0x00
lab_sn:	.byte	'S,'y,'n,'t,'a,'x,0x00
lab_rg:	.byte	'R,'E,'T,'U,'R,'N,' ,'w,'i,'t,'h,'o,'u,'t,' ,'G,'O,'S,'U,'B,0x00
lab_od:	.byte	'O,'u,'t,' ,'o,'f,' ,'D,'A,'T,'A,0x00
lab_fc:	.byte	'F,'u,'n,'c,'t,'i,'o,'n,' ,'c,'a,'l,'l,0x00
lab_ov:	.byte	'O,'v,'e,'r,'f,'l,'o,'w,0x00
lab_om:	.byte	'O,'u,'t,' ,'o,'f,' ,'m,'e,'m,'o,'r,'y,0x00
lab_us:	.byte	'U,'n,'d,'e,'f,'i,'n,'e,'d,' ,'s,'t,'a,'t,'e,'m,'e,'n,'t,0x00
lab_bs:	.byte	'A,'r,'r,'a,'y,' ,'b,'o,'u,'n,'d,'s,0x00
lab_dd:	.byte	'D,'o,'u,'b,'l,'e,' ,'d,'i,'m,'e,'n,'s,'i,'o,'n,0x00
lab_d0:	.byte	'D,'i,'v,'i,'d,'e,' ,'b,'y,' ,'z,'e,'r,'o,0x00
lab_id:	.byte	'I,'l,'l,'e,'g,'a,'l,' ,'d,'i,'r,'e,'c,'t,0x00
lab_tm:	.byte	'T,'y,'p,'e,' ,'m,'i,'s,'m,'a,'t,'c,'h,0x00
lab_ls:	.byte	'S,'t,'r,'i,'n,'g,' ,'t,'o,'o,' ,'l,'o,'n,'g,0x00
lab_st:	.byte	'S,'t,'r,'i,'n,'g,' ,'t,'o,'o,' ,'c,'o,'m,'p,'l,'e,'x,0x00
lab_cn:	.byte	'C,'a,'n,'','t,' ,'c,'o,'n,'t,'i,'n,'u,'e,0x00
lab_uf:	.byte	'U,'n,'d,'e,'f,'i,'n,'e,'d,' ,'f,'u,'n,'c,'t,'i,'o,'n,0x00
lab_ld:	.byte	'L,'O,'O,'P,' ,'w,'i,'t,'h,'o,'u,'t,' ,'D,'O,0x00
lab_uv:	.byte	'U,'n,'d,'e,'f,'i,'n,'e,'d,' ,'v,'a,'r,'i,'a,'b,'l,'e,0x00
lab_ua:	.byte	'U,'n,'d,'i,'m,'e,'n,'s,'i,'o,'n,'e,'d,' ,'a,'r,'r,'a,'y,0x00
lab_wd:	.byte	'W,'r,'o,'n,'g,' ,'d,'i,'m,'e,'n,'s,'i,'o,'n,'s,0x00
lab_ad:	.byte	'A,'d,'d,'r,'e,'s,'s,0x00
;LAB_IT	.byte	'Internal',$00

; keyword table for line (un)crunching

; [keyword,token
; [keyword,token]]
; end marker (#$00)

tab_star:
key_mult:
	.byte	tk_mult,0x00		; *
tab_plus:
key_plus:
	.byte	tk_plus,0x00		; +
tab_mnus:
key_minus:
	.byte	tk_minus,0x00		; -
tab_slas:
key_div:
	.byte	tk_div,0x00		; /
tab_less:
key_lshift:
	.byte	'<,tk_lshift		; <<
key_lt:
	.byte	tk_lt			; <
	.byte	0x00
tab_equl:
key_equal:
	.byte	tk_equal,0x00		; =
tab_more:
key_rshift:
	.byte	'>,tk_rshift		; >>
key_gt:
	.byte	tk_gt			; >
	.byte	0x00
tab_qest:
	.byte	tk_print,0x00		; ?
tab_asca:
key_abs:
	.byte	'B,'S,tk_abs		; ABS
key_and:
	.byte	'N,'D,tk_and		; AND
key_asc:
	.byte	'S,'C,tk_asc		; ASC
key_atn:
	.byte	'T,'N,tk_atn		; ATN
	.byte	0x00
tab_ascb:
key_bins:
	.byte	'I,'N,'$,tk_bins	; BIN$IN$
key_bitclr:
	.byte	'I,'T,'C,'L,'R,tk_bitclr ; BITCLR
key_bitset:
	.byte	'I,'T,'S,'E,'T,tk_bitset ; BITSET
key_bittst:
	.byte	'I,'T,'T,'S,'T,tk_bittst ; BITTST
	.byte	0x00
tab_ascc:
key_call:
	.byte	'A,'L,'L,tk_call	; CALL
key_chrs:
	.byte	'H,'R,'$,tk_chrs	; CHR$
key_clear:
	.byte	'L,'E,'A,'R,tk_clear	; CLEAR
key_cont:
	.byte	'O,'N,'T,tk_cont	; CONT
key_cos:
	.byte	'O,'S,tk_cos		; COS
	.byte	0x00
tab_ascd:
key_data:
	.byte	'A,'T,'A,tk_data	; DATA
key_dec:
	.byte	'E,'C,tk_dec		; DEC
key_deek:
	.byte	'E,'E,'K,tk_deek	; DEEK
key_def:
	.byte	'E,'F,tk_def		; DEF
key_dim:
	.byte	'I,'M,tk_dim		; DIM
key_doke:
	.byte	'O,'K,'E,tk_doke	; DOKE
key_do:
	.byte	'O,tk_do		; DO
	.byte	0x00
tab_asce:
key_end:
	.byte	'N,'D,tk_end		; END
key_eor:
	.byte	'O,'R,tk_eor		; EOR
key_exp:
	.byte	'X,'P,tk_exp		; EXP
	.byte	0x00
tab_ascf:
key_for:
	.byte	'O,'R,tk_for		; FOR
key_fn:
	.byte	'N,tk_fn		; FN
key_fre:
	.byte	'R,'E,tk_fre		; FRE
	.byte	0x00
tab_ascg:
key_get:
	.byte	'E,'T,tk_get		; GET
key_goto:
	.byte	'O,'T,'O,tk_goto	; GOTO
key_gosub:
	.byte	'O,'S,'U,'B,tk_gosub	; GOSUB
	.byte	0x00
tab_asch:
key_hexs:
	.byte	'E,'X,'$,tk_hexs,0x00	; HEX$
tab_asci:
key_if:
	.byte	'F,tk_if		; IF
key_inc:
	.byte	'N,'C,tk_inc		; INC
key_input:
	.byte	'N,'P,'U,'T,tk_input	; INPUT
key_int:
	.byte	'N,'T,tk_int		; INT
	.byte	0x00
tab_ascl:
key_lcases:
	.byte	'C,'A,'S,'E,'$,tk_lcases ; LCASE$
key_leek:
	.byte	'E,'E,'K,tk_leek	; LEEK
key_lefts:
	.byte	'E,'F,'T,'$,tk_lefts	; LEFT$
key_len:
	.byte	'E,'N,tk_len		; LEN
key_let:
	.byte	'E,'T,tk_let		; LET
key_list:
	.byte	'I,'S,'T,tk_list	; LIST
key_load:
	.byte	'O,'A,'D,tk_load	; LOAD
key_log:
	.byte	'O,'G,tk_log		; LOG
key_loke:
	.byte	'O,'K,'E,tk_loke	; LOKE
key_loop:
	.byte	'O,'O,'P,tk_loop	; LOOP
	.byte	0x00
tab_ascm:
key_max:
	.byte	'A,'X,tk_max		; MAX
key_mids:
	.byte	'I,'D,'$,tk_mids	; MID$
key_min:
	.byte	'I,'N,tk_min		; MIN
	.byte	0x00
tab_ascn:
key_new:
	.byte	'E,'W,tk_new		; NEW
key_next:
	.byte	'E,'X,'T,tk_next	; NEXT
key_not:
	.byte	'O,'T,tk_not		; NUT
key_null:
	.byte	'U,'L,'L,tk_null	; NULL
	.byte	0x00
tab_asco:
key_on:
	.byte	'N,tk_on		; ON
key_or:
	.byte	'R,tk_or		; OR
	.byte	0x00
tab_ascp:
key_peek:
	.byte	'E,'E,'K,tk_peek	; PEEK
key_pi:
	.byte	'I,tk_pi		; PI
key_poke:
	.byte	'O,'K,'E,tk_poke	; POKE
key_pos:
	.byte	'O,'S,tk_pos		; POS
key_print:
	.byte	'R,'I,'N,'T,tk_print	; PRINT
	.byte	0x00
tab_ascr:
key_read:
	.byte	'E,'A,'D,tk_read	; READ
key_rem:
	.byte	'E,'M,tk_rem		; REM
key_restore:
	.byte	'E,'S,'T,'O,'R,'E,tk_restore ; RESTORE
key_return:
	.byte	'E,'T,'U,'R,'N,tk_return ; RETURN
key_rights:
	.byte	'I,'G,'H,'T,'$,tk_rights ; RIGHT$
key_rnd:
	.byte	'N,'D,tk_rnd		; RND
key_run:
	.byte	'U,'N,tk_run		; RUN
	.byte	0x00
tab_ascs:
key_sadd:
	.byte	'A,'D,'D,tk_sadd	; SADD
key_save:
	.byte	'A,'V,'E,tk_save	; SAVE
key_sgn:
	.byte	'G,'N,tk_sgn		; SGN
key_sin:
	.byte	'I,'N,tk_sin		; SIN
key_spc:
	.byte	'P,'C,'(,tk_spc		; SPC(
key_sqr:
	.byte	'Q,'R,tk_sqr		; SQR
key_step:
	.byte	'T,'E,'P,tk_step	; STEP
key_stop:
	.byte	'T,'O,'P,tk_stop	; STOP
key_strs:
	.byte	'T,'R,'$,tk_strs	; STR$
key_swap:
	.byte	'W,'A,'P,tk_swap	; SWAP
	.byte	0x00
tab_asct:
key_tab:
	.byte	'A,'B,'(,tk_tab		; TAB(
key_tan:
	.byte	'A,'N,tk_tan		; TAN
key_then:
	.byte	'H,'E,'N,tk_then	; THEN
key_to:
	.byte	'O,tk_to		; TO
key_twopi:
	.byte	'W,'O,'P,'I,tk_twopi	; TWOPI
	.byte	0x00
tab_ascu:
key_ucases:
	.byte	'C,'A,'S,'E,'$,tk_ucases ; UCASE$
key_until:
	.byte	'N,'T,'I,'L,tk_until	; UNTIL
key_usr:
	.byte	'S,'R,tk_usr		; USR
	.byte	0x00
tab_ascv:
key_val:
	.byte	'A,'L,tk_val		; VAL
key_vptr:
	.byte	'A,'R,'P,'T,'R,tk_vptr	; VARPTR
	.byte	0x00
tab_ascw:
key_wait:
	.byte	'A,'I,'T,tk_wait	; WAIT
key_while:
	.byte	'H,'I,'L,'E,tk_while	; WHILE
key_width:
	.byte	'I,'D,'T,'H,tk_width	; WIDTH
	.byte	0x00
tab_powr:
key_power:
	.byte	tk_power,0x00		; ^

; just messages

lab_bmsg:
	.byte	0x0d,0x0a,'B,'r,'e,'a,'k,0x00
lab_emsg:
	.byte	' ,'E,'r,'r,'o,'r,0x00
lab_lmsg:
	.byte	' ,'i,'n,' ,'l,'i,'n,'e,' ,0x00
lab_imsg:
	.byte	'E,'x,'t,'r,'a,' ,'i,'g,'n,'o,'r,'e,'d,0x0d,0x0a,0x00
lab_redo:
	.byte	'R,'e,'d,'o,' ,'f,'r,'o,'m,' ,'s,'t,'a,'r,'t,0x0d,0x0a,0x00
lab_rmsg:
	.byte	0x0d,0x0a,'R,'e,'a,'d,'y,0x0d,0x0a,0x00

lab_mszm:
	.byte	0x0d,0x0a,'M,'e,'m,'o,'r,'y,' ,'s,'i,'z,'e,' ,0x00

lab_smsg:
	.byte	' ,'B,'y,'t,'e,'s,' ,'f,'r,'e,'e,0x0d,0x0a,0x0a
	.byte	'E,'n,'h,'a,'n,'c,'e,'d,' ,'6,'8,'k,' ,'B,'A,'S,'I,'C,' ,'V,'e,'r,'s,'i,'o,'n,' ,'1,'.,'1,'0,0x0d,0x0a,0x00

;************************************************************************************
; EhBASIC keywords quick reference list                                             *
;************************************************************************************

; glossary

      ; <.>         required
      ; {.|.}       one of required
      ; [.]         optional
      ; ...         may repeat as last

      ; any       = anything
      ; num       = number
      ; state     = statement
      ; pint      = positive integer
      ; str       = string
      ; var       = variable
      ; nvar      = numeric variable
      ; svar      = string variable
      ; expr      = expression
      ; nexpr     = numeric expression
      ; sexpr     = string expression

; statement separator

; :         . [state] : [state]                                   * done

; number bases

; %         . %<binary num>                                       * done
; $         . $<hex num>                                          * done

; commands

; END       . END                                                 * done
; FOR       . FOR <nvar>=<nexpr> TO <nexpr> [STEP <nexpr>]        * done
; NEXT      . NEXT [<nvar>[,<nvar>]...]                           * done
; DATA      . DATA [{num|["]str["]}[,{num|["]str["]}]...]         * done
; INPUT     . INPUT [<">str<">;] <var>[,<var>[,<var>]...]         * done
; DIM       . DIM <var>(<nexpr>[,<nexpr>[,<nexpr>]])              * done
; READ      . READ <var>[,<var>[,<var>]...]                       * done
; LET       . [LET] <var>=<expr>                                  * done
; DEC       . DEC <nvar>[,<nvar>[,<nvar>]...]                     * done
; GOTO      . GOTO <pint>                                         * done
; RUN       . RUN [pint]                                          * done
; IF        . IF <nexpr> {THEN {pint|state}|GOTO <pint>}          * done
; RESTORE   . RESTORE [pint]                                      * done
; GOSUB     . GOSUB <pint>                                        * done
; REM       . REM [any]                                           * done
; STOP      . STOP                                                * done
; ON        . ON <nexpr> {GOTO|GOSUB} <pint>[,<pint>[,<pint>]...] * done
; NULL      . NULL <nexpr>                                        * done
; INC       . INC <nvar>[,<nvar>[,<nvar>]...]                     * done
; WAIT      . WAIT <nexpr>,<nexpr>[,<nexpr>]                      * done
; LOAD      . LOAD <sexpr>                                        * done for simulator
; SAVE      . SAVE <sexpr>                                        * done for simulator
; DEF       . DEF FN<var>(<var>)=<expr>                           * done
; POKE      . POKE <nexpr>,<nexpr>                                * done
; DOKE      . DOKE <nexpr>,<nexpr>                                * done
; LOKE      . LOKE <nexpr>,<nexpr>                                * done
; CALL      . CALL <nexpr>                                        * done
; DO        . DO                                                  * done
; LOOP      . LOOP [{WHILE|UNTIL}<nexpr>]                         * done
; PRINT     . PRINT [{;|,}][expr][{;|,}[expr][{;|,}[expr]]...]    * done
; CONT      . CONT                                                * done
; LIST      . LIST [pint][-pint]                                  * done
; CLEAR     . CLEAR                                               * done
; NEW       . NEW                                                 * done
; WIDTH     . WIDTH [<pint>][,<pint>]                             * done
; GET       . GET <var>                                           * done
; SWAP      . SWAP <var>,<var>                                    * done
; BITSET    . BITSET <nexpr>,<nexpr>                              * done
; BITCLR    . BITCLR <nexpr>,<nexpr>                              * done

; sub commands (may not start a statement)

; TAB       . TAB(<nexpr>)                                        * done
; TO        . FOR <nvar>=<nexpr> TO <nexpr> [STEP <nexpr>]        * done
; FN        . FN<var>(<expr>)                                     * done
; SPC       . SPC(<nexpr>)                                        * done
; THEN      . IF <nexpr> {THEN {pint|comm}|GOTO <pint>}           * done
; NOT       . NOT <nexpr>                                         * done
; STEP      . FOR <nvar>=<nexpr> TO <nexpr> [STEP <nexpr>]        * done
; UNTIL     . LOOP [{WHILE|UNTIL}<nexpr>]                         * done
; WHILE     . LOOP [{WHILE|UNTIL}<nexpr>]                         * done

; operators

; +         . [expr] + <expr>                                     * done
; -         . [nexpr] - <nexpr>                                   * done
; *         . <nexpr> * <nexpr>                                   * done fast hardware
; /         . <nexpr> / <nexpr>                                   * done fast hardware
; ^         . <nexpr> ^ <nexpr>                                   * done
; AND       . <nexpr> AND <nexpr>                                 * done
; EOR       . <nexpr> EOR <nexpr>                                 * done
; OR        . <nexpr> OR <nexpr>                                  * done
; >>        . <nexpr> >> <nexpr>                                  * done
; <<        . <nexpr> << <nexpr>                                  * done

; compare functions

; <         . <expr> < <expr>                                     * done
; =         . <expr> = <expr>                                     * done
; >         . <expr> > <expr>                                     * done

; functions

; SGN       . SGN(<nexpr>)                                        * done
; INT       . INT(<nexpr>)                                        * done
; ABS       . ABS(<nexpr>)                                        * done
; USR       . USR(<expr>)                                         * done
; FRE       . FRE(<expr>)                                         * done
; POS       . POS(<expr>)                                         * done
; SQR       . SQR(<nexpr>)                                        * done fast shift/sub
; RND       . RND(<nexpr>)                                        * done 31 bit PRNG
; LOG       . LOG(<nexpr>)                                        * done fast cordic
; EXP       . EXP(<nexpr>)                                        * done fast cordic
; COS       . COS(<nexpr>)                                        * done fast cordic
; SIN       . SIN(<nexpr>)                                        * done fast cordic
; TAN       . TAN(<nexpr>)                                        * done fast cordic
; ATN       . ATN(<nexpr>)                                        * done fast cordic
; PEEK      . PEEK(<nexpr>)                                       * done
; DEEK      . DEEK(<nexpr>)                                       * done
; LEEK      . LEEK(<nexpr>)                                       * done
; SADD      . SADD(<sexpr>)                                       * done
; LEN       . LEN(<sexpr>)                                        * done
; STR$      . STR$(<nexpr>)                                       * done
; VAL       . VAL(<sexpr>)                                        * done
; ASC       . ASC(<sexpr>)                                        * done
; UCASE$    . UCASE$(<sexpr>)                                     * done
; LCASE$    . LCASE$(<sexpr>)                                     * done
; CHR$      . CHR$(<nexpr>)                                       * done
; HEX$      . HEX$(<nexpr>)                                       * done
; BIN$      . BIN$(<nexpr>)                                       * done
; BTST      . BTST(<nexpr>,<nexpr>)                               * done
; MAX       . MAX(<nexpr>[,<nexpr>[,<nexpr>]...])                 * done
; MIN       . MIN(<nexpr>[,<nexpr>[,<nexpr>]...])                 * done
; PI        . PI                                                  * done
; TWOPI     . TWOPI                                               * done
; VARPTR    . VARPTR(<var>)                                       * done
; LEFT$     . LEFT$(<sexpr>,<nexpr>)                              * done
; RIGHT$    . RIGHT$(<sexpr>,<nexpr>)                             * done
; MID$      . MID$(<sexpr>,<nexpr>[,<nexpr>])                     * done

; This lot is in RAM

	.area	data			; org 0x40000
ram_strt:
					; I'll allow 1K for the stack

	.blkb	0x400			; org 0x40400
ram_base:
lab_warm:
	.blkw	1			; BASIC warm start entry point
wrmjpv:	.blkl	1			; BASIC warm start jump vector

usrjmp:	.blkw	1			; USR function JMP address
usrjpv:	.blkl	1			; USR function JMP vector

; system dependant i/o vectors
; these are in RAM and are set at start-up

v_inpt:	.blkw	1			; non halting scan input device entry point
v_inptv:
	.blkl	1			; non halting scan input device jump vector

v_outp:	.blkw	1			; send byte to output device entry point
v_outpv:
	.blkl	1			; send byte to output device jump vector

v_load:	.blkw	1			; load BASIC program entry point
v_loadv:
	.blkl	1			; load BASIC program jump vector

v_save:	.blkw	1			; save BASIC program entry point
v_savev:
	.blkl	1			; save BASIC program jump vector

v_ctlc:	.blkw	1			; save CTRL-C check entry point
v_ctlcv:
	.blkl	1			; save CTRL-C check jump vector

entry_sp:
	.blkl	1			; stack value on entry to basic

itemp:	.blkl	1			; temporary integer (for GOTO etc)

smeml:	.blkl	1			; start of memory (start of program)
sfncl:	.blkl	1			; start of functions    (end of Program)
svarl:	.blkl	1			; start of variables    (end of functions)
sstrl:	.blkl	1			; start of strings      (end of variables)
sarryl:	.blkl	1			; start of arrays       (end of strings)
earryl:	.blkl	1			; end of arrays         (start of free mem)
sstorl:	.blkl	1			; string storage        (moving down)
ememl:	.blkl	1			; end of memory         (upper bound of RAM)
sutill:	.blkl	1			; string utility ptr
clinel:	.blkl	1			; current line          (Basic line number)
blinel:	.blkl	1			; break line            (Basic line number)

cpntrl:	.blkl	1			; continue pointer
dlinel:	.blkl	1			; current DATA line
dptrl:	.blkl	1			; DATA pointer
rdptrl:	.blkl	1			; read pointer
varname:
	.blkl	1			; current var name
cvaral:	.blkl	1			; current var address
frnxtl:	.blkl	1			; var pointer for FOR/NEXT
lvarpl	=	frnxtl			; let var pointer low byte

des_sk_e:
	.blkl	6			; descriptor stack end
des_sk:
					; descriptor stack start address
					; use a4 for the descriptor pointer

; Ibuffs can now be anywhere in RAM just make sure the byte before it is <> $00

	.blkw	1
ibuffs:	.blkl	0x40			; start of input buffer
ibuffe:
					; end of input buffer

fac1_m:	.blkl	1			; FAC1 mantissa1
fac1_e:	.blkw	1			; FAC1 exponent
fac1_s	=	fac1_e+1		; FAC1 sign (b7)
	.blkw	1

fac2_m:	.blkl	1			; FAC2 mantissa1
fac2_e:	.blkl	1			; FAC2 exponent
fac2_s	=	fac2_e+1		; FAC2 sign (b7)
fac_sc	=	fac2_e+2		; FAC sign comparison, Acc#1 vs #2
flag	=	fac2_e+3		; flag byte for divide routine

prnlword:
	.blkl	1			; PRNG seed long word

ut1_pl:	.blkl	1			; utility pointer 1

asptl:	.blkl	1			; array size/pointer
adatal:	.blkl	1			; array data pointer

astrtl:	.blkl	1			; array start pointer low byte

numexp	=	astrtl			; string to float number exponent count
expcnt	=	astrtl+1		; string to float exponent count

expneg	=	astrtl+3		; string to float eval exponent -ve flag

func_l:	.blkl	1			; function pointer


					; these two need to be a word aligned pair !
defdim:	.blkw	1			; default DIM flag
cosout	=	defdim			; flag which CORDIC output (re-use byte)
dtypef	=	defdim+1		; data type flag, $80=string, $40=integer, $00=float


binss:	.blkl	4			; number to bin string start (32 chrs)

decss:	.blkl	1			; number to decimal string start (16 chrs)
	.blkw	1			;
usdss:	.blkw	1			; unsigned decimal string start (10 chrs)

hexss:	.blkl	2			; number to hex string start (8 chrs)

bhsend:	.blkw	1			; bin/decimal/hex string end


prstk:	.blkb	1			; stacked function index

srchc:	.blkb	1			; search character
tpower	=	srchc			; remember CORDIC power (re-use byte)

asrch:	.blkb	1			; scan-between-quotes flag, alt search character

dimcnt:	.blkb	1			; # of dimensions

breakf:	.blkb	1			; break flag, $00=END else=break
oquote:	.blkb	1			; open quote flag (Flag: DATA; LIST; memory)
gclctd:	.blkb	1			; garbage collected flag
sufnxf:	.blkb	1			; subscript/FNX flag, 1xxx xxx = FN(0xxx xxx)
imode:	.blkb	1			; input mode flag, $00=INPUT, $98=READ

cflag:	.blkb	1			; comparison evaluation flag

tabsiz:	.blkb	1			; TAB step size

tempb:	.blkb	1			; temp byte

comp_f:	.blkb	1			; compare function flag, bits 0,1 and 2 used
					; bit 2 set if >
					; bit 1 set if =
					; bit 0 set if <

nullct:	.blkb	1			; nulls output after each line
tpos:	.blkb	1			; BASIC terminal position byte
twidth:	.blkb	1			; BASIC terminal width byte
iclim:	.blkb	1			; input column limit
ccflag:	.blkb	1			; CTRL-C check flag
ccbyte:	.blkb	1			; CTRL-C last received byte
ccnull:	.blkb	1			; CTRL-C last received byte 'life' timer

; these variables for simulator load/save routines

file_byte:
	.blkb	1			; load/save data byte
file_id:
	.blkl	1			; load/save file ID

	.word	0			; dummy even value and zero pad byte

prg_strt:
ram_top	=	0x48000			; last RAM byte + 1

	.end	code_start
