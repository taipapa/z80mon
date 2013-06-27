	org 0x0

EMULATION equ 1

;;; initialize 8255 and other peripherals
;;; SP set to highest RAM
sysinit:
	; set sp to bottom
	ld sp, 0x0

	; initialize 8255 portA in, portB in, portC out
	ld a, 0x92
	out (0x3), a
	jp main

;;; RST 0x38 handler
	org 0x38
	halt
	
;;; This is the main
main:	

	ld HL, banner
	call puts
	
readloop:
	;; prompt
	ld HL, prompt
	call putstr

	;; read command line
	call rdline
	call gettoken
	jr z readloop		; invalid line

	ld IX, cmdlist
_cmdparse
	push HL
	ld E, (IX+0)
	ld D, (IX+1)
	ld A, E
	or D
	jr z, _cmd_bad
	
	call strcmp
	jr z _cmdfound
	pop HL
	ld DE, 4
	add IX, DE
	jr _cmdparse

_cmd_bad
	ld HL, badcommand
	call puts
	jr readloop

_cmdfound
	ld L, (IX+2)
	ld H, (IX+3)
	jp HL


TEST:	cond 0
	
;;; Test items
puthextest
	ld a, 0
	call puthex
	ld a, 9
	call puthex
	ld a, 10
	call puthex
	ld a, 15
	call puthex

	ld A, 0xb9
	call puthex1
	ld A, 0xaf
	call puthex1

	ld HL,0xf89a
	call puthex2
	
	halt
	
linetest
	call rdline

tokenagain:	
	call gettoken
	jr z notoken
	
	ld a, '<'
	call putc
	call putstr
	ld a, '>'
	call putc

	jr tokenagain

notoken:
	ld a, '$'
	call putc
	jr linetest

stringtest:	
	ld BC, testdata4
	call str2hex
	
	ld HL, testdata1
	ld DE, testdata2
	call strcmp

	ld HL, testdata1
	ld DE, testdata3
	call strcmp

	ld BC, testdata4
	call str2hex

	halt

ENDTEST endc

;;; Skip whitespace, tab and CR/LF
skipspace:	
	ld A, (HL)
	cp 0x0d
	jr z _skipspace
	cp 0x0a
	jr z _skipspace
	cp 0x20
	jr z _skipspace
	cp 0x09
	jr z _skipspace
	ret
_skipspace
	inc HL
	jr skipspace

;;; Skip printable character
skipprintable:	
	ld A, (HL)
	cp 0x0d
	jr z _skipprintable_end
	cp 0x0a
	jr z _skipprintable_end
	cp 0x20
	jr z _skipprintable_end
	cp 0x09
	jr z _skipprintable_end
	or A
	jr z _skipprintable_end
	inc HL
	jr skipprintable
_skipprintable_end
	ret

;;; get token
;;; Z flag if no token available
gettoken:	
	ld HL, (consoletoken)
	call skipspace

	;; end of token check
	ld A, (HL)
	or A
	jr z _gettoken_noavail

	push HL
	call skipprintable

	;; already end of token
	ld A, (HL)
	or A
	jr z _gettoken_eol

	;; goto next token
	ld (HL), 0
	inc HL
_gettoken_eol
	ld (consoletoken), HL
	pop HL
	ld a, 1
	or a
_gettoken_noavail
	ret

;;; clearbuf
clearbuf:	
	ld b, BUFSIZ-1
	ld HL, consolebuf
_clearbufloop
	ld (HL), 0
	inc HL
	djnz _clearbufloop
	ret
	
;;; read 1 line, chop CR/LF, (re)initialize token pointer
;;; just read until buffer size
rdline:	
	;; call clearbuf
	ld b, BUFSIZ-1
	ld HL, consolebuf
_rdlineloop
	call getc

	;; EOL check (LF)
	cp 0x0a
	jr z _rdline_eol

	;; CR check
	cp 0x0d
	jr z _rdline_continue

	;; Backspace check
	cp 0x08
	jr z _rdline_backspace

	;; Echo back
	call putc

	;; store into buffer
	ld (HL), A
	inc HL
_rdline_continue
	djnz _rdlineloop
_rdline_eol
	;; null terminate
	ld (HL), 0
	;; echo back CR/LF
	call put_crlf
	;; (re)initialize token pointer
	ld HL, consolebuf
	ld (consoletoken), HL
	ret
_rdline_backspace
	;; echo back
	dec HL
	ld (HL), 0
	call putc
	;; and do not update buffer
	jr _rdline_continue

;;; strcmp HL¡¢DE, return to Z flag (Z=equal)
;;; DE MUST BE ZERO TERMINATED
strcmp:
	ld A, (DE)
	ld B, (HL)
	cp B
	ret nz		; return not eq
	or A
	ret z		; end of string
	inc HL
	inc DE
	jr strcmp
	
;;; conv *BC to hex, error to Z flag
;;; "0" = 0x30, "9" = 0x39, "a" = 0x61, "f" = 0x66
str2hex:	
	ld HL, str2hexbuf
	ld (HL), 0

	;; null string check
	ld A, (BC)
	or A
	jr z _str2hex_badhex

_str2hex_start
	ld A, (BC)
	or A
	jr z _str2hex_convend
	
	cp 0x30
	jr c _str2hex_badhex
	cp 0x3A
	jr c _str2hex_addhex_n

	;; alphabet hex
	cp 0x61
	jr c _str2hex_badhex
	cp 0x67
	jr c _str2hex_addhex_a
_str2hex_badhex
	xor A	; Z flag is set
	ret
_str2hex_addhex_n
	sub 0x30
	jr _str2hex_addhex_rotate
	
_str2hex_addhex_a
	sub 0x61
	add A, 10

_str2hex_addhex_rotate
	;; now A has 4bit 0-15 value
	RLD
	INC HL
	RLD
	DEC HL
	INC BC
	jr _str2hex_start

_str2hex_convend
	ld HL, (str2hexbuf)
	;; Z flag cleared
	or 1
	ret

;;; put string in HL to console
putstr:
	ld A, (HL)
	or A
	jr z _putstr_eol
	call putc
	inc HL
	jr putstr
_putstr_eol
	;; call put_crlf
	ret

;;; put string in HL and CRLF to console

puts:	
	call putstr
	call put_crlf
	ret
	
put_crlf:	
	ld a, 0x0d
	call putc
	ld a, 0x0a	
	call putc
	ret

;;; For z80-asm emulation
	cond EMULATION

putc:	
	out (0x1), A
	ret

getc:	
	in A, (0x0)
	or A
	jr z getc
	ret

	endc
	
;;; For real hardware
	cond !EMULATION
	
	;;; put 1 char to A, waiting until buffer is available
putc:
	push AF
_putc_loop:	
	in A, (0x2)
	and 0x2			; TXE#, 1 for "do not send"
	or A
	jr nz _putc_loop	; wait until buffer is available
	pop AF
	out (0x30), A
	ret
	
	ret
	
	;;; get 1 char to A, waiting until char is read
getc:
	in A, (0x2)
	and 0x1			; RXF#, 1 for "do not read"
	or A
	jr nz getc		; wait until data is available
	in A, (0x30)
	ret

	endc

;;; put 1digit in A by hex
puthex:	
	cp 10
	jr c _puthex_digit
	add A, 97-10
	jr _puthex_put
_puthex_digit
	add A, 48
_puthex_put
	call putc
	ret

;;; put 1byte in A by hex
puthex1:	
	push AF
	srl A
	srl A
	srl A
	srl A
	call puthex
	pop AF
	and 0xf
	call puthex
	ret
	
;;; put 2byte in HL by hex
puthex2:	
	ld a, h
	call puthex1
	ld a, l	
	call puthex1
	ret

;;; do read
do_read:	
	call gettoken
	jp z command_error
	ld B, H
	ld C, L
	call str2hex
	jp z command_error
	call dump_memory
	jp readloop

DUMP_LINE_BYTES equ 16

;;; print 16bytes in line with header
;;; Start address in HL
dump_memory:
	push HL
	call puthex2
	ld A, ':'
	call putc
	pop HL

	ld B, DUMP_LINE_BYTES
_dump_memory_loop
	ld A, (HL)
	call puthex1
	ld A, ' '
	call putc
	inc HL
	djnz _dump_memory_loop
	call put_crlf
	ret

;;; do write
do_write:	
	call gettoken
	jp z command_error
	ld B, H
	ld C, L
	call str2hex		; HL=write address
	jp z command_error
	
_do_write_again
	push HL

	;; prompt write address
	call puthex2
	ld A, ':'
	call putc

	;; request value
	call rdline
	call gettoken
	jr z _do_write_abort
	ld B, H
	ld C, L
	call str2hex
	jr z _do_write_abort

	;; write value
	ld A, L
	pop HL
	ld (HL), A
	inc HL
	jr _do_write_again

_do_write_abort	
	jp readloop
	
;;; do write word
do_write_word:	
	ld HL, cmd_write_word
	call puts
	jp readloop

;;; do help
do_help:	
	ld HL, help_string
	call puts
	jp readloop

;;; go
do_go:	
	call gettoken
	jp z command_error
	ld B, H
	ld C, L
	call str2hex		; HL=jump address
	jp z command_error
	ld D, H
	ld E, L
	ld IX, _do_go_jmp
	ld (IX+1), L
	ld (IX+2), H
_do_go_jmp
	call 0x0
	jp readloop

;;; in

do_in:
	call gettoken
	jp z command_error
	ld B, H
	ld C, L
	call str2hex		; HL=port address
	jp z command_error
	ld C, L
	in A, (C)
	call puthex1
	call put_crlf
	jp readloop

;;; out
do_out:
	call gettoken
	jp z command_error
	ld B, H
	ld C, L
	call str2hex		; HL=port address
	jp z command_error
	push HL
	;; get value to out
	call gettoken
	jp z command_error
	ld B, H
	ld C, L
	call str2hex		; HL=value to out
	jp z command_error
	pop BC			; port address
	ld A, L			; value to out
	out (C), A
	jp readloop
	
;;; common command error
command_error
	ld HL, badparam
	call puts
	jp readloop
	
cmdlist:
	defw cmd_read, do_read
	defw cmd_write, do_write
	defw cmd_write_word, do_write_word
	defw cmd_go, do_go
	defw cmd_in, do_in
	defw cmd_out, do_out	
	defw cmd_help, do_help
	defw 0

cmd_read
	defm "r"
	defb 0
	
cmd_write
	defm "w"
	defb 0

cmd_write_word
	defm "ww"
	defb 0

cmd_help
	defm "help"
	defb 0

cmd_go
	defm "go"
	defb 0

cmd_in
	defm "in"
	defb 0

cmd_out
	defm "out"
	defb 0

help_string
	defm "cmds: r, w, ww, go, in, out, help"
	defb 0

banner
	defm "Z80MON by taipapa"
	defb 0

badcommand
	defm "???"
	defb 0

badparam
	defm "??"
	defb 0
	
prompt
	defm "* "
	defb 0

TESTDATA	cond 0
;;; test data
testdata1
	defm "test"
	defb 0

testdata2
	defm "test"
	defb 0
	
testdata3
	defm "tesT"
	defb 0

testdata4
	defm "f0a"
	defb 0
	
ENDTESTDATA	endc	

;;; Followings are placed into RAM area
	org 0x8000

str2hexbuf
	defs 2

BUFSIZ equ 32
consolebuf
	defs BUFSIZ

consoletoken
	defs 2
