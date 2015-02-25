	.text
	.globl  main
f.26:
	li	$r01, 12345
	ret
g.28:
	addil	$r01, $r01, 1
	ret
main: # main entry point
   # main program start
	li	$r01, 10
	li	$r02, 1
	load	$r11, $r14, -4
	store	$r11, $r14, 4
	addil	$r14, $r14, 8
	call	min_caml_create_array
	subi	$r14, $r14, 8
	load	$r11, $r14, 4
	store	$r11, $r14, -4
	store	$r01, $r14, 0
	load	$r11, $r14, -4
	store	$r11, $r14, 4
	addil	$r14, $r14, 8
	call	f.26
	subi	$r14, $r14, 8
	load	$r11, $r14, 4
	store	$r11, $r14, -4
	lis	$r02, 1
	ori	$r02, $r02, 2354
	load	$r03, $r14, 0
	load	$r04, $r03, 0
	add	$r05, $r04, $r04, 0
	add	$r06, $r05, $r05, 0
	add	$r07, $r06, $r06, 0
	add	$r08, $r07, $r07, 0
	add	$r09, $r08, $r08, 0
	add	$r10, $r09, $r09, 0
	store	$r01, $r14, 4
	add	$r01, $r10, $r10, 0
	store	$r01, $r14, 8
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 12
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 16
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 20
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 24
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 28
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 32
	add	$r01, $r01, $r01, 0
	store	$r01, $r14, 36
	add	$r01, $r01, $r01, 0
	load	$r03, $r03, 4
	beq	$r03, 0, bne_else.86
	add	$r02, $r04, $r05, 0
	add	$r02, $r02, $r06, 0
	add	$r02, $r02, $r07, 0
	add	$r02, $r02, $r08, 0
	add	$r02, $r02, $r09, 0
	add	$r02, $r02, $r10, 0
	load	$r03, $r14, 8
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 12
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 16
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 20
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 24
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 28
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 32
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 36
	add	$r02, $r02, $r03, 0
	add	$r01, $r02, $r01, 0
	load	$r02, $r14, 4
	add	$r01, $r01, $r02, 0
	beqi	$r00, $r00, bne_cont.87
bne_else.86:
	load	$r11, $r14, -4
	mov	$r01, $r02
	store	$r11, $r14, 44
	addil	$r14, $r14, 48
	call	g.28
	subi	$r14, $r14, 48
	load	$r11, $r14, 44
	store	$r11, $r14, -4
bne_cont.87:
	load	$r11, $r14, -4
	store	$r11, $r14, 44
	addil	$r14, $r14, 48
	call	min_caml_print_int
	subi	$r14, $r14, 48
	load	$r11, $r14, 44
	store	$r11, $r14, -4
   # main program end
	beq	$r00, $r00, $r15, 0
