	.text
	.globl  main
f.43:
	add	$r05, $r01, $r02, 0
	add	$r06, $r01, $r03, 0
	add	$r01, $r01, $r04, 0
	add	$r07, $r02, $r03, 0
	add	$r02, $r02, $r04, 0
	add	$r08, $r03, $r04, 0
	add	$r09, $r05, $r06, 0
	add	$r10, $r05, $r01, 0
	store	$r04, $r14, 0
	add	$r04, $r05, $r07, 0
	store	$r03, $r14, 4
	add	$r03, $r05, $r02, 0
	store	$r05, $r14, 8
	add	$r05, $r05, $r08, 0
	store	$r05, $r14, 12
	add	$r05, $r06, $r01, 0
	store	$r05, $r14, 16
	add	$r05, $r06, $r07, 0
	store	$r05, $r14, 20
	add	$r05, $r06, $r02, 0
	store	$r06, $r14, 24
	add	$r06, $r06, $r08, 0
	store	$r06, $r14, 28
	add	$r06, $r01, $r07, 0
	store	$r06, $r14, 32
	add	$r06, $r01, $r02, 0
	store	$r01, $r14, 36
	add	$r01, $r01, $r08, 0
	store	$r01, $r14, 40
	add	$r01, $r07, $r02, 0
	store	$r07, $r14, 44
	add	$r07, $r07, $r08, 0
	store	$r08, $r14, 48
	add	$r08, $r02, $r08, 0
	store	$r10, $r14, 52
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 56
	add	$r10, $r09, $r04, 0
	store	$r10, $r14, 60
	add	$r10, $r09, $r03, 0
	store	$r10, $r14, 64
	load	$r10, $r14, 12
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 68
	load	$r10, $r14, 16
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 72
	load	$r10, $r14, 20
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 76
	add	$r10, $r09, $r05, 0
	store	$r10, $r14, 80
	load	$r10, $r14, 28
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 84
	load	$r10, $r14, 32
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 88
	add	$r10, $r09, $r06, 0
	store	$r10, $r14, 92
	load	$r10, $r14, 40
	add	$r10, $r09, $r10, 0
	store	$r10, $r14, 96
	add	$r10, $r09, $r01, 0
	store	$r10, $r14, 100
	add	$r10, $r09, $r07, 0
	store	$r10, $r14, 104
	add	$r10, $r09, $r08, 0
	store	$r10, $r14, 108
	load	$r10, $r14, 4
	store	$r08, $r14, 112
	load	$r08, $r14, 8
	add	$r10, $r08, $r10, 0
	store	$r07, $r14, 116
	load	$r07, $r14, 0
	add	$r07, $r10, $r07, 0
	add	$r07, $r07, $r08, 0
	load	$r08, $r14, 24
	add	$r07, $r07, $r08, 0
	load	$r08, $r14, 36
	add	$r07, $r07, $r08, 0
	load	$r08, $r14, 44
	add	$r07, $r07, $r08, 0
	add	$r02, $r07, $r02, 0
	load	$r07, $r14, 48
	add	$r02, $r02, $r07, 0
	add	$r02, $r02, $r09, 0
	load	$r07, $r14, 52
	add	$r02, $r02, $r07, 0
	add	$r02, $r02, $r04, 0
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 12
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 16
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 20
	add	$r02, $r02, $r03, 0
	add	$r02, $r02, $r05, 0
	load	$r03, $r14, 28
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 32
	add	$r02, $r02, $r03, 0
	add	$r02, $r02, $r06, 0
	load	$r03, $r14, 40
	add	$r02, $r02, $r03, 0
	add	$r01, $r02, $r01, 0
	load	$r02, $r14, 116
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 112
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 56
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 60
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 64
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 68
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 72
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 76
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 80
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 84
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 88
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 92
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 96
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 100
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 104
	add	$r01, $r01, $r02, 0
	load	$r02, $r14, 108
	add	$r01, $r01, $r02, 0
	neg	$r01, $r01
	ret
main: # main entry point
   # main program start
	li	$r01, 1
	li	$r02, 2
	li	$r03, 3
	li	$r04, 4
	load	$r11, $r14, -4
	store	$r11, $r14, 4
	addil	$r14, $r14, 8
	call	f.43
	subi	$r14, $r14, 8
	load	$r11, $r14, 4
	store	$r11, $r14, -4
	load	$r11, $r14, -4
	store	$r11, $r14, 4
	addil	$r14, $r14, 8
	call	min_caml_print_int
	subi	$r14, $r14, 8
	load	$r11, $r14, 4
	store	$r11, $r14, -4
   # main program end
	beq	$r00, $r00, $r15, 0
