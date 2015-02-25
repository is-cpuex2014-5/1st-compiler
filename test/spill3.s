	.text
	.globl  main
f.56:
	addil	$r02, $r01, 1
	addil	$r03, $r02, 1
	addil	$r04, $r03, 1
	addil	$r05, $r04, 1
	addil	$r06, $r05, 1
	addil	$r07, $r06, 1
	addil	$r08, $r07, 1
	addil	$r09, $r08, 1
	addil	$r10, $r09, 1
	store	$r01, $r14, 0
	addil	$r01, $r10, 1
	store	$r01, $r14, 4
	addil	$r01, $r01, 1
	store	$r01, $r14, 8
	addil	$r01, $r01, 1
	store	$r01, $r14, 12
	addil	$r01, $r01, 1
	store	$r01, $r14, 16
	addil	$r01, $r01, 1
	store	$r01, $r14, 20
	addil	$r01, $r01, 1
	store	$r01, $r14, 24
	addil	$r01, $r01, 1
	store	$r01, $r14, 28
	addil	$r01, $r01, 1
	store	$r01, $r14, 32
	addil	$r01, $r01, 1
	store	$r01, $r14, 36
	add	$r01, $r01, $r02, 0
	store	$r01, $r14, 40
	add	$r01, $r01, $r03, 0
	store	$r01, $r14, 44
	add	$r01, $r01, $r04, 0
	store	$r01, $r14, 48
	add	$r01, $r01, $r05, 0
	store	$r01, $r14, 52
	add	$r01, $r01, $r06, 0
	store	$r01, $r14, 56
	add	$r01, $r01, $r07, 0
	store	$r01, $r14, 60
	add	$r01, $r01, $r08, 0
	store	$r01, $r14, 64
	add	$r01, $r01, $r09, 0
	store	$r01, $r14, 68
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 72
	store	$r10, $r14, 76
	load	$r10, $r14, 4
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 80
	load	$r10, $r14, 8
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 84
	load	$r10, $r14, 12
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 88
	load	$r10, $r14, 16
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 92
	load	$r10, $r14, 20
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 96
	load	$r10, $r14, 24
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 100
	load	$r10, $r14, 28
	add	$r01, $r01, $r10, 0
	store	$r01, $r14, 104
	load	$r10, $r14, 32
	add	$r01, $r01, $r10, 0
	load	$r10, $r14, 0
	add	$r10, $r01, $r10, 0
	add	$r02, $r02, $r03, 0
	add	$r02, $r02, $r04, 0
	add	$r02, $r02, $r05, 0
	add	$r02, $r02, $r06, 0
	add	$r02, $r02, $r07, 0
	add	$r02, $r02, $r08, 0
	add	$r02, $r02, $r09, 0
	load	$r03, $r14, 76
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 4
	add	$r02, $r02, $r03, 0
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
	load	$r03, $r14, 40
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 44
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 48
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 52
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 56
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 60
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 64
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 68
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 72
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 80
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 84
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 88
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 92
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 96
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 100
	add	$r02, $r02, $r03, 0
	load	$r03, $r14, 104
	add	$r02, $r02, $r03, 0
	add	$r01, $r02, $r01, 0
	add	$r01, $r01, $r10, 0
	load	$r02, $r14, 0
	add	$r01, $r01, $r02, 0
	ret
main: # main entry point
   # main program start
	li	$r01, 0
	load	$r11, $r14, -4
	store	$r11, $r14, 4
	addil	$r14, $r14, 8
	call	f.56
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
