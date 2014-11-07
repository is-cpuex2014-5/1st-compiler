	.globl read_char
read_char:
	read	$r01
	ret	
	.globl write_char
put_char:
	write	$r01
	ret
	.globl print_float
	fstore	$f01, $r14, 4 #浮動小数点の関数呼ばれても汎用レジスタって退避されてるっけ？
	load	$r01, $r14, 4
	.globl print_int
print_int:
	slri	$r04, $r01, 24
	slri	$r03, $r01, 16
	slri	$r02, $r01, 8
	write   $r04
	write	$r03
	write	$r02
	write	$r01
	ret
	.globl	read_int
read_int:
	read	$r01
	read	$r02
	read	$r03
	read	$r04
	slli	$r01, $r01, 24
	slli	$r02, $r03, 16
	slli	$r03, $r02, 8
	add	$r01, $r01, $r02, 0
	add	$r01, $r01, $r03, 0
	add	$r01, $r01, $r04, 0
	ret
	.globl	read_float
read_float:
	read	$r01
	read	$r02
	read	$r03
	read	$r04
	slli	$r01, $r01, 24
	slli	$r02, $r03, 16
	slli	$r03, $r02, 8
	add	$r01, $r01, $r02, 0
	add	$r01, $r01, $r03, 0
	add	$r01, $r01, $r04, 0
	store	$r01, $r14, 4
	fload   $f01, $r14, 4
	ret
	
