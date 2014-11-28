	.globl read_char
read_byte:
read_char:
	read	$r01
	ret
	.globl print_char
print_byte:	
print_char:
	write   $r01
	ret

	.globl	read_int
read_int:
	read	$r01
	read    $r02
	read    $r03
	read	$r04
	slli	$r01, $r01, 24
	slli  	$r02, $r02, 16
	slli	$r03, $r03,  8
	add     $r01, $r01, $r02, 0
	add     $r01, $r01, $r03, 0
	add     $r01, $r01, $r04, 0
	ret

	.globl	read_float
read_float:
	read	$r01
	read    $r02
	read    $r03
	read	$r04
	slli	$r01, $r01, 24
	slli  	$r02, $r02, 16
	slli	$r03, $r03,  8
	add     $r01, $r01, $r02, 0
	add     $r01, $r01, $r03, 0
	add     $r01, $r01, $r04, 0
	store	$r01, $r14, 4
	fload	$f01, $r14, 4
	ret

	.globl  print_float
print_float:
	fstore	$f01, $r14, 4
	load	$r01, $r14, 4
	.globl	print_int
print_int:
	srli	$r04, $01, 24
	srli	$r03, $01, 16
	srli	$r02, $01, 8
	write	$r04
	write 	$r03
	write	$r02
	write	$r01
	ret
	
	.globl create_float_array
	.globl create_array
create_float_array:	
create_array: 
	mov 	$r03, $r13  #ret = hp
create_array_L0:
	beqi	$r01, $r00, create_array_L1 #while(i != 0){
	store	$r02, $r13, 0 #*hp = initial value ;
	subi 	$r01, $r01, 1 # i-- ;
	addil	$r13, $r13, 4 # hp += 4 ;
	beqi	$r00, $r00, create_array_L0 #}
create_array_L1:
	mov	$r01, $r03 #return ret 
	ret

		.data
sqrt_D0:
	.integer   1597463174           # magic number m
sqrt_D1:
	.float  0.5
sqrt_D2:
	.float  1.5

	.text 
	.globl sqrt
sqrt: # sqrt by Newton's method(N = 3, using reciprocal of the root)
	fload 	$f03, $r00, sqrt_D0 
	fload	$f04, $r00, sqrt_D1
	fload	$f05, $r00, sqrt_D2
	fstore  $f01, $r14, 4
	load	$r02, $r14, 4
	load 	$r03, $r00, sqrt_D0
	srli    $r02, $r02, 1
	sub     $r02, $r03, $r02   # x0 = m - (u32of(x) >> 1);	
        store   $r02, $r14, 4
	fload	$f02, $r14, 4
	fmul    $f04, $f01, $f04   #  y = 0.5 * x;
	fmul    $f03, $f02, $f04   # 3 times
	fmul    $f03, $f02, $f03
	fsub    $f03, $f05, $f03           
	fmul    $f02, $f02, $f03   # xn = xn * (1.5 - y * xn * xn );
	fmul    $f03, $f02, $f04
	fmul    $f03, $f02, $f03
	fsub    $f03, $f05, $f03
	fmul    $f02, $f02, $f03
	fmul    $f03, $f02, $f04
	fmul    $f03, $f02, $f03
	fsub    $f03, $f05, $f03
	fmul    $f02, $f02, $f03
	fmul    $f01, $f01, $f02          # return x * 1/sqrt(x);
	ret
	.data
finv_D0:
	.float  0.5
finv_D1:
	.float  2.0
finv_D2:
	.float  -1.8823529          # -32.0 / 17.0
finv_D3:
	.float	2.8235294          #  48.0 / 17.0
	.globl finv
finv:
	fstore	$f01, $r14, 4
	load	$r01, $r14, 4
	srli    $r02, $r01, 23
	sub     $r02, $r00, $r02	# sign($f02) = sign($f01);
	addil   $r02, $r02, 253		# exponent($f02) = 253 - exponent($f01);
	slli    $r02, $r02, 23          # fraction($f02) = 0;
	store	$r02, $r14, 8
	fload	$f02, $r14, 8
	load	$r03, $r00, finv_D0
	slli    $r01, $r01, 9
	srli 	$r01, $r01, 9           # sign($f01) = 0;
	add     $r01, $r01, $r03        # exponent($f01) = 126;
	store   $r01, $r14, 4
	fload   $f01, $r14, 4
	fload   $f03, $r00, finv_D2
	fload   $f04, $r00, finv_D3
	fmul    $f03, $f01, $f03          # // initial guess
	fadd    $f03, $f03, $f04          # $f03 = -32/17 * $f01 + 48/17;
	fload   $f05, $r00, finv_D1
	fmul    $f04, $f01, $f03
	fsub    $f04, $f05, $f04          # // repeat 3 times
	fmul    $f03, $f03, $f04          # $f03 *= 2.0 - $f01 * $f03;
	fmul    $f04, $f01, $f03
	fsub    $f04, $f05, $f04
	fmul    $f03, $f03, $f04
	fmul    $f04, $f01, $f03
	fsub    $f04, $f05, $f04
	fmul    $f03, $f03, $f04
	fmul    $f01, $f02, $f03          # return $2 * $3;
	ret
floor_D0:
	.float 0.0
	.globl floor
floor: 
	fload	$f02, $r00, floor_D0
	bflti	$f01, $f02, floor_L0
	ftoi	$r01, $f01
	itof	$f01, $r01
	ret
floor_L0: # if($f01 < 0)
	ftoi	$r01, $f01
	itof	$f03, $r01
	bfeqi	$f01, $f03, floor_L1
	subi	$r01, $r01, 1
	itof	$f01, $r01
	ret
floor_L1: #if $f01 is integer
	ret
