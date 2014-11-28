	.globl n_objects
n_objects:
	.integer 0
	.globl objects
objects: #needs to be initialized
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.globl screen
screen:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl viewpoint
viewpoint:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl light
light:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl beam
beam:
	.float 255.0
	.globl and_net
and_net: #needs to be initialized
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.globl or_net
or_net: #needs to be initialized
	.integer 0
	.globl solver_dist
solver_dist:
	.float 0.0
	.globl intsec_rectside
intsec_rectside:
	.integer 0
	.globl tmin
tmin:
	.float 1000000000.0
	.globl intersection_point
intersection_point:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl intersected_object_id
intersected_object_id:
	.integer 0
	.globl nvector
nvector:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl texture_color
texture_color:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl diffuse_ray
diffuse_ray:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl rgb
rgb:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl image_size
image_size:
	.integer 0
	.integer 0
	.globl image_center
image_center:
	.integer 0
	.integer 0
	.globl scan_pitch
scan_pitch:
	.float 0.0
	.globl startp
startp:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl startp_fast
startp_fast:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl screenx_dir
screenx_dir:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl screeny_dir
screeny_dir:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl screenz_dir
screenz_dir:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl ptrace_dirvec
ptrace_dirvec:
	.float 0.0
	.float 0.0
	.float 0.0
	.globl dirvecs
dirvecs: #needs to be initialized
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.globl light_dirvec
light_dirvec: #needs to be initialized
	.integer 0
	.integer 0
	.globl reflections
reflections: #needs to be initialized
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.integer 0
	.globl n_reflections
n_reflections:
	.integer 0
# initialize some of the elements 
	.global initialize	
initialize:	# does not save regs bigger than $r04
	li     	$r04, 236 #59 * 4
	li     	$r05, objects
	subi 	$r06, $r00, 1
init_L1:		#do{
	li     	$r01, 11 
	li     	$r02, 0
	call    create_array #creates dummy
	add     $r02, $r04, $r05
	store   $r01, $r02, 0 #sets the a link to dummy
	subi    $r04, $r04, 4
	blti     $r06, $r04, init_L1 #}while(-1 < $r03) i.e. while($03>=0)
	li     	$r04, 196 # 49 * 4
	li     	$r05, and_net
init_L2:
	li     	$r01, 1
        li	$r02, -1
	call    create_array
	add     $r02, $r04, $r05
	store   $r01, $r02, 0
	subi    $r04, $r04, 4
	blti    $r06, $r04, init_L2
	li     	$r01, 1
	li     	$r02, -1
	call    create_array
	mov     $r02, $r01
	li     	$r01, 1
	call    create_array
	store   $r01, $r00, or_net
	li     	$r05, light_dirvec
	li     	$r01, 3
	li	$r02, 0
	call    create_array
	store	$r01, $r05, 0
	li     	$r01, 60
	call    create_array
	store   $r01,  $r05, 4 
	li 	$r04, 716 #179 * 4
	li	$r05, reflections
init_L3:
	li     	$r01, 2
	call    create_array
	mov     $r07, $r01
	li     	$r01, 3
	li     	$r02, 0
	call   	create_array
	store  	$r07, $r01, 4
	add    	$r02, $r04, $r05
	store  	$r01, $r02, 0
	subi	$r04, $r04, 4
	blti    $r06, $r04, init_L3
	ret
