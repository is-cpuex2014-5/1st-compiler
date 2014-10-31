	.globl n_objects
n_objects:
	.int 0
	.globl objects
objects: #needs to be initialized
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
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
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.globl or_net
or_net: #needs to be initialized
	.int 0
	.globl solver_dist
solver_dist:
	.float 0.0
	.globl intsec_rectside
intsec_rectside:
	.int 0
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
	.int 0
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
	.int 0
	.int 0
	.globl image_center
image_center:
	.int 0
	.int 0
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
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.globl light_dirvec
light_dirvec: #needs to be initialized
	.int 0
	.int 0
	.globl reflections
reflections: #needs to be initialized
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.int 0
	.globl n_reflections
n_reflections:
	.int 0
# initilize some of the elements 
	.global initialize
initialize:
	li     $r03, 59
	li     $r04, objects
L1:			#do{
	li     $r01, 11 
	li     $r02, 0
	call    create_array #creates dummy
	add     $r02, $r03, $r04
	store   $r01, $r02, 0 #sets the a link to dummy
	subi    $r03, $r03, 1
	subi 	$r05, $r00, 1
	blt     $r05, $r03, L1 #}while(-1 < $r03) i.e. while($03>=0)
	li     	$r03, 49
	li     	$r04, and_net
L2:
	li     	$r01, 1
        li	$r02, -1
	call    create_array
	add     $r02, $r03, $r04
	store   $r01, $r02, 1
	subi    $r03, $r03, 1
	subi 	$r05, $r00, 1
	blt     $r05, $r03, L2
	li     	$r01, 1
	li     	$r02, -1
	call    create_array
	mov     $r02, $r01
	li     	$r01, 1
	call    create_array
	store   $r01, $r00, or_net
	li     	$r04, light_dirvec
	li     	$r01, 3
	li	$r02, 0
	call    create_array
	store	$r01, $r04, 0
	li     $r01, 60
	call    create_array
	store   $r01,  $r04, 1 
	mov     $r03, 179
	mov     $r04, reflections
L3:
	li     	$r01, 2
	call    create_array
	li     	$r03, $1
	li     	$r01, 3
	li     	$r02, 0
	call   	create_array
	store  	$r03, $r01, 1
	add    	$r02, $r04, $r05
	store  	$r01, $r02, 0
	subi	$r04, $r04, 1
	subi 	$r06, $r00, 1
	blt     $r06, $r04, L2
	ret
	
	
