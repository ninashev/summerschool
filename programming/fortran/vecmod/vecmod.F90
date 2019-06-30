module vector_algebra
  use iso_fortran_env, only : REAL64
  implicit none
  type vector_t
     real(REAL64) :: x, y, z
  end type vector_t

  ! TODO: overload operators needed by the parser

 interface operator(+)
  module procedure vector_sum
 end interface operator(+)

 interface operator(-)
  module procedure vector_min
 end interface operator(-)

 interface operator(*)
  module procedure vector_times
 end interface(*)

 interface abs
  module procedure vector_abs
 end interface abs
 
contains
  ! TODO: implement the corresponding functions

  function vector_sum(v1, v2) result(v3)
   type(vector_t),intent(in) :: v1,v2
   type(vector_t) :: v3

   v3 % x = v1 % x + v2 % x 
   v3 % y = v1 % y + v2 % y
   v3 % z = v1 % z + v2 % z
  end function vector_sum

  function vector_min(v1, v2) result(v3)
   type(vector_t),intent(in) :: v1,v2
   type(vector_t) :: v3

   v3 % x = v1 % x - v2 % x 
   v3 % y = v1 % y - v2 % y
   v3 % z = v1 % z - v2 % z
  end function vector_min
 
  function vector_times(v1, v2) result(v3)
   type(vector_t),intent(in) :: v1,v2
   type(vector_t) :: v3

   v3 % x = v1 % x - v2 % x 
   v3 % y = v1 % y - v2 % y
   v3 % z = v1 % z - v2 % z
  end function vector_times
  
  function vector_abs(v1) result(vmod)
   type(vector_t),intent(in) :: v1
   real :: vmod

   vmod = sqrt((v1 % x)**2 + (v1 % y)**2 + (v1 % z)**2) 
  end function vector_abs
  
end module vector_algebra
