program vectorsum
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

  sum = 0

!$omp parallel shared(vecA) private(i,psum)
  psum = 0
 !$omp do
  do i = 1, nx
     psum = psum + vecA(i)
  end do
 !$omp end do
 !$omp critical(dosum)
  !  here 'dosum' is just a name of this critical part,
  !  can be used in other places with the same or different name
  sum = sum + psum
 !$omp end critical(dosum)
!$omp end parallel
  write(*,*) 'sum: ', sum
  
end program vectorsum
