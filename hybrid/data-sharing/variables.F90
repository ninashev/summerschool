program exer1
  use mpi_f08
  use omp_lib
  implicit none
  integer :: var1, var2
  integer :: provided, ierr
  
  var1 = 1
  var2 = 2

!$omp parallel default(shared) private(var1,var2)
 ! TODO:                                                                        !   Test different data sharing clauses here                                                      
  print *, 'Region 1:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  ! end here :) 

!$omp end parallel
  
  print *, 'After region 1: var1=', var1, 'var2=', var2
  print *
!--

!$omp parallel default(shared) firstprivate(var1,var2)                                                       
 ! TODO:                                                                       
 !   Test different data sharing clauses here                                                      
  print *, 'Region 2:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  ! end here :)                                                                                     
!$omp end parallel                                                                                 
  print *, 'After region 2: var1=', var1, 'var2=', var2
  print *
!--

!$omp parallel default(shared)                                  
 ! TODO:     
 !   Test different data sharing clauses here                                                      
  print *, 'Region 3:       var1=', var1, 'var2=', var2
  var1 = var1 + 1
  var2 = var2 + 1
  ! end here :)       

!$omp end parallel                                                                                  
  print *, 'After region 3: var1=', var1, 'var2=', var2
  print *
  
  
end program exer1
