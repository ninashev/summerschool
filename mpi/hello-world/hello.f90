program hello
  use mpi

  integer :: rp,ierror,rank,size
  call mpi_init(rp)
  !write(*,*) 'rp = ',rp
  call mpi_comm_rank(mpi_comm_world,rank,ierror)
  call mpi_comm_size(mpi_comm_world,size,ierror)
   
  ! write(*,*) 'Hello',rank,size
  
  if(rank == 0) then
     write(*,*) 'I am the zero', rank,', the size is',size
  else if (rank == 1) then
     write(*,*) 'I am the first',rank
  else if (rank == 2) then
     write(*,*) 'I am the second',rank
  else
     write(*,*) 'I am the last',rank
  end if
  
  
  call mpi_finalize(ierror)
 
end program hello


