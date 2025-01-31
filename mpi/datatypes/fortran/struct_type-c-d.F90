program datatype_struct
  use mpi_f08
  use iso_fortran_env, only : real64
  implicit none

  type particle
     real :: coords(3)
     integer :: charge
     character(len=2) :: label
  end type particle

  integer, parameter :: n = 1000
  integer :: i, ierror,  myid,  ntasks
  type(particle) :: particles(n)

  integer, parameter :: cnt = 3
  type(mpi_datatype) :: particle_mpi_type, temp_type, types(cnt)
  integer :: blocklen(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: disp(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: lb, extent
  real(real64) :: t1, t2


  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)

  ! insert some data for the particle struct
  if(myid == 0) then
     do i = 1, n
        call random_number(particles(i)%coords)
        particles(i)%charge = 54
        particles(i)%label = 'Xe'
     end do
  end if

     call mpi_get_address(particles(1)%coords,disp(1),ierror)
     call mpi_get_address(particles(1)%charge,disp(2),ierror)
     call mpi_get_address(particles(1)%label,disp(3),ierror)
     
   blocklen = [3,1,2]
    disp(3) = disp(3) - disp(1)
    disp(2) = disp(2) - disp(1)
    disp(1) = 0
   types = [mpi_real,mpi_integer,mpi_character]

  ! TODO: define the datatype for type particle
  call mpi_type_create_struct(cnt,blocklen,disp,types,temp_type,ierror)
  call mpi_type_commit(temp_type,ierror)

  !--call mpi_type_create_subarray(ndims,sizes,subsizes,offsets,order, &
  !--     mpi_integer,temp_type,ierror)
  
  ! TODO: Check extent.
  ! (Not really neccessary on most systems.)
  call mpi_type_get_extent(temp_type,lb,extent)

  ! TODO: resize the particle_mpi_type if needed
  call mpi_get_address(particles(1),disp(1),ierror)
  call mpi_get_address(particles(2),disp(2),ierror)

  if(extent /= disp(2) - disp(1)) then
   write(*,*) 'disp(2) - disp(1) = ',disp(2) - disp(1)
     ! TODO: resize the particle_mpi_type if needed
  end if

  t1 = MPI_WTIME()
  if(myid == 0) then
     do i = 1, 1000
!       call MPI_SEND(particles, n, particle_mpi_type, 1, i, &
!            & MPI_COMM_WORLD, ierror)
    call MPI_SEND(particles,n,temp_type,1,i,MPI_COMM_WORLD,ierror)
     end do
  else if(myid == 1) then
     do i = 1, 1000
!        call MPI_RECV(particles, n, particle_mpi_type, 0, i, &
!             & MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
    call MPI_RECV(particles,n,temp_type,0,i,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
     end do
  end if
  t2 = MPI_WTIME()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

 ! call MPI_TYPE_free(particle_mpi_type, ierror)
  call MPI_TYPE_free(temp_type, ierror)
  call MPI_FINALIZE(ierror)

end program datatype_struct
