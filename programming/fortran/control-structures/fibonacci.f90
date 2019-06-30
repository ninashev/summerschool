program fibonacci
  integer, parameter :: n=100
  integer :: i, F(0:n-1)

  F(0) = 0
  F(1) = 1

  i = 1
  fcheck = 1
  do while (fcheck < 100)
     F(i+1) = F(i) + F(i-1)
     fcheck = F(i+1)
     i = i + 1
     write(*,*) 'i,F(i) = ',i,F(i)
  end do

end program fibonacci 
