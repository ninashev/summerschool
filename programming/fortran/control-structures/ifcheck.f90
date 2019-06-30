program ifcheck
  integer :: i

  write(*,*) 'Enter i:'
  read(*,*) i
  if (i<0) then
     write(*,*) 'i is negative'
  else if (i == 0) then
     write(*,*) 'i is equal to zero'
  else if (i>100) then
     write(*,*) 'i is more than 100'
  else
     write(*,*) 'something other'
  end if
  
end program ifcheck
