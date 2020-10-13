program hash_testing
  use Hashing

  integer :: i, sample_size = 20000
  type(coordinate) :: coord
  type(key_value_pair) :: test
  type(Dict) :: dictionary
  real :: numx, numy, numz

  dictionary = create_dictionary(sample_size)

  do i = 1, sample_size
    call random_number(numx)
    call random_number(numy)
    call random_number(numz)
    coord = coordinate(numx * 10000, numy * 10000, numz * 10000, i)
    if(i == 12345) then
      coord = coordinate(1, 2, 3, 999)
    end if
    call add(dictionary, coord)
  end do

  if(value_exists(dictionary, coordinate(1, 2, 3))) then
    test = get_value(dictionary, coordinate(1, 2, 3))
    write(*,*) "Index of the matched coordinate is: ", test%value%index
  end if

  write(*,*) "Collision percentage: ", (global_collisions_count / real(sample_size)), "~0.3 is good memory/speed ratio"

  call delete_dictionary(dictionary)
end program hash_testing
