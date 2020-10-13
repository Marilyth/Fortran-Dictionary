Module Hashing
    TYPE coordinate
      real :: x
      real :: y
      real :: z
      integer :: index = -1
    end type coordinate
  
    TYPE key_value_pair
      character(len=255) :: key = '-1'
      type(coordinate) :: value
      type(key_value_pair), pointer :: next => null()
    end type key_value_pair
  
    TYPE Dict
      type(key_value_pair), dimension(:), ALLOCATABLE :: hashtable
    end Type Dict
  
    integer :: global_collisions_count = 0
  
    private :: coord_to_string
    private :: hash
    private :: key_exists
    private :: set_size
    private :: delete_linked_list
  
    contains
    function create_dictionary(number_of_elements) result(dictionary)
      integer :: number_of_elements
      type(Dict) :: dictionary
      call set_size(dictionary, int(number_of_elements*1.3))
    end function create_dictionary
  
    function coord_to_string(coord) result(string)
      type(coordinate), value :: coord
      CHARACTER(len=255), ALLOCATABLE :: string
      string = ''
      write(string, *) coord%x, coord%y, coord%z
    end function coord_to_string
  
    function hash(dictionary, string) result(hashVal)
      CHARACTER(len=255) :: string
      type(Dict) :: dictionary
      integer :: hashVal, i
      hashVal = 0
      do i = 1, 255
        hashVal = hashVal + ichar(string(i:i))
        hashVal = hashVal + lshift(hashVal, 10)
        hashVal = IEOR(hashVal, lshift(hashVal, 6))
      end do
      hashVal = hashVal + lshift(hashVal, 3)
      hashVal = IEOR(hashVal, lshift(hashVal, 11))
      hashVal = hashVal + lshift(hashVal, 15)
  
      hashVal = modulo(hashVal, size(dictionary%hashtable)) + 1
    end function hash
  
    subroutine add(dictionary, coord)
      type(coordinate) :: coord
      type(Dict) :: dictionary
      type(key_value_pair) :: base_collision
      type(key_value_pair), pointer :: to_save, i_collision
      logical :: collision, duplicate
      integer :: hashVal, collision_count
  
      if(value_exists(dictionary, coord)) then
        write(*,*) coord_to_string(coord), "already exists at"
        return
      end if
  
      collision = key_exists(dictionary, coord)
      hashVal = hash(dictionary, coord_to_string(coord))
  
      allocate(to_save)
      allocate(to_save%next)
      to_save = key_value_pair(coord_to_string(coord), coord)
  
      if(collision) then
        global_collisions_count = global_collisions_count + 1
        collision_count = 1
        base_collision = dictionary%hashtable(hashVal)
        i_collision => base_collision%next
  
        if(.not. associated(i_collision)) then
          base_collision%next => to_save
          dictionary%hashtable(hashVal) = base_collision
        else
          do
            collision_count = collision_count + 1
            if(associated(i_collision%next)) then
              i_collision => i_collision%next
            else
              exit
            end if
          end do
          
          i_collision%next => to_save
        end if
      else
        dictionary%hashtable(hashVal) = to_save
      end if
    end subroutine add
  
    function get_value(dictionary, coord) result(kvp)
      type(coordinate), value :: coord
      type(Dict) :: dictionary
      type(key_value_pair) :: kvp
      integer :: key
  
      key = hash(dictionary, coord_to_string(coord))
      kvp = dictionary%hashtable(hash(dictionary, coord_to_string(coord)))
      do
        if((kvp%value%x == coord%x .and. kvp%value%y == coord%y .and. kvp%value%z == coord%z) .or. .not. associated(kvp%next)) then
          return
  
        else
          kvp = kvp%next
        end if
      end do
    end function get_value
  
    function key_exists(dictionary, coord) result(bool)
      type(coordinate), value :: coord
      type(Dict) :: dictionary
      logical :: bool
      type(key_value_pair) :: kvp
  
      kvp = dictionary%hashtable(hash(dictionary, coord_to_string(coord)))
      bool = .not. kvp%value%index == -1
    end function key_exists
  
    function value_exists(dictionary, coord) result(bool)
      type(coordinate), value :: coord
      type(Dict) :: dictionary
      logical :: bool
      type(key_value_pair) :: kvp
  
      bool = .FALSE.
  
      if(key_exists(dictionary, coord)) then
        kvp = dictionary%hashtable(hash(dictionary, coord_to_string(coord)))
        do
          bool = kvp%value%x == coord%x .and. kvp%value%y == coord%y .and. kvp%value%z == coord%z
          if((bool .eqv. .TRUE.) .or. (.not. associated(kvp%next))) then
            return
  
          else
            kvp = kvp%next
          end if
        end do
      end if
    end function value_exists
  
    subroutine set_size(dictionary, val)
      integer, value :: val
      type(Dict) :: dictionary
  
      if(allocated(dictionary%hashtable)) then
        DEALLOCATE(dictionary%hashtable)
      end if
  
      ALLOCATE(dictionary%hashtable(val))
    end subroutine set_size
  
    subroutine delete_dictionary(dictionary)
      type(Dict) :: dictionary
      type(key_value_pair), pointer :: cur_kvp
      integer :: i
      do i = 1, size(dictionary%hashtable)
        cur_kvp => dictionary%hashtable(i)%next
        if (associated(cur_kvp)) then
            call delete_linked_list(cur_kvp)
        end if
      end do
      DEALLOCATE(dictionary%hashtable)
    end subroutine delete_dictionary
  
    subroutine delete_linked_list(list_begin)
      type(key_value_pair), pointer  :: list_begin
  
      type(key_value_pair), pointer  :: current
      type(key_value_pair), pointer  :: next
  
      current => list_begin
      do while(associated(current%next))
          next => current%next
          deallocate(current)
          current => next
      end do
    end subroutine delete_linked_list
end module Hashing
