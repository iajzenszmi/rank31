program rank31_array_editor
  implicit none
  integer, parameter :: R = 31
  integer :: arr(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
  integer :: coords(R)
  character(len=10) :: action
  integer :: val

  arr = 0  ! Initialize array with zeros

  print *, "Rank-31 array initialized with shape [2,2,...]"

  do
     print *, "Choose operation: INSERT / EDIT / DELETE / EXIT"
     read(*,*) action
     select case (trim(adjustl(action)))
     case ("INSERT", "EDIT")
        call get_coords(coords)
        print *, "Enter value:"
        read(*,*) val
        call set_cell(arr, coords, val)
     case ("DELETE")
        call get_coords(coords)
        call set_cell(arr, coords, 0)
     case ("EXIT")
        exit
     case default
        print *, "Invalid choice."
     end select
  end do

contains

  subroutine get_coords(coords)
    integer, intent(out) :: coords(R)
    integer :: i
    print *, "Enter ", R, " coordinates (1 or 2):"
    do i = 1, R
       read(*,*) coords(i)
       if (coords(i) /= 1 .and. coords(i) /= 2) then
          print *, "Only 1 or 2 allowed. Setting to 1."
          coords(i) = 1
       end if
    end do
  end subroutine get_coords

  subroutine set_cell(arr, idx, value)
    integer, intent(inout) :: arr(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
    integer, intent(in) :: idx(R)
    integer, intent(in) :: value
    arr(idx(1), idx(2), idx(3), idx(4), idx(5), idx(6), idx(7), idx(8), idx(9), idx(10), &
        idx(11), idx(12), idx(13), idx(14), idx(15), idx(16), idx(17), idx(18), idx(19), idx(20), &
        idx(21), idx(22), idx(23), idx(24), idx(25), idx(26), idx(27), idx(28), idx(29), idx(30), idx(31)) = value
  end subroutine set_cell

end program rank31_array_editor

