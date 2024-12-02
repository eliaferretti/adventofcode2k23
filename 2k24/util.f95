
module utilities
	
	implicit none

	contains

	subroutine sort_array(arr, n)
		implicit none
		integer, intent(inout) :: arr(:)   ! Array to be sorted (in-place)
		integer, intent(in) :: n           ! Number of elements in the array
		integer :: i, j, temp

		! Bubble Sort Algorithm
		do i = 1, n - 1
			do j = 1, n - i
				if (arr(j) > arr(j + 1)) then
					temp = arr(j)
					arr(j) = arr(j + 1)
					arr(j + 1) = temp
				end if
			end do
		end do
	end subroutine sort_array

	subroutine count_occurrences(array, size, target, count)
		implicit none
		integer, intent(in) :: array(:)
		integer, intent(in) :: size
		integer, intent(in) :: target
		integer, intent(out) :: count
		integer :: i

		count = 0

		do i = 1, size
			if (array(i) == target) then
				count = count + 1
			end if
		end do
	end subroutine count_occurrences

	subroutine check_array(array, size, is_valid)
		implicit none
		integer, intent(in) 		:: array(:)
		integer, intent(in) 		:: size
		logical, intent(out) 	:: is_valid
		integer 						:: i
		logical 						:: increasing, decreasing

		increasing 	= .true.
		decreasing 	= .true.
		is_valid 	= .true.

		do i = 1, size - 1
			if (array(i+1) > array(i)) then
				decreasing = .false.
			else if (array(i+1) < array(i)) then
				increasing = .false.
			end if

			if (abs(array(i+1) - array(i)) < 1 .or. abs(array(i+1) - array(i)) > 3) then
				is_valid = .false.
				return
			end if
		end do
		is_valid = is_valid .and. (increasing .or. decreasing)
	end subroutine check_array

	subroutine count_integers(line,count)
		implicit none
		character(len=100), intent(in   ) :: line
		integer 				, intent(  out) :: count
		integer :: i

		count = 0
		do i = 1, len(trim(adjustl(line)))
			if (line(i:i) .eq. ' ') then
				count = count + 1
			end if
		end do
		count = count + 1  ! Add one for the last number on the line
	end subroutine count_integers



end module utilities