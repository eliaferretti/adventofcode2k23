
module utilities
	
	implicit none

	contains

	subroutine convert2integer( char, int )
		character(len=1), intent(in   ) :: char
		integer , intent(  out) :: int

		select case ( char )

			case ( "1" )
				int = 1
			case ( "2" )
				int = 2
			case ( "3" )
				int = 3
			case ( "4" )
				int = 4
			case ( "5" )
				int = 5
			case ( "6" )
				int = 6
			case ( "7" )
				int = 7
			case ( "8" )
				int = 8
			case ( "9" )
				int = 9
			case default
				int = 0
		end select
	end subroutine

	subroutine checkNums( str, num, flag )

		character(len=100)        , intent(inout)	:: str
		integer                   , intent(inout) :: num
		character(len=20) 								:: fileName
		character(len=100) 								:: line
		integer 												:: lineCount, ios
		logical 												:: flag

		flag = .false.
		fileName = "inputs/nums.txt"
		open(unit=2,file=fileName, status="old", action="read")

		lineCount = 0
		do
			lineCount = lineCount + 1
			read(2,"(A)",iostat=ios) line
			if (ios /= 0) exit

			if ( len(trim(str)) >= len(trim(line)) ) then
				if ( str(1:len(trim(line))) == trim(line) ) then
					num = lineCount
					flag = .true.
					exit
				end if
			end if			
		end do

		close(unit=2)
	end subroutine

	subroutine readNumber( str, number, last_index )

		character(len=7) 				:: str
		integer, intent(  out) 		:: number, last_index
		logical							:: gameFlag
		integer        				:: i, digit, n

		i = 1
		gameFlag = .true.

		do while ( gameFlag )
			if ( str(i:i)>="0" .and. str(i:i)<="9" ) then
				i = i + 1
			else
				gameFlag = .false.
			end if
		end do

		number = 0
		do n = 0, i-1
			call convert2integer( str(n:n), digit )
			number = number + 10**(i-1-n)*digit
		end do

		last_index = i - 1
	end subroutine

	subroutine readNumber_short( str, number, last_index )

		character(len=5) 				:: str
		integer, intent(  out) 		:: number, last_index
		logical							:: gameFlag
		integer        				:: i, digit, n

		i = 1
		gameFlag = .true.

		do while ( gameFlag )
			if ( str(i:i)>="0" .and. str(i:i)<="9" ) then
				i = i + 1
			else
				gameFlag = .false.
			end if
		end do

		number = 0
		do n = 1, i
			call convert2integer( str(n:n), digit )
			number = number + 10**(i-n-1)*digit
		end do

		last_index = i - 1
	end subroutine

	subroutine readNumber_super_short( str, number, last_index )

		character(len=3) 				:: str
		integer, intent(  out) 		:: number, last_index
		logical							:: gameFlag
		integer        				:: i, digit, n

		i = 1
		gameFlag = .true.

		do while ( gameFlag )
			if ( str(i:i)>="0" .and. str(i:i)<="9" ) then
				i = i + 1
			else
				gameFlag = .false.
			end if
		end do

		number = 0
		do n = 1, i
			call convert2integer( str(n:n), digit )
			number = number + 10**(i-n-1)*digit
		end do

		last_index = i - 1
	end subroutine

	subroutine readNumber_long( str, number, last_index )

		character(len=15) 							:: str
		integer(kind=8),         intent(  out) :: number != selected_int_kind(16)
		integer, 				    intent(  out) :: last_index
		logical											:: gameFlag
		integer        								:: i, digit, n, add

		i = 1
		gameFlag = .true.

		do while ( gameFlag )
			if ( str(i:i)>="0" .and. str(i:i)<="9" ) then
				i = i + 1
			else
				gameFlag = .false.
			end if
		end do

		number = 0
		do n = 0, i-1
			call convert2integer( str(n:n), digit )
			add = 10**(i-1-n)*digit
			number = number + int(add, 8)
		end do

		last_index = i - 1
	end subroutine

	subroutine convertCard( char, int )
		character(len=1), intent(in   ) :: char
		integer , intent(  out) :: int

		select case ( char )

			case ( "2" )
				int = 1
			case ( "3" )
				int = 10
			case ( "4" )
				int = 10**2
			case ( "5" )
				int = 10**3
			case ( "6" )
				int = 10**4
			case ( "7" )
				int = 10**5
			case ( "8" )
				int = 10**6
			case ( "9" )
				int = 10**7
			case ( "T" )
				int = 10**8
			case ( "J" )
				int = 10**9
			case ( "Q" )
				int = 10**10
			case ( "K" )
				int = 10**11
			case ( "A" )
				int = 10**12
			case default
				int = 0
		end select
	end subroutine

	subroutine convertCard_norm( char, int )
		character(len=1), intent(in   ) :: char
		integer , intent(  out) :: int

		select case ( char )

			case ( "2" )
				int = 2
			case ( "3" )
				int = 3
			case ( "4" )
				int = 4
			case ( "5" )
				int = 5
			case ( "6" )
				int = 6
			case ( "7" )
				int = 7
			case ( "8" )
				int = 8
			case ( "9" )
				int = 9
			case ( "T" )
				int = 10
			case ( "J" )
				int = 11
			case ( "Q" )
				int = 12
			case ( "K" )
				int = 13
			case ( "A" )
				int = 14
			case default
				int = 0
		end select
	end subroutine

	subroutine sort_indices(arr, indices)
    integer, 			intent(inout) :: arr(:)
    integer, 		   intent(inout) :: indices(:)
    integer 						     :: i, j, temp_index
    integer                        :: temp_value

    do i = 1, size(arr)-1
      do j = i+1, size(arr)
        if (arr(j) > arr(i)) then
          ! Swap array values
          temp_value = arr(i)
          arr(i) = arr(j)
          arr(j) = temp_value
          
          ! Swap corresponding indices
          temp_index = indices(i)
          indices(i) = indices(j)
          indices(j) = temp_index
        end if
      end do
    end do
   end subroutine
    
subroutine sort_indices2(arrI, iI, arr, indices)
    integer, 			intent(in   ) :: arrI(:)
    integer, 		   intent(in   ) :: iI(:)
    integer, 			intent(  out) :: arr(:)
    integer, 		   intent(  out) :: indices(:)
    integer 						     :: i, j, temp_index
    integer                        :: temp_value

    arr = arrI
    indices = iI

    do i = 1, size(arr)-1
      do j = i+1, size(arr)
        if (arr(j) > arr(i)) then
          ! Swap array values
          temp_value = arr(i)
          arr(i) = arr(j)
          arr(j) = temp_value
          
          ! Swap corresponding indices
          temp_index = indices(i)
          indices(i) = indices(j)
          indices(j) = temp_index
        end if
      end do
    end do

end subroutine

subroutine getCards(arr, str)
	integer, 			 intent(in   ) :: arr(:)
	character(len=5), intent(  out) :: str
	integer 						      :: i


	do i = 1, 5
      select case ( arr(i) )
      	case ( 1 )
				str(i:i) = "2"
			case ( 10 )
				str(i:i) = "3"
			case ( 10**2 )
				str(i:i) = "4"
			case ( 10**3 )
				str(i:i) = "5"
			case ( 10**4 )
				str(i:i) = "6"
			case ( 10**5 )
				str(i:i) = "7"
			case ( 10**6 )
				str(i:i) = "8"
			case ( 10**7 )
				str(i:i) = "9"
			case ( 10**8 )
				str(i:i) = "T"
			case ( 10**9 )
				str(i:i) = "J"
			case ( 10**10 )
				str(i:i) = "Q"
			case ( 10**11 )
				str(i:i) = "K"
			case ( 10**12 )
				str(i:i) = "A"
			case default
				str(i:i) = "-"
      end select
   end do
end subroutine

subroutine get_direction(c,d)
	character(len=1), 		intent(in   ) :: c
	integer, 					intent(inout) :: d

	select case ( c )
		case ( "|" )
			d = d 
		case ( "F" )
			if ( d == 0) d = 3 
			if ( d == 1) d = 2
		case ( "7" )
			if ( d == 3) d = 2 
			if ( d == 0) d = 1
		case ( "L" )
		   if ( d == 2) d = 3 
			if ( d == 1) d = 0
	   case ( "-" )
	   	d = d 
   	case ( "J" )
   		if ( d == 2) d = 1 
			if ( d == 3) d = 0
   	case default
   		d = -1
	end select
end subroutine

function raycasting(xa, ya, x, y, npoints) result(isinside)
    integer, intent(in) :: xa, ya
    integer, intent(in) :: x(:), y(:)
    integer, intent(in) :: npoints
    logical :: isinside
    integer :: i, j
    integer :: numintersections

    ! initialize the ray endpoint
    integer :: xray, yray
    xray = 1
    yray = 1

    ! initialize the number of intersections
    numintersections = 0

    ! check each line segment formed by connecting consecutive vertices
    do i = 1, npoints
      j = mod(i, npoints) + 1

      if (((y(i) <= ya .and. ya < y(j)) .or. (y(j) <= ya .and. ya < y(i))) &
          .and. (xa < x(i) + real(ya - y(i)) / real(y(j) - y(i)) * real(x(j) - x(i)))) then
        numintersections = numintersections + 1
      end if
    end do

    ! determine if the point is inside the contour
    isinside = mod(numintersections, 2) == 1

  end function raycasting

end module utilities