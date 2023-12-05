
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

end module utilities