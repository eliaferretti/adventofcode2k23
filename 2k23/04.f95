program main

	use utilities

	implicit none

	! variable declaration
	character(len=300) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, ios, count, first, last, i_w, i_m  &
	                            , len_n, number, points, j, k
	integer 							:: digit1 = 0, digit2 = 0	
	integer							:: lineCount = 0		
	logical 							:: flag, flag_w	
	integer,     dimension(10) :: winning	
	integer,     dimension(25)	:: mine		
	integer,    dimension(204)	:: ist

	fileName = "inputs/input04.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		i = 11
		i_w = 1
		i_m = 1
		flag = .true.
		flag_w = .true.
		
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		print*,"----------------------------------------------------------------"
		print*,"CARD NUMBER:",lineCount

		count = 0
		digit1 = 0
		digit2 = 0
		do while ( flag )
			if ( line(i:i) == "|" ) flag_w = .false.
			if ( flag_w ) then
				if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
					call readNumber_super_short(  line(i:i+2), number, len_n )
					winning(i_w) = number
					!print*,"winning:",number
					i = i + len_n
					i_w = i_w + 1
				end if
			else
				if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
					call readNumber_super_short(  line(i:i+2), number, len_n )
					mine(i_m) = number
					!print*,"mine:",number
					i = i + len_n
					i_m = i_m + 1
				end if
			end if
			i = i + 1
			if ( i > len(trim(line))) flag = .false.
		end do

		points = 0
		do k = 1, 10
			do j = 1, 25	
				if ( mine(j)==winning(k) ) then
					print*,"--> ",mine(j)," is a winning number"
					points = points + 1
					exit
				end if
			end do
		end do
		sum = sum + 2**(points-1)			
		
		write(*,"(A,25(I2,1X))") " winning = ",winning
		write(*,"(A,25(I2,1X))") " mine    = ",mine
		print*,"points  = ",2**(points-1)	
		print*,"sum     = ",sum
		print*,"----------------------------------------------------------------"
		
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 4 - part 1 = ",sum
	print*,"-----------------------------"


	! --- PART 2

	fileName = "inputs/input04.txt"
	open(unit=1,file=fileName, status="old", action="read")

	ist = 1
	lineCount = 0

	do
		i = 11
		i_w = 1
		i_m = 1
		flag = .true.
		flag_w = .true.
		
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		count = 0
		digit1 = 0
		digit2 = 0
		do while ( flag )
			if ( line(i:i) == "|" ) flag_w = .false.
			if ( flag_w ) then
				if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
					call readNumber_super_short(  line(i:i+2), number, len_n )
					winning(i_w) = number
					i = i + len_n
					i_w = i_w + 1
				end if
			else
				if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
					call readNumber_super_short(  line(i:i+2), number, len_n )
					mine(i_m) = number
					i = i + len_n
					i_m = i_m + 1
				end if
			end if
			i = i + 1
			if ( i > len(trim(line))) flag = .false.
		end do

		points = 0
		do k = 1, 10
			do j = 1, 25	
				if ( mine(j)==winning(k) ) then
					points = points + 1
					exit
				end if
			end do
		end do

		do k = 1, points
			if ( lineCount+k > 204 ) exit
			ist(lineCount+k) = ist(lineCount+k) + ist(lineCount)
		end do
	end do

	close(unit=1)
	sum = 0
	do k = 1, 204
		sum = sum + ist(k)
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 4 - part 2 = ",sum
	print*,"-----------------------------"

end program main