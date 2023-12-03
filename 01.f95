program main

	use utilities

	implicit none

	! variable declaration
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, ios, count, first, last 
	integer 							:: digit1 = 0, digit2 = 0	
	integer							:: lineCount = 0		
	logical 							:: flag			

	fileName = "inputs/input01.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		count = 0
		digit1 = 0
		digit2 = 0
		do i = 1, len(trim(line))
			if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
				if ( count == 0 ) then
					call convert2integer(line(i:i),digit1)
				else
					call convert2integer(line(i:i),digit2)
				end if
				count = count + 1
			else
				if ( count == 0 ) then
					call checkNums( line(i:len(trim(line))), digit1, flag )
					if ( flag ) count = count + 1
				else
					call checkNums( line(i:len(trim(line))), digit2, flag )
					count = count + 1
				end if		
			end if
			
			if ( digit2==0 ) digit2 = digit1
		end do

		sum = sum + digit1*10 + digit2
		
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 1 - part 2 = ",sum
	print*,"-----------------------------"

end program main