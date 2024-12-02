program main

	use utilities

	implicit none

	! variable declaration
	integer, parameter 			:: n = 1000
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: res = 0
	integer							:: i, ios, count
	integer							:: lineCount = 0	
	integer 							:: v1(n), v2(n)	
	logical 							:: flag			

	fileName = "inputs/input01.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do i = 1, n
	  	read(1, '(A)', iostat=ios) line
		if (ios /= 0) exit 
		read(line, *) v1(i), v2(i)
	end do

	close(unit=1)

	call sort_array(v1, n)
	call sort_array(v2, n)

	do i = 1, n
      print "(I4, 1X, I12, 1X, I12, 1X, I12)", i, v1(i), v2(i), abs(v1(i)-v2(i))
	  	res = res + abs(v1(i)-v2(i))
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 1 - part 1 = ",res
	print*,"-----------------------------"

	res = 0 
	do i = 1, n
		call count_occurrences(v2, n, v1(i), count)
		res = res + v1(i) * count
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 1 - part 2 = ",res
	print*,"-----------------------------"

end program main