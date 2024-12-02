program main

	use utilities

	implicit none

	! variable declaration
	integer,parameter 			:: rows = 1000
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: res = 0
	integer							:: i,j,k, ios, count, n
	integer							:: lineCount = 0	
	integer, allocatable 		:: v(:),v2(:)
	logical							:: flag, flag2		

	fileName = "inputs/input02.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do i = 1, rows
	  	read(1, '(A)', iostat=ios) line
		if (ios /= 0) exit 
		call count_integers(line,n)
		allocate(v(n))
		read(line, *) v
		call check_array(v, n, flag)
		if (flag) res = res + 1
		deallocate(v)
		print*,n
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 1 - part 1 = ",res
	print*,"-----------------------------"

	open(unit=2,file=fileName, status="old", action="read")

	res = 0
	do i = 1, rows
	  	read(2, '(A)', iostat=ios) line
		if (ios /= 0) exit 
		call count_integers(line,n)
		allocate(v(n))
		read(line, *) v
		call check_array(v, n, flag)
		if (flag) then
			res = res + 1
		else
			allocate(v2(n-1))
			do k = 1, n
				count = 0
				do j = 1, n
					if (j/=k) then
						count = count + 1
						v2(count) = v(j)
					end if
				end do
				call check_array(v2, n-1, flag2)
				if (flag2) then
					res = res + 1
					exit
				end if
			end do 
			deallocate(v2)
		end if
		deallocate(v)
	end do

	close(unit=2)

	print*,""
	print*,"-----------------------------"
	print*,"day 1 - part 2 = ",res
	print*,"-----------------------------"

end program main