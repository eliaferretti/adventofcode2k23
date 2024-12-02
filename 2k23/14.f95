program main

	use utilities

	implicit none

	! variable declaration
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, j, k, ios, last_k = 0, num
	integer							:: lineCount = 0		
	logical 							:: i_did_smt	
	character(len=1), dimension(100,100) :: map, m1, m2, m3, m4		

	fileName = "inputs/input14.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		do i = 1, 100
			map(lineCount,i) = line(i:i)
		end do
	!print*,map(lineCount,:)
	end do
	close(unit=1)

	i_did_smt = .true.

	m1 = map

	do while( i_did_smt )
		i_did_smt = .false.
		do i = 2, 100
	   	do j = 1, 100
	   		if ( m1(i,j)=="O" .and. m1(i-1,j)=="." ) then
	   			m1(i-1,j) = "O"
	   			m1(i,j) = "."
	   			i_did_smt = .true.
	   		end if
   		end do 
		end do 
	end do

	! print*,""
	! print*,"--------------------------------------------------------"
	! print*,""

	do i = 1, 100
		do j = 1, 100
			if ( m1(i,j) == "O") sum = sum + 101 - i
		end do
		!print*,m1(i,:) 
	end do


	print*,""
	print*,"-----------------------------"
	print*,"day 14 - part 1 = ",sum
	print*,"-----------------------------"

	! part 2

	m2 = map
	num = mod(1000000000-1000,17)

	do k = 1, 1000000000

		!print*,"on cycle ", k

		! NORTH
		i_did_smt = .true.
		do while( i_did_smt )
			i_did_smt = .false.
			do i = 2, 100
		   	do j = 1, 100
		   		if ( m2(i,j)=="O" .and. m2(i-1,j)=="." ) then
		   			m2(i-1,j) = "O"
		   			m2(i,j) = "."
		   			i_did_smt = .true.
		   		end if
	   		end do 
			end do 
		end do

		! WEST
		i_did_smt = .true.
		do while( i_did_smt )
			i_did_smt = .false.
			do i = 1, 100
		   	do j = 2, 100
		   		if ( m2(i,j)=="O" .and. m2(i,j-1)=="." ) then
		   			m2(i,j-1) = "O"
		   			m2(i,j) = "."
		   			i_did_smt = .true.
		   		end if
	   		end do 
			end do 
		end do

		! SOUTH
		i_did_smt = .true.
		do while( i_did_smt )
			i_did_smt = .false.
			do i = 1, 99
		   	do j = 1, 100
		   		if ( m2(i,j)=="O" .and. m2(i+1,j)=="." ) then
		   			m2(i+1,j) = "O"
		   			m2(i,j) = "."
		   			i_did_smt = .true.
		   		end if
	   		end do 
			end do 
		end do

		! EAST
		i_did_smt = .true.
		do while( i_did_smt )
			i_did_smt = .false.
			do i = 1, 100
		   	do j = 1, 99
		   		if ( m2(i,j)=="O" .and. m2(i,j+1)=="." ) then
		   			m2(i,j+1) = "O"
		   			m2(i,j) = "."
		   			i_did_smt = .true.
		   		end if
	   		end do 
			end do 
		end do

		! if you're asking yourself "why 1000?"
		! i don't have the answer for you

		if ( k == 1000 ) m3 = m2

		if ( k == 1000 + num ) then
			m4 = m2
			exit
		end if
	end do

	sum = 0

	do i = 1, 100
		do j = 1, 100
			if ( m4(i,j) == "O") sum = sum + 101 - i
		end do
		!print*,m4(i,:) 
	end do



	print*,""
	print*,"-----------------------------"
	print*,"day 14 - part 2 = ",sum
	print*,"-----------------------------"




end program main