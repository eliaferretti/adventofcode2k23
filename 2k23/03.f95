program main

	use utilities

	implicit none

	! variable declaration
	character(len=200) 			:: line
	character(len=20)				:: fileName
	character(len=5)				:: str
	integer							:: i, j, k, ios, count, first, last, len_n &
										 , number, lineCount = 0, next_j, counter  &
										 , sum = 0, gear = 0, prod, save1, save2
	logical 							:: flag, valid, flag1, flag2
							
	character(len=1), dimension(140+2,140+2) :: matrix
	integer, dimension(140+2,140+2) :: matrix_n

	integer, 			 dimension(4) :: mul_n


	fileName = "inputs/input03.txt"
	open(unit=1,file=fileName, status="old", action="read")
	
	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		do j = 1, 140
			matrix(lineCount+1,j+1) = line(j:j)
		end do
	end do

	close(unit=1)

	matrix(1  ,: )  = "."
	matrix(142,: )  = "."
	matrix(:  ,1 )  = "."
	matrix(:  ,142) = "."

	do i = 1, 142
		flag = .true.
		do j = 1, 142
			len_n = 0
			!print*,"counter =",counter," - flag = ",flag
			if (flag) then
				if ( matrix(i,j)>="0" .and. matrix(i,j)<="9"  ) then
					valid = .false.
					str = matrix(i,j)//matrix(i,j+1)//matrix(i,j+2)//matrix(i,j+3)//matrix(i,j+4)
					call readNumber_short(  str, number, len_n )
					do k = j-1, j + len_n
						if ( .not.(matrix(i-1,k)>="0" .and. matrix(i-1,k)<="9") .and. matrix(i-1,k)/="." ) valid = .true.
						if ( .not.(matrix(i+1,k)>="0" .and. matrix(i+1,k)<="9") .and. matrix(i+1,k)/="." ) valid = .true.
					end do
					if ( .not.(matrix(i,j-1)>="0"     .and. matrix(i,j-1)<="9")     .and. matrix(i,j-1)/="." ) valid = .true.
					if ( .not.(matrix(i,j+len_n)>="0" .and. matrix(i,j+len_n)<="9") .and. matrix(i,j+len_n)/="." ) valid = .true.
					if ( .not.(valid) ) then
						do k = j, j + len_n - 1
						   matrix(i,k) = "."
						end do
					end if
				end if
				counter = len_n
			end if
			if ( counter>0 ) then
				flag = .false.
				counter = counter - 1
			else
				flag = .true.
			end if
		end do 
	end do

	do i = 1, 142
		flag = .true.
		do j = 1, 142
			len_n = 0
			!print*,"counter =",counter," - flag = ",flag
			if (flag) then
				if ( matrix(i,j)>="0" .and. matrix(i,j)<="9"  ) then
					valid = .false.
					str = matrix(i,j)//matrix(i,j+1)//matrix(i,j+2)//matrix(i,j+3)//matrix(i,j+4)
					call readNumber_short(  str, number, len_n )
					sum = sum + number
				end if
				counter = len_n
			end if
			if ( counter>0 ) then
				flag = .false.
				counter = counter - 1
			else
				flag = .true.
			end if
		end do 
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 3 - part 1 = ",sum
	print*,"-----------------------------"

	! --- PART 2

	fileName = "inputs/input03.txt"
	open(unit=2,file=fileName, status="old", action="read")
	lineCount = 0
	do
		lineCount = lineCount + 1
		read(2,"(A)",iostat=ios) line
		if (ios /= 0) exit
		do j = 1, 140
			matrix(lineCount+1,j+1) = line(j:j)
		end do
	end do

	close(unit=2)

	matrix(1  ,: )  = "."
	matrix(142,: )  = "."
	matrix(:  ,1 )  = "."
	matrix(:  ,142) = "."

	do i = 1, 142
		flag = .true.
		do j = 1, 142
			len_n = 0
			!print*,"counter =",counter," - flag = ",flag
			if (flag) then
				if ( matrix(i,j)>="0" .and. matrix(i,j)<="9"  ) then
					valid = .false.
					str = matrix(i,j)//matrix(i,j+1)//matrix(i,j+2)//matrix(i,j+3)//matrix(i,j+4)
					call readNumber_short(  str, number, len_n )
					do k = j-1, j + len_n
						if ( matrix(i-1,k)=="*" )  valid = .true.
						if ( matrix(i+1,k)=="*" )  valid = .true.
					end do
					if ( matrix(i,j-1)=="*" )     valid = .true.
					if ( matrix(i,j+len_n)=="*" ) valid = .true.
					if ( .not.(valid) ) then
						do k = j, j + len_n - 1
						   matrix(i,k) = "."
						end do
					else
						matrix_n(i,j:j+len_n-1) = number
					end if
				end if
				counter = len_n
			end if
			if ( counter>0 ) then
				flag = .false.
				counter = counter - 1
			else
				flag = .true.
			end if
		end do 
	end do

	sum = 0

	do i = 1, 142
		do j = 1, 142
			counter = 0
			if ( matrix(i,j)=="*" ) then
				prod = 1
				mul_n = 0
				flag1 = .true.
				flag2 = .true.
				save1 = 0 
				save2 = 0
				do k = j-1, j+1
					if ( matrix(i-1,k)>="0" .and. matrix(i-1,k)<="9" .and. (flag1 .or. matrix_n(i-1,k)/=save1) )  then
						if (save1 == 0) then
							mul_n(1) = matrix_n(i-1,k)
						else
							mul_n(2) = matrix_n(i-1,k)
						end if
						counter = counter + 1
						save1 = matrix_n(i-1,k)
						flag1 = .false.
					end if
					if ( matrix(i+1,k)>="0" .and. matrix(i+1,k)<="9" .and. (flag2 .or. matrix_n(i+1,k)/=save2) ) then
						if (save2 == 0) then
							mul_n(2) = matrix_n(i+1,k)
						else
							mul_n(1) = matrix_n(i+1,k)
						end if
						counter = counter + 1
						save2 = matrix_n(i+1,k)
						flag2 = .false.						
					end if
				end do
				if ( matrix(i,j-1)>="0" .and. matrix(i,j-1)<="9") then
					counter = counter + 1
					mul_n(3) = matrix_n(i,j-1)
				end if
				if ( matrix(i,j+1)>="0" .and. matrix(i,j+1)<="9") then
					counter = counter + 1
					mul_n(4) = matrix_n(i,j+1)
				end if
				if ( counter >= 2) then
					do k = 1,4
						if ( mul_n(k)>0 ) prod = prod * mul_n(k)
					end do
					sum = sum + prod
				end if
			end if

		end do 
	end do
	print*,""
	print*,"-----------------------------"
	print*,"day 3 - part 2 = ",sum
	print*,"-----------------------------"

end program main