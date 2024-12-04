program main

	use utilities

	implicit none

	! variable declaration
	integer,parameter 			:: rows = 6
	character(len=10000)			:: line
	character(len=20019)			:: filez
	character(len=20)				:: fileName
	character(len=1)				:: char
	integer 							:: res = 0
	integer							:: i,j,k, ios, count, n, n1,n2, comma_idx
	integer							:: lineCount = 0	
	integer, allocatable 		:: v(:),v2(:)
	logical							:: flag, flag2, foundFirstNumber,foundSecondNumber, enabled	

	fileName = "inputs/input03.txt"
	open(unit=1,file=fileName, status="old", action="read")

	j = 1
	do i = 1, rows
	  	read(1, '(A)', iostat=ios) line
		if (ios /= 0) exit 
		filez(j:j + len_trim(line) - 1) = line
		j = j + len_trim(line) + 1
	end do
	close(unit=1)

	do i = 1,20019-4
		if (filez(i:i+3) == 'mul(') then
			foundFirstNumber  = .false.
			foundSecondNumber = .false.
			do j = 1, 4
				call is_digit(filez(i+3+j:i+3+j), flag)
				if (.not.(flag) .and. j>1) then
					if ( filez(i+3+j:i+3+j) == ',' ) then
						comma_idx = i+3+j 
						if (j == 2) then
							read(filez(i+4:i+3+j-1), '(I1)', iostat=ios) n1
						elseif (j == 3) then
							read(filez(i+4:i+3+j-1), '(I2)', iostat=ios) n1
						else
							read(filez(i+4:i+3+j-1), '(I3)', iostat=ios) n1
						end if
						foundFirstNumber = .true.
						exit
					end if
				end if
			end do
			if (foundFirstNumber) then
				do j = comma_idx + 1, comma_idx + 4
					call is_digit(filez(j:j), flag)
					if (.not.(flag) .and. j>comma_idx + 1) then
						if (filez(j:j)==')') then
							if (j-(comma_idx+1) == 1) then
								read(filez(comma_idx+1:j-1), '(I1)', iostat=ios) n2
							elseif (j-(comma_idx+1) == 2) then
								read(filez(comma_idx+1:j-1), '(I2)', iostat=ios) n2
							else
								read(filez(comma_idx+1:j-1), '(I3)', iostat=ios) n2
							end if
							foundSecondNumber = .true.
							exit
						end if
					end if
				end do
			end if 

			if (foundFirstNumber .and. foundSecondNumber) res = res + n1*n2
		end if
	end do

   

	print*,""
	print*,"-----------------------------"
	print*,"day 3 - part 1 = ",res
	print*,"-----------------------------"

	enabled = .true.
	res = 0

	do i = 1,20019-4
		if (filez(i:i+6) == "don't()") enabled = .false.
		if (filez(i:i+3) == 'do()') enabled = .true.
		if (enabled) then
			if (filez(i:i+3) == 'mul(') then
				foundFirstNumber  = .false.
				foundSecondNumber = .false.
				do j = 1, 4
					call is_digit(filez(i+3+j:i+3+j), flag)
					if (.not.(flag) .and. j>1) then
						if ( filez(i+3+j:i+3+j) == ',' ) then
							comma_idx = i+3+j 
							if (j == 2) then
								read(filez(i+4:i+3+j-1), '(I1)', iostat=ios) n1
							elseif (j == 3) then
								read(filez(i+4:i+3+j-1), '(I2)', iostat=ios) n1
							else
								read(filez(i+4:i+3+j-1), '(I3)', iostat=ios) n1
							end if
							foundFirstNumber = .true.
							exit
						end if
					end if
				end do
				if (foundFirstNumber) then
					do j = comma_idx + 1, comma_idx + 4
						call is_digit(filez(j:j), flag)
						if (.not.(flag) .and. j>comma_idx + 1) then
							if (filez(j:j)==')') then
								if (j-(comma_idx+1) == 1) then
									read(filez(comma_idx+1:j-1), '(I1)', iostat=ios) n2
								elseif (j-(comma_idx+1) == 2) then
									read(filez(comma_idx+1:j-1), '(I2)', iostat=ios) n2
								else
									read(filez(comma_idx+1:j-1), '(I3)', iostat=ios) n2
								end if
								foundSecondNumber = .true.
								exit
							end if
						end if
					end do
				end if 
				if (foundFirstNumber .and. foundSecondNumber) res = res + n1*n2
			end if
		end if
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 3 - part 2 = ",res
	print*,"-----------------------------"

end program main