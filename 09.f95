program main

	use utilities

	implicit none

	! variable declaration
	character(len=250) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer(kind=8)				:: number, res
	integer							:: i, j, k, len_n, lineCount, ios, last_i, last_j
	logical 							:: flag, is_negative		
	integer(kind=8), dimension(200,21) :: seq
	integer(kind=8), dimension(200,21,21) :: delta
	integer(kind=8), dimension(200) :: new

	fileName = "inputs/input09.txt"
	open(unit=1,file=fileName, status="old", action="read")
	lineCount = 0
	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		flag = .true.
		j = 1
		i = 1
		do while ( flag )
			is_negative = .false.
			if ( line(i:i) == "-" ) then
				i = i + 1
				is_negative = .true.
			end if
			if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
				call readNumber_long(  line(i:i+12), number, len_n )
				if ( is_negative ) number = - number
				seq(lineCount,j) = number
				print'(A,I3,A,I2,A,I10)',"i #",lineCount," - j #",j," = ",number
				i = i + len_n
				j = j + 1
			end if
			i = i + 1
			if ( i > len(trim(line))) flag = .false.
		end do	
	end do

	close(unit=1)

	flag = .true.

	delta = 0
	delta(:,1,:) = seq
	res = 0
	new = 0
	do k = 1, 200
		do i = 2, 21
			do j = 1, 22-i
				delta(k,i,j) = delta(k,i-1,j+1) - delta(k,i-1,j)
				!print'(A,I2,A,I2,A,I10)',"line = ",i," - col = ",j," - d = ",delta(k,i,j)
				last_j = j + 1
			end do
			sum = 0
			do j = 1,21
				sum = sum + abs(delta(k,i,j))
			end do
			if ( sum == 0 ) then
				last_i = i - 1
				exit
			end if 
		end do

		do i = last_i,1,-1
			new(k) = new(k) + delta(k,i,last_j)
			last_j = last_j + 1
		end do
		res = res + new(k)
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 9 - part 1 = ",res
	print*,"-----------------------------"

! PART 2

	res = 0
	new = 0
	do k = 1, 200
		do i = 2, 21
			do j = 1, 22-i
				delta(k,i,j) = delta(k,i-1,j+1) - delta(k,i-1,j)
				!print'(A,I2,A,I2,A,I10)',"line = ",i," - col = ",j," - d = ",delta(k,i,j)
				last_j = j + 1
			end do
			sum = 0
			do j = 1,21
				sum = sum + abs(delta(k,i,j))
			end do
			if ( sum == 0 ) then
				last_i = i - 1
				exit
			end if 
		end do

		do i = last_i,1,-1
			new(k) = delta(k,i,1) - new(k)
		end do
		res = res + new(k)
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 9 - part 2 = ",res
	print*,"-----------------------------"

end program main