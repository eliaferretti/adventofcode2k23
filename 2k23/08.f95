program main

	use utilities

	implicit none

	! variable declaration
	character(len=100) 			:: line
	character(len=30)				:: fileName
	integer 							:: sum = 0
	integer							:: i, j, k, ios, first, last 
	integer(kind=8)				:: count
	integer 							:: current_line, countA
	integer							:: lineCount = 0		
	logical 							:: flag, part2
	integer, dimension(6) :: cpl
	integer, dimension(798,2) :: line_lr
	character(len=3) 				:: currentPoint
	character(len=3),dimension(6) 				:: cp2
	character(len=2),dimension(6) 				:: f2			
	character(len=283) :: inst
	character(len=3), dimension(798,3) :: pp	

	fileName = "inputs/input08.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		if ( lineCount == 1) inst(1:100) = line
		if ( lineCount == 2) inst(101:200) = line
		if ( lineCount == 3) inst(201:283) = line
		if ( lineCount > 4 ) then
			pp(lineCount-4,1) = line(1:3)
			pp(lineCount-4,2) = line(8:10)
			pp(lineCount-4,3) = line(13:15)
		end if
		print'(A,1X,A,1X,A)',pp(lineCount-2,:)
	end do
	currentPoint = "AAA"
	current_line = 468
	count = 0
	flag = .true.
	do while ( flag )
		do i = 1, 283
			if (inst(i:i)=="L") then
				count = count + 1
				currentPoint = pp(current_line,2)
				do j = 1, 798
					if ( pp(j,1)==currentPoint ) current_line = j
				end do
				if (currentPoint=="ZZZ") then
					flag = .false.
					exit
				end if
			else ! that is "R"
				count = count + 1
				currentPoint = pp(current_line,3)
				do j = 1, 798
					if ( pp(j,1)==currentPoint ) current_line = j
				end do
				if (currentPoint=="ZZZ") then
					flag = .false.
					exit
				end if
			end if 
			print*,"count = ",count," - currentPoint ",pp(current_line,1),"- inst:",inst(i:i)," - avail:",pp(current_line,2:3)
		end do
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 8 - part 1 = ",count
	print*,"-----------------------------"

	! part2

	countA = 0
	do i = 1, 798
		currentPoint = pp(i,1)
		if ( currentPoint(3:3) == "A" ) then 
			countA = countA + 1
			cp2(countA) = currentPoint
			cpl(countA) = i
			f2(countA) = currentPoint(1:2)
		end if
	end do

	line_lr = 1000000

	do i = 1, 798
		do j = 1, 798
			if ( pp(i,2)==pp(j,1) ) then
				line_lr(i,1) = j
			end if
			if ( pp(i,3)==pp(j,1) ) then
				line_lr(i,2) = j
			end if
		end do
		print*,"i = ",i," - L = ",line_lr(i,1)," - R = ",line_lr(i,2)
	end do

	! next part part would take forever to reach the solution (this is why part2 = .false.)
	! However, it provides the cycles at which every starting nodes reaches the "Z".
	! The length of the cycle is then extracted from the
	! output file and the LCM is computed with wolfram alpha:
	! https://www.wolframalpha.com/input?i=LCM+20659+20093+14999+17263+22357+16697
	! of course one could extract the duration of each cycle here and after factorization extract the 
	! answer without any external help.

	part2 = .false.
	if ( part2 ) then
		count = 0
		flag = .true.
		do while ( flag )
			do i = 1, 283
				if (inst(i:i)=="L") then
					do k = 1, 6
						cp2(k) = pp(cpl(k),2)
						cpl(k) = line_lr(cpl(k),1)
					end do
				else ! that is "R"
					do k = 1, 6
						cp2(k) = pp(cpl(k),3)
						cpl(k) = line_lr(cpl(k),2)
					end do
				end if 
				count = count + 1
				print'(A,I12)',"count = ",count

				do k = 1, 6
					if ( cp2(k)(3:3)=="Z" ) then
						fileName = 'outputs/08.txt'
						open(UNIT=1, FILE=fileName, STATUS='old', ACTION='write',POSITION='append')
						write(1, '(1X,I16,1X,I2)', advance = 'no') count, k
						close(1)
					end if
				end do

				if (cp2(1)(3:3)=="Z" .and. cp2(2)(3:3)=="Z" .and. cp2(3)(3:3)=="Z" .and. & 
					 cp2(4)(3:3)=="Z" .and. cp2(5)(3:3)=="Z" .and. cp2(6)(3:3)=="Z") then
					flag = .false.
					exit
				end if
			end do
		end do
	end if

	count = 22103062509257

	print*,""
	print*,"-----------------------------"
	print*,"day 8 - part 2 = ",count
	print*,"-----------------------------"

end program main