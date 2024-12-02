program main

	use utilities

	implicit none

	! variable declaration
	character(len=150) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, j, k, ios, count, first, last, i_p, j_p	
	integer							:: lineCount = 0, dir, inter		
	logical 							:: flag, isInside			
	character(len=1), dimension(140,140) :: m, m_cp
	integer, dimension(2)      :: start,p
	integer, dimension(13465,2) :: cor

	fileName = "inputs/input10.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		do i = 1, len(trim(line))
			m(lineCount,i) = line(i:i)
			if ( line(i:i)=="S" ) start = [lineCount,i]
		end do	
	end do

	close(unit=1)

	m_cp = " "
	flag = .true.
	dir = 0
	count = 0
	p = start
	do while ( flag )
		print'(A,I3,A,I3,A,A,A,I5,A,I1)',"Currently in (",p(1),", ",p(2),") = ",m(p(1),p(2))," count = ",count," - dir = ", dir
		m_cp(p(1),p(2)) = "X"
		cor(count,1) = 141 - p(1)
		cor(count,2) = p(2)
		count = count + 1
		if ( dir == 0 ) then ! that is Nord
			p(1) = p(1) - 1
			if ( m(p(1),p(2))=="|" .or. m(p(1),p(2))=="7" .or. m(p(1),p(2))=="F" ) then
				call get_direction(m(p(1),p(2)),dir)
			else
				if ( .not.(p(1) == start(1) .and. p(2)==start(2)) ) print'(A,I3,A,I3,A)',"impossibile to go on. Stuck in (",p(1),", ",p(2),")"
			end if
		else if ( dir == 1 ) then ! that is West
			p(2) = p(2) - 1
			if ( m(p(1),p(2))=="-" .or. m(p(1),p(2))=="L" .or. m(p(1),p(2))=="F" ) then
				call get_direction(m(p(1),p(2)),dir)
			else
				if ( .not.(p(1) == start(1) .and. p(2)==start(2)) ) print'(A,I3,A,I3,A)',"impossibile to go on. Stuck in (",p(1),", ",p(2),")"
			end if
		else if ( dir == 2 ) then ! that is South
			p(1) = p(1) + 1
			if ( m(p(1),p(2))=="|" .or. m(p(1),p(2))=="L" .or. m(p(1),p(2))=="J" ) then
				call get_direction(m(p(1),p(2)),dir)
			else
				if ( .not.(p(1) == start(1) .and. p(2)==start(2)) ) print'(A,I3,A,I3,A)',"impossibile to go on. Stuck in (",p(1),", ",p(2),")"
			end if
		else if ( dir == 3 ) then ! that is East
			p(2) = p(2) + 1
			if ( m(p(1),p(2))=="-" .or. m(p(1),p(2))=="7" .or. m(p(1),p(2))=="J" ) then
				call get_direction(m(p(1),p(2)),dir)
			else
				if ( .not.(p(1) == start(1) .and. p(2)==start(2)) ) print'(A,I3,A,I3,A)',"impossibile to go on. Stuck in (",p(1),", ",p(2),")"
			end if
		end if

		if ( p(1) == start(1) .and. p(2)==start(2) ) then
			print*,""
			print'(A,I5)',"SUCCESS! Got back to starting point!!! -> count = ",count
			flag = .false.
			exit
		end if

		!print*,"Currently in (",p(1),", ",p(2),") - count = ",count
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 10 - part 1 = ",count/2
	print*,"-----------------------------"

	open(UNIT=1, FILE='outputs/10.txt', STATUS='old', ACTION='write')
	do i = 1,140
		do j = 1, 140
			line(j:j) = m_cp(i,j)
		end do
		write(1,*) trim(line)
	end do
	close(1)

	count = 0
	do i = 1, 140 
		do j = 1, 140
			if ( m_cp(i,j) == " ") then
				i_p = i 
				j_p = j
				flag = .true.
				inter = 0 
				do while (flag)
					i_p = i_p - 1
					j_p = j_p - 1
					if ( i_p>0 .and. j_p>0) then 
						if ( m_cp(i_p,j_p)=="X" ) inter = inter + 1
					else
						flag = .false.
					end if
				end do
				if ( mod(inter,2) == 1 ) then 
					count = count + 1
					 m_cp(i,j) = "$"
				end if
			end if
		end do 
	end do 

	open(UNIT=1, FILE='outputs/10.txt', STATUS='old', ACTION='write')
	do i = 1,140
		do j = 1, 140
			line(j:j) = m_cp(i,j)
		end do
		write(1,*) trim(line)
	end do
	close(1)

	print*,""
	print*,"-----------------------------"
	print*,"day 10 - part 2 = ",count
	print*,"-----------------------------"

end program main