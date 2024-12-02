program main

	use utilities

	implicit none

	! variable declaration
	character(len=150) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, j, k, el, ec, ios, count, first, last 
	integer							:: dist_x, dist_y, dist, add 
	integer							:: lineCount = 0		
	logical 							:: flag,isempty		
	character(len=1), dimension(140,140) :: m	
	character(len=1), dimension(147,148) :: g
	integer,          dimension(452,2) :: cor
	integer,          dimension(2) :: minmax_x, minmax_y
	integer,          dimension(7) :: empty_rows
	integer,          dimension(8) :: empty_cols
	fileName = "inputs/input11.txt"
	open(unit=1,file=fileName, status="old", action="read")

	el = 0 
	ec = 0
	count = 0

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		isempty = .true.
		do i = 1, len(trim(line))
			m(lineCount,i) = line(i:i)
			if ( line(i:i)=="#" ) then
				isempty = .false.
				count = count + 1
				cor(count,:) = [lineCount,i]
			end if
		end do	
		if ( isempty ) el = el + 1
		if ( isempty ) print*,"line",lineCount," is empty: ", m(:,i)
	end do
	print*,"galaxies = ",count
	close(unit=1)

	do j = 1, 140
		isempty = .true.
		do i = 1, 140
			if ( m(i,j) == "#" ) isempty = .false.
		end do 
		if ( isempty ) ec = ec + 1
		if ( isempty ) print*,"column",j," is empty: ", m(:,j)
	end do

	g = "."

	el = 0 
	ec = 0
	open(UNIT=1, FILE='outputs/11.txt', STATUS='old', ACTION='write')
	i = 1
	do while ( i <= 147 )
		j = 1
		ec = 0
		do while ( j <= 148 ) 

			g(i,j) = m(i-el,j-ec)

			if (j==29 .or. j==48 .or. j==55 .or. j==61 .or. j==73 .or. j==83 .or. j==93 .or. j==99 ) then
				j = j + 2
				ec = ec + 1
			else
				j = j + 1
			end if
		end do

		write(1,*) g(i,:)

		if (i==31 .or. i==48 .or. i==53 .or. i==58 .or. i==65 .or. i==86 .or. i==88 ) then
			write(1,*) g(i+1,:)
			i = i + 2
			el = el + 1
		else
			i = i + 1
		end if
	end do
	close(1)

	count = 0
	do i = 1, 147
		do j = 1, 148 
			if ( g(i,j)=="#" ) then 
				count = count + 1
				print'(A,I3,A,I3,A,I3,A)',"galaxy #",count," (",i,",",j,")"
				cor(count,:) = [i,j]
			end if 
		end do 
	end do 

	sum = 0
	do i = 1, 452 - 1
		do j = i + 1, 452
			dist_x = abs( cor(i,1) - cor(j,1) )
			dist_y = abs( cor(i,2) - cor(j,2) )
			dist = dist_x + dist_y
			sum = sum + dist
		end do  
	end do


	print*,""
	print*,"-----------------------------"
	print*,"day 11 - part 1 = ",sum
	print*,"-----------------------------"

!  PART 2
	count = 0
	do i = 1, 140
		do j = 1, 140 
			if ( m(i,j)=="#" ) then 
				count = count + 1
				cor(count,:) = [i,j]
			end if 
		end do 
	end do

	empty_rows = [31,47,51,55,61,81,82]
	empty_cols = [29,47,53,58,69,78,87,92]

	sum = 0
	do i = 1, 452 - 1
		do j = i + 1, 452
			add = 0
			minmax_x = [min(cor(i,1),cor(j,1)),max(cor(i,1),cor(j,1))]
			minmax_y = [min(cor(i,2),cor(j,2)),max(cor(i,2),cor(j,2))]
			do k = 1, 7
				if (empty_rows(k)>minmax_x(1) .and. empty_rows(k)<minmax_x(2)) add = add + 999999
			end do
			do k = 1, 8
				if (empty_cols(k)>minmax_y(1) .and. empty_cols(k)<minmax_y(2)) add = add + 999999
			end do
			dist_x = abs( cor(i,1) - cor(j,1) )
			dist_y = abs( cor(i,2) - cor(j,2) )
			dist = dist_x + dist_y
			sum = sum + dist + add
			print'(A,I3,A,I3,A,I3,A,I3,A,I7)',"x = [",minmax_x(1),",",minmax_x(2),"] - y = [",minmax_y(1),",",minmax_y(2),"] - add = ",add
		end do  
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 11 - part 2 = ",sum
	print*,"-----------------------------"


end program main