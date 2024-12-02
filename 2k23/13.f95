program main

	use utilities

	implicit none

	! variable declaration
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, j, k, ios, count, idx, l, num, add_idx
	integer 							:: digit1 = 0, digit2 = 0	
	integer							:: lineCount = 0, mapsCount = 1, last_lineCount = 0		
	logical 							:: flag, flag_rfl
	character(len=1),    allocatable :: m(:,:)	
	character(len=1),		allocatable :: column(:), row(:)	
	integer,       dimension(1000,2) :: sizes	= -1

	fileName = "inputs/input13.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) then
			sizes(mapsCount,2) = lineCount - 1 - last_lineCount	
			exit
		end if		

		if ( line=="" ) then
			sizes(mapsCount,2) = lineCount - 1 - last_lineCount
			last_lineCount = lineCount + 1
			mapsCount = mapsCount + 1
			lineCount = lineCount + 1
		else
			sizes(mapsCount,1) = len(trim(line))
		end if
	end do

	close(unit=1)

	do i = 1, 1000
		if ( sizes(i,1)==-1 ) exit
		print*,"#",i,"x = ",sizes(i,1)," - y = ",sizes(i,2)
	end do

	print*,"--------------------------------------------"	
	sum = 0
	open(unit=1,file=fileName, status="old", action="read")	
	do k = 1, 100
		allocate( m(sizes(k,2),sizes(k,1)) )
		allocate( column (sizes(k,2))      )
		allocate( row    (sizes(k,1))		  )
		print*,"Reading map #",k	
		do i = 1, sizes(k,2) 	
			read(1,"(A)",iostat=ios) line
			if (ios /= 0) exit
			if (line=="") exit
			do j = 1, sizes(k,1)
				m(i,j) = line(j:j)
			end do
			print*,m(i,:)	
		end do
		do i = 1, sizes(k,2)-1
			if ( all(m(i,:)==m(i+1,:)) ) then
				flag = .true.; flag_rfl = .true.; count = 1
				if ( i == 1 .or. i == sizes(k,2)-1 ) flag = .false.
				do while ( flag )
					flag_rfl = flag_rfl .and. ( all(m(i-count,:)==m(i+1+count,:)) )
					count = count + 1
					if ( i - count < 1 .or. i + 1 + count > sizes(k,2) .or. .not.(flag_rfl) ) flag = .false.
				end do
				if ( flag_rfl ) then
					sum = sum + i*100	
					print*,"found ROW symmetry - #",i
				end if		
			end if	
		end do
		do j = 1, sizes(k,1)-1	
			if (all(m(:,j)==m(:,j+1))) then
				flag = .true.; flag_rfl = .true.; count = 1
				if ( j == 1 .or. j == sizes(k,1)-1 ) flag = .false.
				do while ( flag )
					flag_rfl = flag_rfl .and. ( all(m(:,j-count)==m(:,j+1+count)) )
					count = count + 1
					if ( j - count < 1 .or. j + 1 + count > sizes(k,1) .or. .not.(flag_rfl) ) flag = .false.
				end do
				if ( flag_rfl ) then
					sum = sum + j	
					print*,"found COLUMN symmetry - #",j
				end if
			end if	
		end do
		read(1,"(A)",iostat=ios) line
		print*,""
		print*,"sum = ",sum
		print*,"--------------------------------------------"
		print*,""

		deallocate( m )
		deallocate( column )
		deallocate( row )
	end do
	close(1)

	print*,""
	print*,"-----------------------------"
	print*,"day 13 - part 1 = ",sum
	print*,"-----------------------------"

end program main