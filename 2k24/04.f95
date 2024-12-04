program main

	use utilities

	implicit none

	! variable declaration
	integer,parameter 							:: rows = 140
	character(len=rows)							:: line
	character(len=1), dimension(rows,rows) :: charMatrix
	character(len=20)								:: fileName
	character(len=4)								:: xmas
	character(len=3)								:: diag1, diag2
	integer 											:: res = 0
	integer											:: i,j,k, ios, count, n, n1,n2, comma_idx
	logical											:: flag, flag2, foundFirstNumber,foundSecondNumber, enabled	

	fileName = "inputs/input04.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do i = 1, rows
	  	read(1, '(A)', iostat=ios) line
		if (ios /= 0) exit 
		do j = 1, rows
			charMatrix(i,j) = line(j:j)
		end do
	end do
	close(unit=1)

	! HORIZONTAL WORDS
	do i = 1,rows
		do j = 1,rows-3
			xmas = charMatrix(i  ,j  ) //			&
					 charMatrix(i  ,j+1) //			&
					 charMatrix(i  ,j+2) //			&
					 charMatrix(i  ,j+3)
			if ( xmas == 'XMAS' ) res = res + 1
			if ( xmas == 'SAMX' ) res = res + 1
		end do
	end do

	! VERTICAL WORDS
	do i = 1,rows-3
		do j = 1,rows
			xmas = charMatrix(i  ,j  ) //			&
					 charMatrix(i+1,j  ) //			&
					 charMatrix(i+2,j  ) //			&
					 charMatrix(i+3,j  )
			if ( xmas == 'XMAS' ) res = res + 1
			if ( xmas == 'SAMX' ) res = res + 1
		end do
	end do

	! RIGHT DIAGONAL WORDS
	do i = 1,rows-3
		do j = 1,rows-3
			xmas = charMatrix(i  ,j  ) //			&
					 charMatrix(i+1,j+1) //			&
					 charMatrix(i+2,j+2) //			&
					 charMatrix(i+3,j+3)
			if ( xmas == 'XMAS' ) res = res + 1
			if ( xmas == 'SAMX' ) res = res + 1
		end do
	end do

	! LEFT DIAGONAL WORDS
	do i = 1,rows-3
		do j = 4,rows
			xmas = charMatrix(i  ,j  ) //			&
					 charMatrix(i+1,j-1) //			&
					 charMatrix(i+2,j-2) //			&
					 charMatrix(i+3,j-3)
			if ( xmas == 'XMAS' ) res = res + 1
			if ( xmas == 'SAMX' ) res = res + 1
		end do
	end do


	print*,""
	print*,"-----------------------------"
	print*,"day 4 - part 1 = ",res
	print*,"-----------------------------"

	res = 0 

	do i = 1,rows-2
		do j = 1,rows-2

			diag1 = charMatrix(i  ,j  ) //			&
					  charMatrix(i+1,j+1) //			&
					  charMatrix(i+2,j+2)
			diag2 = charMatrix(i  ,j+2) //			&
					  charMatrix(i+1,j+1) //			&
					  charMatrix(i+2,j  )

			if ( (diag1 == 'MAS' .or. diag1 == 'SAM') .and. (diag2 == 'MAS' .or. diag2 == 'SAM') ) res = res + 1
		end do
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 4 - part 2 = ",res
	print*,"-----------------------------"

end program main