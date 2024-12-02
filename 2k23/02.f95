program main

	use utilities

	implicit none

	! variable declaration
	character(len=1000) 			:: line
	character(len=20)				:: fileName
	character(len=1)				:: color
	integer							:: i, n, ios, int, digit        	&
	                            , gameNumber, NN, add_idx     	&
	                            , i_current, number           	&
	                            , green, red, blue				 	&
	                            , max_green, max_blue, max_red	&
	                            , sum = 0, lineCount = 0
	logical 							:: gameFlag, reading_line 			&
										 , possible

	max_red   = 12
	max_green = 13
	max_blue  = 14		

	fileName = "inputs/input02.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		i_current = 6
		call readNumber( line(i_current:i_current+7), gameNumber, add_idx )

		i_current = i_current + add_idx + 2

		reading_line = .true.
		possible = .true.
		red   = 0 
		green = 0
		blue  = 0
		do while( reading_line ) ! same game
			call readNumber( line(i_current:i_current+7), number, add_idx )
			i_current = i_current + add_idx + 1

			color = line(i_current:i_current)

			select case ( color )
				case( "r" )
					red = red + number
					i_current = i_current + 3
				case( "g" )
					green = green + number
					i_current = i_current + 5
				case( "b" )
					blue = blue + number
					i_current = i_current + 4
				case default
					print*,"ERROR! number = ",number,"line() = ",line(i_current-1:i_current+1)
		   end select

		   if ( line(i_current:i_current)==";" .or. i_current > len(trim(line)) ) then
		   	if (red>max_red .or. green>max_green .or. blue>max_blue) then
		   		possible = .false.
		   	end if
		   	red   = 0 
				green = 0
				blue  = 0
		   end if
		   i_current = i_current + 2
		   if ( i_current > len(trim(line)) ) reading_line = .false.
		end do
		if ( possible ) sum = sum + gameNumber

		print*,"# ",gameNumber," - possible =",possible,"sum = ",sum
		
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 2 - part 1 = ",sum
	print*,"-----------------------------"

	! --- PART 2 ---

	fileName = "inputs/input02.txt"
	open(unit=1,file=fileName, status="old", action="read")
	sum = 0
	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit
		i_current = 6
		call readNumber( line(i_current:i_current+7), gameNumber, add_idx )

		i_current = i_current + add_idx + 2

		reading_line = .true.
		possible = .true.
		red   = 0 
		green = 0
		blue  = 0
		do while( reading_line ) ! same game
			call readNumber( line(i_current:i_current+7), number, add_idx )
			i_current = i_current + add_idx + 1

			color = line(i_current:i_current)

			select case ( color )
				case( "r" )
					if ( number>red ) red = number
					i_current = i_current + 3
				case( "g" )
					if ( number>green ) green = number
					i_current = i_current + 5
				case( "b" )
					if ( number>blue ) blue = number
					i_current = i_current + 4
				case default
					print*,"ERROR! number = ",number,"line() = ",line(i_current-1:i_current+1)
		   end select

		   ! if ( line(i_current:i_current)==";" .or. i_current > len(trim(line)) ) then
		   ! 	if (red>max_red .or. green>max_green .or. blue>max_blue) then
		   ! 		possible = .false.
		   ! 	end if
		   ! 	red   = 0 
			! 	green = 0
			! 	blue  = 0
		   ! end if
		   i_current = i_current + 2
		   if ( i_current > len(trim(line)) ) reading_line = .false.
		end do
		sum = sum + red*blue*green

		print*,"# ",gameNumber," - power =",red*blue*green,"sum = ",sum
		
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 2 - part 2 = ",sum
	print*,"-----------------------------"

end program main