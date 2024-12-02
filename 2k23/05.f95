program main

	use utilities

	implicit none

	! variable declaration
	character(len=300) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, ios, count, first, last, i_s, i_m  &
	                            , len_n, points, j, k, i_conv
	integer 							:: digit1 = 0, digit2 = 0	
	integer							:: lineCount = 0		
	logical 							:: flag, flag_w, finished, flag_s	
	integer(kind=8)				:: number, dest, src, rng, connection, min
	integer(kind=8),     dimension(20  ) :: seeds
	integer(kind=8),     dimension(10  ) :: seeds2
	integer(kind=8),     dimension(10  ) :: s_rng
	integer(kind=8),		dimension(20,8) :: con
	integer(kind=8),     dimension(31,3) :: soil
	integer(kind=8),     dimension(47,3) :: fert
	integer(kind=8),     dimension(45,3) :: water
	integer(kind=8),     dimension(43,3) :: light
	integer(kind=8),     dimension(20,3) :: temp
	integer(kind=8),     dimension(17,3) :: hum
	integer(kind=8),     dimension(18,3) :: loc
	integer,     dimension(25)	:: mine		
	integer,    dimension(204)	:: ist

	fileName = "inputs/input05.txt"
	open(unit=1,file=fileName, status="old", action="read")
	seeds = 0
	flag = .true.
	finished = .false.
	i_conv = 1

	do
		i = 8
		i_s = 1
			
		flag_w = .true.
		
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		do while ( flag )
			if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
				call readNumber_long(  line(i:i+12), number, len_n )
				seeds(i_s) = number
				print'(A,I2,A,I10)',"seed #",i_s," = ",number
				i = i + len_n
				i_s = i_s + 1
			end if
			i = i + 1
			if ( i > len(trim(line))) flag = .false.
			con(:,1) = seeds
			con(:,2) = seeds
		end do
		
		if ( line(1:4)=="seed" .and. lineCount>1 )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do

			con(:,i_conv+1) = con(:,i_conv)
		end if

		if ( line(1:4)=="soil" )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do

			con(:,i_conv+1) = con(:,i_conv)
		end if

		if ( line(1:10)=="fertilizer" )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do

			con(:,i_conv+1) = con(:,i_conv)
		end if

		if ( line(1:5)=="water" )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do

			con(:,i_conv+1) = con(:,i_conv)
		end if

		if ( line(1:5)=="light" )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do

			con(:,i_conv+1) = con(:,i_conv)
		end if

		if ( line(1:11)=="temperature" )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do

			con(:,i_conv+1) = con(:,i_conv)
		end if

		if ( line(1:8)=="humidity" )then
			i_conv = i_conv + 1
			flag_w = .true.
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					do i = 1, 20
						if ( con(i,i_conv-1) >= src .and. con(i,i_conv-1) < src + rng ) con(i,i_conv) = dest + con(i,i_conv) - src
					end do
				else
					flag_w = .false.
				end if
			end do
		end if
	end do
	close(unit=1)

	print'(A)',"----------------------------------"
	print'(A,I10)',"day 5 - part 1 = ",minval(con(:,i_conv))
	print'(A)',"----------------------------------"
	print*,""

! PART 2

	open(unit=1,file=fileName, status="old", action="read")
	seeds = 0
	flag = .true.
	flag_s = .true.
	i_conv = 1
	lineCount = 0

	do
		i = 8
		i_s = 1
			
		flag_w = .true.
		
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		do while ( flag_s )
			if ( line(i:i) >= "0" .and. line(i:i)<="9" ) then
				call readNumber_long(  line(i:i+12), number, len_n )
				seeds2(i_s) = number
				i = i + len_n + 1
				call readNumber_long(  line(i:i+12), number, len_n )
				s_rng(i_s) = number
				print'(A,I2,A,I10,A,I10)',"seed #",i_s," = ",seeds2(i_s)," range = ",s_rng(i_s)
				i = i + len_n
				i_s = i_s + 1
			end if
			i = i + 1
			if ( i > len(trim(line))) flag_s = .false.
		end do

		if ( line(1:4)=="seed" .and. lineCount>1 )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					soil(j,:) = [dest,src,rng]
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
		end if

		if ( line(1:4)=="soil" )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					fert(j,:) = [dest, src, rng] 
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
		end if

		if ( line(1:10)=="fertilizer" )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					water(j,:) = [dest, src, rng] 
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
		end if

		if ( line(1:5)=="water" )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					light(j,:) = [dest, src, rng] 
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
		end if

		if ( line(1:5)=="light" )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					temp(j,:) = [dest, src, rng] 
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
		end if

		if ( line(1:11)=="temperature" )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n

					hum(j,:) = [dest, src, rng] 
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
		end if

		if ( line(1:8)=="humidity" )then
			i_conv = i_conv + 1
			flag_w = .true.
			j = 1
			do while( flag_w )
				lineCount = lineCount + 1
				read(1,"(A)",iostat=ios) line
				if (ios /= 0) exit
				if (line/="") then
					i = 1
					! ->1
					call readNumber_long(  line(i:i+12), dest, len_n )
					i = i + len_n + 1
					! -> 2	
					call readNumber_long(  line(i:i+12), src, len_n )
					i = i + len_n + 1
					! -> 3
					call readNumber_long(  line(i:i+12), rng, len_n )
					i = i + len_n
					loc(j,:) = [dest, src, rng] 
					j = j + 1
				else
					flag_w = .false.
				end if
			end do
			finished = .true.
		end if
		if ( finished ) exit
	 end do
	close(unit=1)
	min = 100000000000
	print'(A)',"--------------------------------------------------------------------------------"

	do i = 1, 10
		print'(A,I2,A,I10,A,I10,A,I12)',"seed number",i," = ",seeds2(i)," - rng =",s_rng(i)," - current min = ",min
		do j = seeds2(i),seeds2(i)+s_rng(i)-1
			flag = .true.
			do k = 1, 31
				if ( j >= soil(k,2) .and. j < soil(k,2) + soil(k,3) ) then
					connection = soil(k,1) + j - soil(k,2)
					flag = .false.
				end if
			end do
			if ( flag ) connection = j
			do k = 1, 47
				if ( connection >= fert(k,2) .and. connection < fert(k,2) + fert(k,3) ) then
					connection = fert(k,1) + connection - fert(k,2)
					exit
				end if
			end do
			do k = 1, 45
				if ( connection >= water(k,2) .and. connection < water(k,2) + water(k,3) ) then
					connection = water(k,1) + connection - water(k,2)

					exit
				end if
			end do
			do k = 1, 43
				if ( connection >= light(k,2) .and. connection < light(k,2) + light(k,3) ) then
					connection = light(k,1) + connection - light(k,2)				
					exit
				end if
			end do
			do k = 1, 20
				if ( connection >= temp(k,2) .and. connection < temp(k,2) + temp(k,3) ) then
					connection = temp(k,1) + connection - temp(k,2)
					exit
				end if
			end do
			do k = 1, 17
				if ( connection >= hum(k,2) .and. connection < hum(k,2) + hum(k,3) ) then
					connection = hum(k,1) + connection - hum(k,2)
					exit
				end if
			end do
			do k = 1, 18
				if ( connection >= loc(k,2) .and. connection < loc(k,2) + loc(k,3) ) then
					connection = loc(k,1) + connection - loc(k,2)
					exit
				end if
			end do
			if (connection<min) min = connection
		end do 
	end do
	print*,""
	print'(A)',"----------------------------------"
	print'(A,I10)',"day 5 - part 2 = ",min
	print'(A)',"----------------------------------"

end program main