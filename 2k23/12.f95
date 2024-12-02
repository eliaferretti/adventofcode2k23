program main

	use utilities

	implicit none

	! variable declaration
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0, sum2 = 0
	integer							:: i, j, k, ios, count, idx, l, num, add_idx
	integer 							:: counter, count_seq, sum_o = 0, sum2_o = 0	
	integer							:: lineCount = 0, counter2		
	logical 							:: flag
	character(len=100)			:: bin
	character(len=1), 	      allocatable :: spring(:), s2(:)
	integer, 						allocatable :: seq(:), seq2(:)	
	integer, 						allocatable :: indices(:), indices2(:)				

	fileName = "inputs/input12.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		idx = index(line, " ")
		l = 0
		do j = idx, len(trim(line))
			if ( line(j:j)=="," ) l = l + 1
		end do 

		allocate( spring(idx-1) )
		allocate( s2(5*(idx-1)) )
		allocate( seq2(5*(l+1)) )
		allocate( seq(l+1)      ) 

		do i = 1, len(trim(line))
			if ( i<idx ) then
				spring(i) = line(i:i)
			elseif ( i>idx ) then
				flag = .true.
				j = i
				count = 1
				do while ( flag )
					call readNumber_super_short( line(j:j+2), num, add_idx ) 
					seq(count) = num
					j = j + add_idx + 1
					count = count + 1
					if ( j > len(trim(line)) ) flag = .false.
				end do
				exit
			end if
		end do

		do i = 1, 5
			s2((i-1)*(idx-1)+1:i*(idx-1)) = spring(:)
			seq2((i-1)*(l+1)+1:i*(l+1))   = seq(:)
		end do 

		counter = 0	
		do i = 1, idx-1
			if ( spring(i)=="?" ) counter = counter + 1
		end do

		counter2 = 0	
		do i = 1, 5*(idx-1)
			if ( s2(i)=="?" ) counter2 = counter2 + 1
		end do

		print*,s2
		print*,seq2

		allocate( indices(counter) )
		allocate( indices2(counter2) )

		count = 1
		do i = idx-1,1,-1
			if ( spring(i)=="?" ) then
				indices(count) = i
				count = count + 1
			end if
		end do

		count = 1
		do i = 5*(idx-1),1,-1
			if ( s2(i)=="?" ) then
				indices2(count) = i
				count = count + 1
				print*,i
			end if
		end do

		print*,counter,counter2

		do i = 0, 2**counter - 1
			bin = "0"
			write(bin,'(B0)') i
    
      	do j = 1, counter
      		if ( j <= len(trim(bin)) ) then
               if ( bin(len(trim(bin))+1-j:len(trim(bin))+1-j)=="1" ) then
                  spring(indices(j)) = '#' ! set to '#' if the corresponding bit is 1
               else
                  spring(indices(j)) = '.' ! set to '.' if the corresponding bit is 0
               end if
            else
               spring(indices(j)) = '.'
            end if		
         end do

         count = 0
         count_seq = 1
         do j = 1, idx-1
            if ( spring(j)=="#" ) then
               count = count + 1
            else
            	if ( count > 0) then
            	   if ( count_seq < l+2 ) then
	            	   if ( count == seq(count_seq) ) then
	            	      count_seq = count_seq + 1
	                  else
	            	      exit
	                  end if
	               else
	            	   exit
	               end if
	               count = 0
               end if
            end if
            if ( j == idx-1 .and. spring(j)=="#" .and. count > 0 ) then
            	if ( count_seq < l+2 ) then
	            	if ( count == seq(count_seq) ) then
	            		count_seq = count_seq + 1
	            		count = 0 
	            	end if
	            end if	   
            end if 
         end do

         if ( count_seq == l+2 .and. count == 0 ) sum = sum + 1

      end do

      ! PART - 2

      do i = 0, 2**counter2 - 1
			bin = "0"
			write(bin,'(B0)') i
    
      	do j = 1, counter2
      		if ( j <= len(trim(bin)) ) then
               if ( bin(len(trim(bin))+1-j:len(trim(bin))+1-j)=="1" ) then
                  s2(indices2(j)) = '#' ! set to '#' if the corresponding bit is 1
               else
                  s2(indices2(j)) = '.' ! set to '.' if the corresponding bit is 0
               end if
            else
               s2(indices2(j)) = '.'
            end if		
         end do

         count = 0
         count_seq = 1
         do j = 1, 5*(idx-1)
            if ( s2(j)=="#" ) then
               count = count + 1
            else
            	if ( count > 0) then
            	   if ( count_seq < 5*(l+1)+1 ) then
	            	   if ( count == seq2(count_seq) ) then
	            	      count_seq = count_seq + 1
	                  else
	            	      exit
	                  end if
	               else
	            	   exit
	               end if
	               count = 0
               end if
            end if
            if ( j == 5*(idx-1) .and. s2(j)=="#" .and. count > 0 ) then
            	if ( count_seq < 5*(l+1)+1 ) then
	            	if ( count == seq2(count_seq) ) then
	            		count_seq = count_seq + 1
	            		count = 0 
	            	end if
	            end if	   
            end if 
         end do

         if ( count_seq == 5*(l+1)+1 .and. count == 0 ) sum2 = sum2 + 1

       end do


		print*,"#",lineCount," - comb(1) = ",sum - sum_o," - comb(2) = ",sum2 - sum2_o
		sum_o = sum; sum2_o = sum2

		deallocate( s2 )
		deallocate( seq2 )
		deallocate( indices2 )
		deallocate( spring )
		deallocate( seq )
		deallocate( indices )
		exit
	end do

	close(unit=1)

	print*,""
	print*,"-----------------------------"
	print*,"day 12 - part 1 = ",sum
	print*,"-----------------------------"

	print*,""
	print*,"-----------------------------"
	print*,"day 12 - part 2 = ",sum2
	print*,"-----------------------------"

end program main