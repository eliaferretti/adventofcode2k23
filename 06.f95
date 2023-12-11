program main

	use utilities

	implicit none

	! variable declaration
	integer							:: i, j
	integer,      dimension(4) :: time, dist, comb	
	integer 							:: t, d	

	time = [47, 84, 74, 67]
	dist = [207, 1394, 1209, 1014]
	comb = 0

	do i = 1, 4
		do j = 1, dist(i)
			if ( (time(i)-j)*j>dist(i) ) comb(i) = comb(i)+1
		end do		
	end do

	print*,""
	print*,"-----------------------------"
	print*,"day 6 - part 1 = ",comb(1)*comb(2)*comb(3)*comb(4)
	print*,"-----------------------------"


	! PART 2

	t = 47847467
	d = 207139412091014

	print*,"-----------------------------"
	print*,"day 6 - part 2 = ",int(sqrt(real(t**2 - 4*d)))
	print*,"-----------------------------"

end program main