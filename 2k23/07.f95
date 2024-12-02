program main

	use utilities

	implicit none

	! variable declaration
	character(len=100) 			:: line
	character(len=20)				:: fileName
	integer 							:: sum = 0
	integer							:: i, j, k, ios, count, number, add_idx, i_o
	integer							:: lineCount = 0, max_p, coeff, count_j		
	logical 							:: flag			
	integer, dimension(1000,5) :: cards, o_cards
	integer, dimension(1000,5) :: c
	integer, dimension(1000)   :: bids, o_bids, order, idx, points,hand_card &
										 , dummy, ix, o_hand_card, combo, o_combo
	integer, dimension(1000,13) :: counter, o_counter
	integer, dimension(5) :: old_line
	integer, dimension(7) :: hand
	character(len=5) ::str

	fileName = "inputs/input07.txt"
	open(unit=1,file=fileName, status="old", action="read")

	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		do i = 1, len(trim(line))
			if (i <= 5) then
				call convertCard(line(i:i),number)
				cards(lineCount,i) = number 
			else if ( i >= 7 ) then
				call readNumber_short(line(i:i+4),number,add_idx)
				bids(lineCount) = number
				exit
			end if
		end do
	end do
	close(unit=1)
	c = cards
	counter = 0
	order = 0
	do i = 1, 1000
		old_line = c(i,:)
		do j = 1, 5
			do k = 13, 1, -1
				counter(i,k) = counter(i,k) + c(i,j)/10**(k-1)
				c(i,j) = mod(c(i,j),10**(k-1))
			end do 
			c(i,:) = old_line
		end do 
	end do 
	counter = counter**2

	points = 0

	hand = 0
	hand_card = 0
	do i = 1, 1000
		do k = 1, 13
			points(i) = points(i) + counter(i,k)
		end do
		select case ( points(i) )
			case( 5 )
				hand(1) = hand(1) + 1 
				combo(i) = 1
			case( 7 )
				hand(2) = hand(2) + 1
				combo(i) = 2
			case( 9 )
				hand(3) = hand(3) + 1
				combo(i) = 3
			case( 11 )
				hand(4) = hand(4) + 1
				combo(i) = 4
			case( 13 )
				hand(5) = hand(5) + 1
				combo(i) = 5
			case( 17 )
				hand(6) = hand(6) + 1
				combo(i) = 6
			case( 25 )
				hand(7) = hand(7) + 1
				combo(i) = 7
			case default
				print*,"ERROR"
		end select
	end do

	idx = pack([(i, i=1,1000)], mask=combo>0)

	call sort_indices(combo, idx)

	sum = 0 
	o_combo = combo
	o_cards = cards
	o_bids = bids
	o_counter = counter
	do i = 1,1000
		combo(i) = o_combo(idx(i))
		cards(i,:) = o_cards(idx(i),:)
		bids(i) = o_bids(idx(i))
		counter(i,:) = o_counter(idx(i),:)
	end do



	order = 0
	do i = 1, 1000
		call getCards(cards(i,:), str)
		do j = 1, 5
			call convertCard_norm( str(j:j), number )
			order(i) = order(i) + number*10**(10-2*j)
		end do
	end do

	sum = 0 
	idx = pack([(i, i=1,1000)], mask=points>0)
	i = 1
	do k = 7, 1, -1
		ix = 0
		call sort_indices2(order(i:i+hand(k)-1), idx(i:i+hand(k)-1), dummy(i:i+hand(k)-1), ix(i:i+hand(k)-1))
		o_cards = cards
		o_bids = bids
		o_counter = counter
		do j = i,i+hand(k)-1
			cards(j,:) = o_cards(ix(j),:)
			bids(j) = o_bids(ix(j))
			counter(j,:) = o_counter(ix(j),:)
		end do
		i = i + hand(k)
	end do

	order = 1
	do i = 1, 1000
		call getCards(cards(i,:), str)
		do j = 1, 5
			call convertCard_norm( str(j:j), number )
			order(i) = order(i) + number*10**(10-2*j)
		end do
	end do

	sum = 0 
	idx = pack([(i, i=1,1000)], mask=points>0)
	i = 1
	do j = 7,2,-1
		i = i + hand(j)
	end do

	k = 1
	ix = 0
	call sort_indices2(order(i:i+hand(k)-1), idx(i:i+hand(k)-1), dummy(i:i+hand(k)-1), ix(i:i+hand(k)-1))
	o_cards = cards
	o_bids = bids
	o_counter = counter
	o_hand_card = hand_card
	do j = i,i+hand(k)-1
		cards(j,:) = o_cards(ix(j),:)
		bids(j) = o_bids(ix(j))
		counter(j,:) = o_counter(ix(j),:)
		hand_card(j) = o_hand_card(ix(j))
	end do

	sum = 0
	do j = 1,1000
		sum = sum + bids(j)*(1001-j)
		call getCards(cards(j,:),str)
	end do


	print*,"-----------------------------"
	print*,"day 7 - part 1 = ",sum
	print*,"-----------------------------"

! PART 2
	
	open(unit=1,file=fileName, status="old", action="read")
	lineCount = 0
	do
		lineCount = lineCount + 1
		read(1,"(A)",iostat=ios) line
		if (ios /= 0) exit

		do i = 1, len(trim(line))
			if (i <= 5) then
				call convertCard(line(i:i),number)
				cards(lineCount,i) = number 
			else if ( i >= 7 ) then
				call readNumber_short(line(i:i+4),number,add_idx)
				bids(lineCount) = number
				exit
			end if
		end do
	end do
	close(unit=1)

	c = cards
	counter = 0
	order = 0
	do i = 1, 1000
		old_line = c(i,:)
		do j = 1, 5
			do k = 13, 1, -1
				counter(i,k) = counter(i,k) + c(i,j)/10**(k-1)
				c(i,j) = mod(c(i,j),10**(k-1))
			end do 
			c(i,:) = old_line
		end do 
	end do 
	counter = counter**2

	points = 0

	hand = 0
	hand_card = 0
	do i = 1, 1000
		do k = 1, 13
			points(i) = points(i) + counter(i,k)
		end do
		select case ( points(i) )
			case( 5 )
				hand(1) = hand(1) + 1 
				combo(i) = 1
			case( 7 )
				hand(2) = hand(2) + 1
				combo(i) = 2
			case( 9 )
				hand(3) = hand(3) + 1
				combo(i) = 3
			case( 11 )
				hand(4) = hand(4) + 1
				combo(i) = 4
			case( 13 )
				hand(5) = hand(5) + 1
				combo(i) = 5
			case( 17 )
				hand(6) = hand(6) + 1
				combo(i) = 6
			case( 25 )
				hand(7) = hand(7) + 1
				combo(i) = 7
			case default
				print*,"ERROR"
		end select
	end do

	idx = pack([(i, i=1,1000)], mask=combo>0)

	call sort_indices(combo, idx)

	sum = 0 
	o_combo = combo
	o_cards = cards
	o_bids = bids
	o_counter = counter
	do i = 1,1000
		combo(i) = o_combo(idx(i))
		cards(i,:) = o_cards(idx(i),:)
		bids(i) = o_bids(idx(i))
		counter(i,:) = o_counter(idx(i),:)
	end do

	do i = 1,1000
		count_j = 0
		do k = 1, 13
			points(i) = points(i) + counter(i,k)
			if ( counter(i,k)>0 ) count_j = count_j + 1
		end do
		if ( count_j>0 ) then
			select case ( points(i) )
				case( 5 )
					! working here
				case( 7 )
					
				case( 9 )
					
				case( 11 )
					
				case( 13 )
					
				case( 17 )
					
				case( 25 )
					
				case default
					!print*,"ERROR"
			end select
		end if
	end do

	order = 0
	do i = 1, 1000
		call getCards(cards(i,:), str)
		do j = 1, 5
			call convertCard_norm( str(j:j), number )
			order(i) = order(i) + number*10**(10-2*j)
		end do
	end do

	sum = 0 
	idx = pack([(i, i=1,1000)], mask=points>0)
	i = 1
	do k = 7, 1, -1
		ix = 0
		call sort_indices2(order(i:i+hand(k)-1), idx(i:i+hand(k)-1), dummy(i:i+hand(k)-1), ix(i:i+hand(k)-1))
		o_cards = cards
		o_bids = bids
		o_counter = counter
		do j = i,i+hand(k)-1
			cards(j,:) = o_cards(ix(j),:)
			bids(j) = o_bids(ix(j))
			counter(j,:) = o_counter(ix(j),:)
		end do
		i = i + hand(k)
	end do

	order = 1
	do i = 1, 1000
		call getCards(cards(i,:), str)
		do j = 1, 5
			call convertCard_norm( str(j:j), number )
			order(i) = order(i) + number*10**(10-2*j)
		end do
	end do

	sum = 0 
	idx = pack([(i, i=1,1000)], mask=points>0)
	i = 1
	do j = 7,2,-1
		i = i + hand(j)
	end do

	k = 1
	ix = 0
	call sort_indices2(order(i:i+hand(k)-1), idx(i:i+hand(k)-1), dummy(i:i+hand(k)-1), ix(i:i+hand(k)-1))
	o_cards = cards
	o_bids = bids
	o_counter = counter
	o_hand_card = hand_card
	do j = i,i+hand(k)-1
		cards(j,:) = o_cards(ix(j),:)
		bids(j) = o_bids(ix(j))
		counter(j,:) = o_counter(ix(j),:)
		hand_card(j) = o_hand_card(ix(j))
	end do

	sum = 0
	do j = 1,1000
		sum = sum + bids(j)*(1001-j)
		call getCards(cards(j,:),str)
	end do


	print*,"-----------------------------"
	print*,"day 7 - part 2 = ",sum
	print*,"-----------------------------"

end program main