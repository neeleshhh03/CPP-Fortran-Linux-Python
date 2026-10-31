program rainfall
	implicit none
	integer, parameter :: nsets= 6
	integer		   :: is, iy, m, iyear, I, J, c, d, alloc_status
	real, allocatable  :: rf(:, :, :), std(:, :)
	character(len=95)  :: details
	character(len=5)   :: st
	character(len=107) :: detail1
	character(len=100) :: detail2
	character(len=107) :: detail3

	open(11, file='Rainfall.txt', form='formatted', access= 'sequential', status='old')
	open(12, file='rain-output.txt', form='formatted', access= 'sequential', status='unknown')
!------------------
	allocate(rf(6, 146, 17), std(4, 17), stat=alloc_status)
	if (alloc_status /= 0) then
		print *, "Error: Failed to allocate cache memory!"
		stop
	end if
!------------------
	do is=1, nsets
		read(11,1) iyear, details
1		format(i3,2x,A)
		write(12,1) iyear, details

		do c= 1, 5
			read(11,2) detail1
2	        	format(A)
        		write(12,2) detail1
        	end do

		do iy=1, 146
			read(11,7) iyear, (rf(is, iy, m), m=1,17)
7			format(i4,1x,16f6.1,1x,f7.1)
			write(12,7) iyear, (rf(is, iy, m), m=1, 17)

		end do
	
	!---Rainfall Statistics---
		do d=1, 3
			read(11,153) detail2
153			format(A)
			write(12,153) detail2
		end do

	!Monthly and Seasonal rainfall stats
		
		do I= 1, 4
			read(11,156) st
156			format(A5)
			read(11, *) (std(I,J), J= 1, 17)
			write(12,157) st, (std(I,J), J= 1, 17)
157			format(A5, 16F6.1, 1X, F7.1)

			read(11, 160) detail3
160			format(a)
			write(12, 160) detail3	
		end do
	end do
	close(11)
	close(12)
	print*, "=======Given I/P have been written on O/P file in Format======="
!-----------------
	if (allocated(rf)) deallocate(rf)
	if (allocated(std)) deallocate(std)
!---------------
	stop
end program rainfall
