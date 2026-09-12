!Pragram Menu to choose which particular program you wanna compile and run.
program main_menu
        integer :: choice
        print*, "============================================"
        print*, "Select a program to run(1,2,3,4,...so on)"
        print*, "1. prog1"
        print*, "2. prog2"
        print*, "3. prog3"
        print*, "4. Cosolidated Read and Format Example(11.1)"
        print*, "============================================"
        read*, choice

        select case (choice)
        case (1)
                call prog1()
        case (2)
                call prog2()
        call (3)
                call prog3()
        call (4)
                call Read_Format4()
        case default
                print*, "Invalid Choice"
        end select

contains

subroutine prog1()
!formula: i+j-kl+(m**3)/n
        print*, "Enter the Values:-- "
        read*, I,J,K,L,M,N
        nterm= I+J- K*L + (M**3)/N
        print*, nterm
        stop
        end subroutine prog1

subroutine prog2()
        !formula: (mod(sin(A))+ln(B)))/(cos(A)-exp(B))
        implicit none
        print*, "Enter value of A & B: "
        read*, A,B
        a_num= abs(sin(A)) - ALOG(B)
        den  = cos(A) - exp(B)
        result= a_num/den
       print*,"Result: ", result
      stop
      end subroutine prog2

subroutine prog3()
        implicit none
        character(len= 80):: a,b,c        
        print*, "Enter 1st name: "
        read*, a
        print*, "Enter middle name: "
        read*, b
        c= trim(a)//trim(b)
        print*,"Name is: ",c
        STOP
        end subroutine prog3

subroutine Read_Format4()
	implicit none
	integer:: int, i, j
	real:: a,b,c,d,e,f,g,h,p,q
	
	read(*,10) int
	10 Format(I4)
	read(*,20) a,b,c,d
	20 Format(F4.2,F5.2,F4.0,F6.0)
	read(*,30) e,f
	30 Format(E8.2,E6.0)
	read(*,40) g,h,i
	40 Format(F6.2,E8.2,I6)
	read(*,50) p,q,j
	50 Format(F8.2,E5.0,I4)

	print*, "-----Output Results-----"
	print*, int
	print*, a,b,c,d
	print*, e,f
	print*, g,h,i
	print*, p,q,j
	STOP
	END subroutine Read_Format4
end program main_menu
