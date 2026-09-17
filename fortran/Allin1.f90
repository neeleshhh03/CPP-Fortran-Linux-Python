!Pragram Menu to choose which particular program you wanna compile and run.
program main_menu
        integer :: choice
        print*, "============================================"
        print*, "Select a program to run(1,2,3,4,...so on)"
        print*, "1. Celcius to Farenhite"
        print*, "2. Magnitude of Vector Resultant"
        print*, "3. Area Calculator for Different Geometrical shapes"
        print*, "4. Concatenate two or three words or sentences & split one sentence into two or three parts."
        print*, "5. WAP to prepare Histogram graph of the marks, keeping 5 marks as interval." 
	print*, "============================================"
        read*, choice

        select case (choice)
        case (1)
                call Q1()
        case (2)
                call Q2()
	case (3)
		call area_calculator()
	case (4)
		call con_split()
	case (5)
		call histogram()
        case default
                print*, "Invalid Choice"
        end select

contains

subroutine Q1()
        implicit none
        real:: C_t, F_t
        print*, "Enter Value of Temperature(in Degree Celcius): "
        read*, C_t

        F_t= (C_t * (9.0/5.0))+32
        
        print*, "Teperature(In Degree Farenhite) is: ",F_t
        stop
        end subroutine Q1

subroutine Q2()
        implicit none
        integer:: a,b,c
        real:: res
        print*, "*******We are calculating resultant vector of wind Vector*******"
        print*, "Enter value of 'a': "
        read*, a 
        print*, "Enter value of 'b': "
        read*, b 
        print*, "Enter value of 'c': "
        read*, c 

        res= sqrt(float(a**2+b**2+c**2))
        print*, "Magnitude of resustant is: ", res
        stop 
	end subroutine Q2

subroutine area_calculator()
    implicit none
    integer :: choice
    real :: base, height, length, width, radius, area, pi= 3.1415926535959

    print *, "Choose a shape to calculate the area:"
    print *, "1. Triangle (Base and Height)"
    print *, "2. Circle (Radius)"
    print *, "3. Sphere (Surface Area - Radius)"
    print *, "4. Rectangle (Length and Width)"
    read *, choice

    select case (choice)
    case (1)
        print *, "Enter the base:"
        read *, base
        print *, "Enter the height:"
        read *, height
        area = (1.0/2.0)*base*height
        print *, "The area of the triangle is: ", area

    case (2)
        print *, "Enter the radius of the circle:"
        read *, radius
        area = pi * radius**2
        print *, "The area of the circle is: ", area

    case (3)
        print *, "Enter the radius of the sphere:"
        read *, radius
        area = 4.0 * pi * radius**2
        print *, "The surface area of the sphere is: ", area

    case (4)
        print *, "Enter the length:"
        read *, length
        print *, "Enter the width:"
        read *, width
        area = length * width
        print *, "The area of the rectangle is: ", area

    case default
        print *, "Invalid choice! Please run the program again and select 1, 2..."
    end select

end subroutine area_calculator

subroutine con_split()
implicit none
        character(len=8)   :: word1, word2
        character(len=50)  :: sentence
        character(len= 16) :: con

        print*, "=====To Concatenate the words===="
        print*, "Enter two words: "
        read*, word1, word2

        con= trim(word1) // trim(word2)
        print*, "Concatenated word is: ", con

        print*
        print*, "=====Split one sentence into to two part====="
        print*, "=====sentence length should be 50(max.)====="
        print*
        print*, "Enter a Sentence): "
        read(*, '(A)') sentence

        print*, "1st Split Sentence is: ", sentence(1:25)
        print*, "2nd Split Sentence is: ", sentence(26:50)
stop
end subroutine con_split

!Prepare a frequency histogram of marks as the interval.
subroutine histogram()
	implicit none
	integer:: roll_no(10), marks(10,3)
	integer:: freq(20)
	integer:: i,j, bin_idx, total_marks
	character(len=100):: store
	i=0
	freq=0
	bin_idx= 0
	open(50, file='student.txt', status= 'old', action= 'read')
	
	read(50,'(A)') store
	do i=1,10
		read(50,*) roll_no(i),marks(i,1),marks(i,2),marks(i,3)
	end do
	close(50)

	do i=1,10
		do j=1,3
			total_marks=marks(i,j) 
			! Calculate bin index (0-4 -> 1, 5-9 -> 2, ..., 95-100 -> 20)
			bin_idx= (total_marks/5)+1
			freq(bin_idx)= freq(bin_idx)+1
		end do
	end do 

print*,"Interval | Frequency | Histogram"
print*,"--------------------------------"
do i=1,20
	if(freq(i)>0) then
		print "(i2, '-', i2, 2x,'|', 2x, i2,2x, '|', 2x, A)", (i-1)*5, (i*5)-1, freq(i), repeat('*', freq(i))
	end if
end do

end subroutine histogram
end program main_menu
