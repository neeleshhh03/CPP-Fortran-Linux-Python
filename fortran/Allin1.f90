!Pragram Menu to choose which particular program you wanna compile and run.
program main_menu
        integer :: choice
        print*, "============================================"
        print*, "Select a program to run(1,2,3,4,...so on)"
        print*, "1. Celcius to Farenhite"
        print*, "2. Magnitude of Vector Resultant"
        print*, "3. Area Calculator for Different Geometrical shapes"
        print*, "4. prog4"
        print*, "============================================"
        read*, choice

        select case (choice)
        case (1)
                call Q1()
        case (2)
                call Q2()
	case (3)
		call area_calculator()
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

end program main_menu
