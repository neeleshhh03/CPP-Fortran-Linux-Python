program Grade_Division
    implicit none

    integer, parameter :: p = 5
    real :: mm(p), sum(p), avg(p)
    integer :: i, j, k
    character(len=6) :: Division
    character(len=15) :: name(p)

    print*, "=========Details of name & score(out of 100)========="

    ! Loop through 3 semesters
    do j = 1, 3
        print*, ""
        print*, "========================================="
        print*, "Semester: ", j
        print*, "========================================="
        
        ! Initialize/Reset sums and averages for the current semester
        do k = 1, p
            sum(k) = 0.0
            avg(k) = 0.0
        end do

        ! Loop through 5 students
        do k = 1, p
            print*, "Enter Student Name ", k, ":"
            read*, name(k)
            print*, "Enter marks: "
            read*, mm(k)

            sum(k) = sum(k) + mm(k)
            avg(k) = sum(k) / p

            ! Evaluate Division
            if (mm(k) >= 60.0) then
                Division = "first"
            else if (mm(k) >= 45.0 .and. mm(k) < 60.0) then
                Division = "second"
            else if (mm(k) >= 33.0 .and. mm(k) < 45.0) then
                Division = "third"
            else
                Division = "Fail"
            endif

            print*, name(k), " achieved ", Division, " Division"
            print*, "-----------------------------------------"
        end do
    end do

    stop
end program Grade_Division
