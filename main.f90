! BYS 205-01 - Mortgage Calculator 
! Group Members: Lorelei Darzi, Tony Hardiman, and Jasmine Ivey.

program mortgage
    implicit none

    integer :: L, N, payments = 0
    real :: P, J, M, H, C, Q
    character(22) :: column1 = "Payment Number:"
    character(25) :: column2 = " Balance:", column3 = " Scheduled Payment:", column4 = " Monthly Interest:"
    
    !P = principal
    !I = APR
    !L = length of loan in years
    !J = monthly interest in decimal(I/12^100)
    !N = num of months(L*12)
    !M = monthly payment
    !H = current monthly interest
    !C = monthly payment - interest(principal for month)
    !Q = new principal balance

    !user input P & error message
    write(*,*) 'What is the principal loan amount?'
    read(*,*) P

   do while (P < 0)
        print *, 'Invalid Input'
        write(*,*) 'What is the principal loan amount?'
        read(*,*) P
    enddo

    write(*, *) 'You have entered the loan amount as:' , P

    !user input J & error   
    write(*,*) 'What is the monthly interest? (decimal)'
    read(*,*) J

    do while (J < 0)
        print *, 'Invalid Input'
        write(*,*) 'What is the monthly interest? (decimal)'
        read(*,*) J
    enddo

    write(*,'(A, F10.3)') 'You have entered the monthly interest as:' , J

    !user input L & error
    write(*,*) 'What is the length of the loan? (years)'
    read(*,*) L

    do while (L < 0)
        print *, 'Invalid Input'
        write(*,*) 'What is the length of the loan? (years)'
        read(*,*) L
    end do

    write(*,*) 'You have entered the loan length as:' , L

    !monthly interest rate
    J = (J / (12 * 100))

    !total number of months
    N = L * 12

    !monthly payment
    M = P * (J / (1.0 - (1.0 + J) ** (-N)))

    write(*, *) " ", " "

    do while (Q >= 0)
        !calculate current monthly interest
        H = P * J

        !calculate principle for month
        C = M - H

        !calculate new principal
        Q = P - C

        ! Increment Payment
        payments = payments + 1
        
        ! Print the results
        write(*,*) column1, payments
        write(*, '(A, F10.2)') column2, Q
        write(*, '(A, F10.2)') column3, C
        write(*, "(A, F10.2)") column4, H
        print*, " "

        !update principal
        P = Q
    end do
end program