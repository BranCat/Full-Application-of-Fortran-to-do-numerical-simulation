program full_fortran_application
    implicit none
    
    ! Define the differential equation size
    integer, parameter :: n = 2  
    
    ! Other parameters
    integer :: num = 60, i, j
    real(kind=8):: t0, ti, dt, tmax
    real(kind=8):: x0(n), xi(n)
    
    ! Define the array to read to rate-constant data
    integer, allocatable :: date_num(:)                     
    real(kind=8), allocatable :: r_rate(:) 
    allocate(date_num(num), r_rate(num))
    
    ! Access the rate-constant data file
    open(1, file = 'Reaction_rate_constant.txt')
    
    ! Create the file to expore the simulation result
4    open(2, file='Simulation result2.txt', status='new')
      
    ! Define the initial conditions and time step
    t0 = 0.0
    x0(1) = 50.0
    x0(2) = 0.0
    
    dt = 0.5
    ti = 0.0
    tmax = 60.0
    
    ! Read the reaction rate-constant data file
    do i = 1, num
        read(1, *) date_num(i), r_rate(i)
    end do
    
    !!! Loop to integration of RK4 numerical solution     
    do while(ti <= tmax)          
       do i = 1, num
            ti = t0 + dt
            
            ! The following codes to make sure the integration time step is smaller than reaction rate-constant data time interval, and make sure all the rate-constant data can be read
            if (ti >= (i-1) .and. ti <= i) then
                call ODE_RK4(t0, ti, x0, xi, n, r_rate(i))
                print*, ti, xi(1), xi(2)
                write(2, *), ti, xi(1), xi(2)
                
                t0 = ti
                
                do j = 1, n
                    x0(j) = xi(j)
                end do
            else 
                continue
            endif
            
        end do
    end do
   
    deallocate(date_num, r_rate)
        
end program
