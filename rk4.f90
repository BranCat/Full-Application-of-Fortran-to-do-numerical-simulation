!RK4 numerical solution of differential equations 
subroutine ODE_RK4(t0, ti, x0, xi, num, r_rate)
    implicit none
    
    integer num, j 
    real(kind=8) :: t0, ti, h, t
    real(kind=8) :: r_rate
    real(kind=8) :: x0(num), xi(num)
    real(kind=8) :: x(num), dx(num)
    real(kind=8) :: k1(num),k2(num),k3(num),k4(num)

    h = ti - t0
    t = t0
        
    ! Calculate k1
    call diff(t, x0, dx, r_rate)
    do j = 1,num
        k1(j) = h*dx(j)
        x(j) = x0(j) + k1(j)/2.0
    end do
        
    ! Calculate k2
    call diff(t+h/2.0, x, dx, r_rate)
    do j=1,num
        k2(j) = h*dx(j) 
        x(j)  = x0(j) + k2(j)/2.0   
    end do

    ! Calculate k3
    call diff(t+h/2.0, x, dx, r_rate)
    do j=1,num
        k3(j) = h*dx(j) 
        x(j)  = x0(j) + k3(j)   
    end do     

    ! Calculate k4 and the result      
    call diff(t+h, x, dx, r_rate)
    do j=1,num
        k4(j) = h*dx(j)
        xi(j) = x0(j) + k1(j)/6.0+k2(j)/3.0+k3(j)/3.0+k4(j)/6.0 
    end do  
end subroutine ODE_RK4
