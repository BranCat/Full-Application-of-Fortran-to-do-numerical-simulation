! Define the Ordinary Differential Equations(ODEs) in this file
subroutine diff(t, x, dx, r_rate)
   implicit none
   
   integer, parameter :: n = 2
   real(kind=8) :: t
   real(kind=8) :: x(n), dx(n)
   real(kind=8) :: r_rate         !!! r_rate is passed from main file, and they are from rate-constant data file
   
   ! Define the ODE or ODEs here   
   dx(1) = -r_rate*x(1)
   dx(2) = r_rate*x(1)
end subroutine
