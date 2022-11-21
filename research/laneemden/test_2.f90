Program test_RK4

   integer, parameter :: SIZE = 1000

! x = zi, y = theta

!y'' + (2/x)y' + y = 0

   real*8 x(SIZE), y(SIZE), z(SIZE)
   real*8 xl, x0, h
   integer k, kl, l, j, jl

   pi = 4.d0*DATAN(1.d0)
   x0 = 1.e-15 ! 0.d0
   xl = 1.d0 ! 3.14159265358979d0
   kl = 1000.d0
   h = (xl - x0)/kl
   x(1) = x0
   y(1) = 1.d0
   z(1) = -1.e-15/3.d0
  

   do k=1, kl - 1 
        do j=1, jl -1
                call RK4(x(k), y(k), z(k), h, x(k + 1), y(k + 1), z(k + 1))
      end do 
   end do

   print *,'# '
   print *,'#     X      Y estimated    Z estimated'
   print *,' #-------------------------------------'
  !write kl+1 result lines
   do k=1, kl
     write(*,'(F9.4,5(ES23.15,1X))') x(k), y(k), z(k)
   end do
   print *,'# -------------------------------------'
   print *,'# '
   stop

10 format(F9.4,F12.7,F15.7)

CONTAINS
   ! Computes RHS of the 1st order linear system:
   ! / y' = z,
   ! \ z' = -(2/x)*z - y**n
   !  where y = y(1), z = y(2)
   Function f(x,y,z)
   implicit none
     real*8 x
     real*8, dimension(1) ::  y, z, f 
     real*8, parameter :: n = 1.d0
     f(1) = z(1)
     !f(2) = (SIN(x*pi))/((x + 1.e-15)*pi)
     !f(1) = y(2)
   END
   
   Function g(x,y,z)
   implicit none
     real*8 x
     real*8, dimension(1) :: z, g, y
     real*8, parameter :: n = 1.d0
     !y = (SIN(x*pi))/((x + 1.e-15)*pi)
     g(1) = (-2.d0/((x + 1.e-15)*pi))*z(1) - y(1)**n
     !g(1) = (-2.d0/((x + 1.e-15)*pi))*(1.d0/(x*x))*(x*COS(x*pi) - SIN(x*pi)) - ((SIN(x*pi))/((x + 1.e-15)*pi))**n
   END

   Subroutine rk4(x, y, z, h, x1, y1, z1)
   implicit none
   real*8, intent(in)    :: x, h, y(1), z(1)
   real*8, intent(inout) :: x1, y1(1), z1(1)
   real*8, dimension(1)  :: k1, k2, k3, k4, j1, j2, j3, j4
     k1 = h*f(x, y, z)
     j1 = h*g(x, y, z)
     k2 = h*f(x + h/2.d0, y + k1/2.d0, z + k1/2.d0)
     j2 = h*g(x + h/2.d0, y + j1/2.d0, z + j1/2.d0)
     k3 = h*f(x + h/2.d0, y + k2/2.d0, z + k2/2.d0)
     j3 = h*g(x + h/2.d0, y + j2/2.d0, z + j2/2.d0)
     k4 = h*f(x + h, y + k3, z + k3)
     j4 = h*g(x + h, y + j3, z + j3)
     y1 = y + (k1 + 2.0*k2 + 2.0*k3 + k4)/6.d0
     z1 = z + (j1 + 2.0*j2 + 2.0*j3 + j4)/6.d0
     x1 = x + h
   End

END PROGRAM
