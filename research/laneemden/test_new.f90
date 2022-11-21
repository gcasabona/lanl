Program test_RK4

   integer, parameter :: SIZE = 1000

! x = zi, y = theta = y(1), z = y' = y(2)

!y'' + (2/x)y' + y = 0

   real*8 x(SIZE), y(SIZE,2)
   real*8 xl, x0, h
   integer k, kl, l

   pi = 4.d0*DATAN(1.d0)
   x0 =  0.d0
   xl = 1.d0 ! 3.14159265358979d0
   kl = 1000.d0
   h = (xl - x0)/kl
   x(1) = x0
   y(1,1) = 1.d0
   y(1,2) = -1.e-15/3.d0 !  -x(1)/3.d0
  

   do k=1, kl - 1
      call RK4(x(k), y(k,1:2), h, x(k + 1), y(k + 1,1:2))
   end do

   print *,'# '
   print *,'#     x    y-estimated   z-estimated '
   print *,' #-------------------------------------'
  !write kl+1 result lines
   do k=1, kl
     write(*,'(F9.4,5(ES23.15,1X))') x(k), y(k,1), y(k,2)
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
   Function f(x,y)
   implicit none
     real*8 x
     real*8, dimension(2) :: y, f
     real*8, parameter :: n = 1.d0
     y = (SIN(x*pi))/((x + 1.e-15)*pi)
     f(1) = y(2)
     f(2) = (-2.d0/((x + 1.e-15)*pi))*y(2) - y(1)**n
     !f(2) = (-2.d0/(LOG(x + 1.e-15)))*y(2) - y(1)**n  
     !f(2) = (-2.d0/(pi*LOG(x + 1.e-15)))*y(2) - y(1)**n  
   END

   Subroutine rk4(x, y, h, x1, y1)
   implicit none
   real*8, intent(in)    :: x, h, y(2)
   real*8, intent(inout) :: x1, y1(2)
   real*8, dimension(2)  :: k1, k2, k3, k4
     k1 = h*f(x, y)
     k2 = h*f(x + h/2.d0, y + k1/2.d0)
     k3 = h*f(x + h/2.d0, y + k2/2.d0)
     k4 = h*f(x + h, y + k3)
     y1 = y + (k1 + 2*k2 + 2*k3 + k4)/6.d0
     x1 = x + h
   End

END PROGRAM
