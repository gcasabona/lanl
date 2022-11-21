Program test_RK4
   implicit none
   integer, parameter :: i = 1000

! x = radius, y1 = mass, y2 = rho, y3 = pressure

! TOV eqns

   real*8 x(i), y(i,2), a(i), b(i), xi(1),  xf(1), yi(1), yf(1), y2f(1), ymin(2), R(1)
   real*8 xl, x0, h
   real*8, parameter :: n = 1.d0, gma = 1.98d0, c = 1.d12
   real*8, parameter :: pi = 4.d0*DATAN(1.d0)
   integer :: k, kl, l  

   x0 =  0.d0
   xl = 2.d6 ! 3.14159265358979d0
   kl = i
   h = (xl - x0)/kl
   x(1) = x0
   y(1,1) = 0.d0 
   y(1,2) = 1.d15 !10.d14
   

   print *,'# '
   print *,'#  Radius          Mass              Rho               Pressure '
   print *,' #-------------------------------------'
   integrate: do k=1, kl - 1
      write(*,'(5(ES23.15,1X))') x(k), y(k,1), y(k,2), p(y(k,2))
      y(k + 1, :) = y(k,:) + h*f(x(k),y(k,:))
      IF (y(k+1,2) < 0.d0) EXIT integrate
      call RK4(x(k), y(k,1:2), h, x(k + 1), y(k + 1,1:2))
   end do integrate

   xi = x(k)
   xf = x(k + 1)   !MAXVAL(x, DIM=1, MASK = x .LT.1.254d6)
   yi = y(k, 2)
   yf = y(k + 1, 2)
   R  = xi - yi*(xi - xf)/(yi - yf)
   print *, '# Radius of star =', R

           !print *, 'size of rho =', SIZE(b)
           !yf = MINVAL(a, MASK = a .GT.0)   !ymin(1)
           !write(*, *) b(size(b))
           !s = b(size(b))
           !print *, 's =',  s
           !y2f = b(s)  !MINVAL(b, MASK =b .LT.0)  !ymin(2)  !   Y(ubound(Y, DIM=2)) 
           !R = -(yf/y2f) + xf 
           
           
           !print *,'# -------------------------------------'
           !print *,'# '
           !print *, 'Radius of star =', R
   !print *, 'Final x, y, y''', xf, yf, y2f
   !print *, 'maxloc X ', a 
   !write(*,'(F9.4,5(ES23.15,1X))') y



   !IF (Y(k,1) + h*Y(k,2) == 0d0) THEN
    !    print *,'R =', X(k)
   !ENDIF

10 format(F9.4,F12.7,F15.7)



CONTAINS

   Function p(rho)
   implicit none
     real*8 rho, p
     p = c*rho**gma
   END    



   Function f(x,y)
   implicit none
     real*8 x
     real*8, dimension(2) :: f, y
     f(1) = 4.d0*pi*x*x*y(2) 
     f(2) = -y(2)*y(2)*y(1)/(gma*(x + 1.d-15)**2*p(y(2)))
   END


   Subroutine rk4(x, y, h, x1, y1)
   implicit none
   real*8, intent(in)    :: h, x, y(2)
   real*8, intent(inout) :: x1, y1(2)
   real*8, dimension(2)  :: k1, k2, k3, k4
     k1 = h*f(x, y)
     
     k2 = h*f(x + h/2.d0, y + k1/2.d0)
     
     k3 = h*f(x + h/2.d0, y + k2/2.d0)

     k4 = h*f(x + h, y + k3)
     
     y1 = y + (k1 + 2.d0*k2 + 2.d0*k3 + k4)/6.d0

     x1 = x + h

   End

END PROGRAM
