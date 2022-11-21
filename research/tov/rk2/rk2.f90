Program test_RK2
   implicit none
   integer, parameter :: i = 4000

! x = radius, y1 = mass, y2 = rho, p = pressure

! TOV eqns

   real*8 x(i), y(i,2), a(i), b(i), yi(1), yf(1), R(1), yim(1), yfm(1), M(1)
   real*8 xl, x0, h
   real*8, parameter :: n = 1.d0, gma = 2.d0, kap = 100.d0
   real*8, parameter :: pi = 4.d0*DATAN(1.d0), g = 1.d0, c = 1.d0 !c = 3.0d10, g = 6.67d-8
   integer :: k, kl  

   x0 =  0.d0
   xl = 1.d2 !12
   kl = i
   h = (xl - x0)/kl
   x(1) = x0
   y(1,1) = 0.d0 
   y(1,2) = 1.0d-3  !7.9d14
   

   print *,'# '
   print *,'#  Radius              Mass                  Rho                   Pressure '
   print *,' #-------------------------------------'
   h = (xl - x0)/kl*1.d-7
   write(*,'(5(ES23.15,1X))') h, y(1,1), y(1,2), p(y(1,2))
   y(2,:) = y(1,:) + h*f(0d0,y(1,:))
   h = (xl - x0)/kl
   integrate: do k=2, kl - 1
      write(*,'(5(ES23.15,1X))') x(k), y(k,1), y(k,2), p(y(k,2))
      y(k + 1, :) = y(k,:) + h*f(x(k),y(k,:))
      IF (y(k+1,2) < 0.d0) EXIT integrate
      call RK2(x(k), y(k,1:2), h, x(k + 1), y(k + 1,1:2))
   end do integrate

   yi = y(k, 2)
   yf = y(k + 1, 2)
   R  = x(k) + yi*h/(yi - yf)
   print *, '# Radius of star =', R
   yim = y(k, 1)
   yfm = y(k + 1, 1)
   M = y(k,1) + yi*h/(yi - yf)
   print *, '# Mass of star =', M


10 format(F9.4,F12.7,F15.7)



CONTAINS

   Function p(rho)
   implicit none
     real*8 rho, p
     p = kap*rho**gma
   END    

   Function f(x,y)
   implicit none
     real*8 x, rho, rho2
     real*8, dimension(2) :: f, y
     IF (x.EQ.0.d0) THEN
        rho = y(2) + h*h*rho2/2.d0
        rho2 = -4.d0*pi*y(2)*(1.d0 + kap*y(2))*(1.d0 + 3.d0*kap*y(2))
        f(1) = 4.d0*pi*h*h*(y(2) + h*h*rho2/2.d0)
        !f(2) = -4.d0*g*y(2)**3*pi*h/&
        !        (3.d0*gma*p(y(2)))
        
        f(2) = (-2.d0*pi*y(2)/kap)*(h/3.d0 + h*h*h*rho2/10.d0)*(1.d0 + kap*rho)*&
                (1.d0 + kap*rho*rho/(y(2)*(1.d0/3.d0 + h*h/10.d0)))/(1.d0 - 8.d0*pi*y(2)*h*h/3.d0) 
        
        !-4.d0*g*y(2)*y(2)*y(2)*pi*h*(1.d0 + p(y(2))/(y(2)*c*c))*(1.d0 + 3.d0*p(y(2))/(y(2)*c*c))/&
        !        (3.d0*gma*p(y(2))*(1.d0 - 8.d0*g*y(2)*pi*h*h/(3.d0*c*c)))
        !PRINT *, (1.d0 + p(y(2))/(y(2)*c*c))
        !PRINT *, (1.d0 - 8.d0*g*y(2)*pi*h*h/(3.d0*c*c))
        !PRINT *, (1.d0 + 3.d0*p(y(2))/(y(2)*c*c))
        !PRINT '(ES23.14)', f(2)
     ELSE 
        f(1) = 4.d0*pi*x*x*y(2)
        !f(2) = -y(2)*y(2)*y(1)*g/&
        !        (gma*x*x*p(y(2)))
        f(2) = -g*y(2)*y(2)*y(1)*(1.d0 + p(y(2))/(y(2)*c*c))*(1.d0 + 4.d0*pi*p(y(2))*x*x*x/(y(1)*c*c))/&
                (gma*x*x*p(y(2))*(1.d0 - 2.d0*g*y(1)/(x*c*c)))
     ENDIF
   END


   Subroutine rk2(x, y, h, x1, y1)
   implicit none
   real*8, intent(in)    :: h, x, y(2)
   real*8, intent(inout) :: x1, y1(2)
   real*8, dimension(2)  :: k1, k2, k3, k4
     k1 = h*f(x, y)
     
     k2 = h*f(x + h/2.d0, y + k1/2.d0)
     

     y1 = y + (k1 + k2)/2.d0

     x1 = x + h

   End

END PROGRAM
