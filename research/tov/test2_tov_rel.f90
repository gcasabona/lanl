Program test_RK4
   implicit none
   integer, parameter :: i = 1000

! x = radius, y = mass, z = rho, p = pressure

! TOV eqns

   real*8 x(i), y(i), z(i), a(i), b(i), xi(1),  xf(1), yi(1), yf(1), y2f(1), ymin(2), R(1)
   real*8 xl, x0, h
   real*8, parameter :: n = 1.d0, gma = 2.d0, kap = 100.d0
   real*8, parameter :: pi = 4.d0*DATAN(1.d0), g = 1.d0, c = 1.d0 !c = 3.0d10, g = 6.67d-8
   integer :: k, kl, l  

   x0 =  0.d0
   xl = 12.d0
   kl = i
   h = (xl - x0)/kl
   x(1) = x0
   y(1) = 0.d0 
   z(1) = 5.d-4  !7.9d14
   

   print *,'# '
   print *,'#  Radius              Mass                  Rho                   Pressure '
   print *,' #-------------------------------------'
   h = (xl - x0)/kl !*1.d-7
   write(*,'(5(ES23.15,1X))') h, y(1), z(1), p(z(1))
   z(1) = y(1) + h*f(0d0,y(1),z(1))
   h = (xl - x0)/kl
   integrate: do k=2, kl - 1
      write(*,'(5(ES23.15,1X))') x(k), y(k), z(k), p(z(k))
      y(k + 1) = y(k) + h*f(x(k),y(k),z(k))
      IF (z(k+1) < 0.d0) EXIT integrate
      call RK4(x(k), y(k), z(k), h, x(k + 1), y(k + 1), z(k + 1))
   end do integrate

   !yi = y(k, 2)
   !yf = y(k + 1, 2)
   !R  = x(k) + yi*h/(yi - yf)
   !print *, '# Radius of star =', R


10 format(F9.4,F12.7,F15.7)



CONTAINS

   Function p(rho)
   implicit none
     real*8 rho, p
     p = kap*rho**gma
   END    

   Function f(x,y,z)
   implicit none
     real*8 x, y, z, h
     real*8, dimension(1) :: f
     IF (x.EQ.0.d0) THEN
        f(1) = 4.d0*pi*h*h*z(1)
     ELSE 
        f(1) = 4.d0*pi*x*x*z(1)
     ENDIF
        return
   END

   Function v(x,y,z)
   implicit none
     real*8 x, y, z, h
     real*8, dimension(1) :: v
     IF (x.EQ.0.d0) THEN
        v(1) = -4.d0*g*z(1)*z(1)*z(1)*pi*h*(1.d0 + p(z(1))/(z(1)*c*c))*(1.d0 + 3.d0*p(z(1))/(z(1)*c*c))/&
                (3.d0*gma*p(z(1))*(1.d0 - 8.d0*g*z(1)*pi*h*h/(3.d0*c*c)))
     ELSE 
        v(1) = -g*z(1)*z(1)*y(1)*(1.d0 + p(z(1))/(z(1)*c*c))*(1.d0 + 4.d0*pi*p(z(1))*x*x*x/(y(1)*c*c))/&
                (gma*x*x*p(z(1))*(1.d0 - 2.d0*g*y(1)/(x*c*c)))
     ENDIF
        return
   END



   Subroutine rk4(x, y, z, h, x1, y1, z1)
   implicit none
   real*8, intent(in)    :: h, x
   real*8, intent(in), dimension(1) :: y, z
   real*8, intent(inout) :: x1, y1, z1
   real*8, dimension(1)  :: k1, k2, k3, k4, j1, j2, j3, j4
     k1 = h*f(x, y, z)
     j1 = h*v(x, y, z)

     k2 = h*f(x + h/2.d0, y + k1/2.d0, z + j1/2.d0)
     j2 = h*v(x + h/2.d0, y + k1/2.d0, z + j1/2.d0)
     
     k3 = h*f(x + h/2.d0, y + k2/2.d0, z + j2/2.d0)
     j3 = h*v(x + h/2.d0, y + k2/2.d0, z + j2/2.d0)

     k4 = h*f(x + h, y + k3, z + j3)
     j4 = h*v(x + h, y + k3, z + j3)
     
     y1 = y + (k1 + 2.d0*k2 + 2.d0*k3 + k4)/6.d0
     z1 = z + (j1 + 2.d0*j2 + 2.d0*j3 + j4)/6.d0

     x1 = x + h

   End

END PROGRAM
