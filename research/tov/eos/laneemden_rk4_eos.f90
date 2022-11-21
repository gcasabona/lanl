Program test_RK4

   integer, parameter :: i = 1000


! realtivistic tov equations >> lane emden   
! x = r, y = rho

!y'' + (2/x)y' + y = 0

   real*8 x(i), y(i,2), xf(1), yf(1), y2f(1), ymin(2), R(1)
   real*8 xl, x0, h, r0, pc
   real*8, parameter :: n = 1.d0, gma = 2.d0, kap = 100.d0
   real*8, parameter :: pi = 4.d0*DATAN(1.d0), g = 1.d0, c = 1.d0
   integer k, kl, l, s
   real*8, dimension(16) :: data(16,308)
   
   x0 = 0.d0
   xl = 20.d0 
   h = (xl - x0)/i
   x(1) = x0
   y(1,1) = 7.9e14 ! 1.d0
   y(1,2) = 0.d0
   pc = kap*y(1,1)*y(1,1)
   r0 = (pc/(2.d0*pi*y(1,1)*y(1,1)*g))**(1.d0/2.d0)

   print *,'# '
   print *,'#   Radius             Rho          dRho             Pressure '
   print *,' #-------------------------------------'
   integrate: do k=1, i - 1
      write(*,'(F9.4,5(ES23.15,1X))') x(k), y(k,1), y(k,2), p(y(k,1))
      IF (y(k,1) + h*y(k,2) < 0d0) EXIT integrate
      call RK4(x(k), y(k,1:2), h, x(k + 1), y(k + 1,1:2))
           xi = x(k)
           xf = x(k +1) 
           yi = y(k, 1)
           yf = y(k + 1, 1)
           y2i = y(k, 2)
           y2f = y(k + 1, 2)
           R = xi - yi*(xi - xf)/(yi - yf) 
   end do integrate
           
   
   
   
   print *,'# -------------------------------------'
   print *,'# '
   print *, '# Radius of star =', R
   !print *, '# Final R, rho, P', xf, yf, y2f




10 format(F9.4,F12.7,F15.7)

CONTAINS
   
   Subroutine read_data(eos)
   implicit none
     character :: eos
     !real*8, dimension(16) :: read_data !, dat
     integer :: q = 1
           open(unit=10, file = 'sfho_0.1MeV_beta.txt', status = "old", action = "read")
           do  q = 1, 308
                call skip_comments(10)
                read(unit=10,fmt=*) data(:,q)  !pressure, density , Ye, u, mu, cs, e, xn, xp, xd, xt, xh, xalp, xa, a, z
           end do
           close(unit=10)
           open(unit=20, file = 'eos.txt', status = 'old', action = "readwrite")
           do  q = 1, 308
                write(unit=20,fmt=*) data(1,q)*1.602d33/pc, (10d0**data(2,q))/y(1,1)
           end do
           close(unit=20)
           return
   END

   Subroutine skip_comments(fileUnit)
     integer, intent(in) :: fileUnit
     character(len=1) :: firstChar

       firstChar = '#'
       do while (firstChar .eq. '#')
          read(fileUnit, '(A)') firstChar
       enddo
       backspace(fileUnit)

   end subroutine skip_comments

   Function dp0(rho)
   implicit none
     real*8 rho, dp0
     integer :: q = 1
     dp0 = (data(1,2) - data(1,1))/(data(2,2) - data(2,1))

   END

   Function dpq(rho)
   implicit none
     real*8 rho, dpq
     integer :: q = 1

     dpq = (data(1,308) - data(1,307))/(data(2,308) - data(2,307))
   END

   Function dpf(rho)
   implicit none
     real*8 rho, dpf
     integer :: q = 1

     dpf = (data(1,q+1) - data(1,q-1))/(data(2,q+1) - data(2,q-1))
   END



   ! Computes RHS of the 1st order linear system:
   ! / y' = z,
   ! \ z' = -(2/x)*z - y**n
   Function f(x,y)
   implicit none
     real*8 x
     real*8, dimension(2) :: y, f
     real*8, parameter :: n = 1.d0
     f(1) = y(2)
     IF (x.EQ.0.d0) THEN
        f(2) = 2.d0 - y(1)/r0
     ELSE
        f(2) = -2.d0*f(1)/x - y(1)/(r0*r0)
     ENDIF
   END

   Function p(rho)
   implicit none
     real*8 rho, p
     p = kap*rho**gma
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
     y1 = y + (k1 + 2.d0*k2 + 2.d0*k3 + k4)/6.d0
     x1 = x + h

   End

END PROGRAM
