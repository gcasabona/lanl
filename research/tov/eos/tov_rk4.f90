! x = radius, y1 = mass, y2 = rho, p = pressure

! TOV eqns
Program test_RK4
   implicit none
   integer, parameter :: i = 1000
   real*8, dimension(16) :: sfho(16,308)
   real*8, dimension(2)  :: cgs(2,308)
   real*8 x(i), y(i,3), yi(1), yf(1), R(1), yim(1), yfm(1), M(1)
   real*8 xl, x0, h, rho
   real*8, parameter :: n = 1.d0, gma = 2.d0, kap = 100.d0 !2.3759d14
   
   !! geometric units: 
   !real*8, parameter :: pi = 4.d0*DATAN(1.d0), c = 1.d0,  g = 1.d0   !c = 3.0d10, g = 6.67d-8
   
   ! CGS units:
   real*8, parameter :: pi = 4.d0*DATAN(1.d0), c = 3.0d10, g = 6.67d-8
   integer :: k, q  
   !integer :: q, dat1, dat2 !, pressure, density , Ye, u, mu, cs, e, xn, xp, xd, xt, xh, xalp, xa, a, z
   
   character :: eos
   
   x0 = 0.d0
   xl = 5.d6 !12
   h = (xl - x0)/i*1.d-7
   x(1) = x0
   y(1,1) = 0.d0 
   y(1,2) = 1.6d13  !7.9d14 !3.6231d-14 
   call read_data(eos)
   

   print *, '# Pressure Initial', p(y(1,2))
   print *,'# '
   print *,'#  Radius              Mass                  Rho                   Pressure '
   print *,' #-------------------------------------'
   h = (xl - x0)/i!*1.d-7
   write(*,'(5(ES23.15,1X))') h, y(1,1), y(1,2), p(y(1,2))
   y(2,:) = y(1,:) + h*f(x0,y(1,:))
   h = (xl - x0)/i
   integrate: do k=2, i - 1
      write(*,'(5(ES23.15,1X))') x(k), y(k,1), y(k,2), p(y(k,2))
      y(k + 1, :) = y(k,:) + h*f(x(k),y(k,:))
      IF (y(k+1,2) < 0.d0) EXIT integrate
      call RK4(x(k), y(k,1:2), h, x(k + 1), y(k + 1,1:2))
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

   Subroutine read_data(eos)
   implicit none
     character :: eos
     !real*8, dimension(16) :: read_data !, dat
     integer :: q  !k = 1
           open(unit=10, file = 'sfho_0.1MeV_beta.txt', status = "old", action = "read")
           do  q = 1, 308
                call skip_comments(10)
                read(unit=10,fmt=*) sfho(:,q)  !pressure, density , Ye, u, mu, cs, e, xn, xp, xd, xt, xh, xalp, xa, a, z
           end do
           close(unit=10)
           open(unit=20, file = 'eos.txt', status = 'old', action = "readwrite")
           cgs(1,:) = sfho(1,:)*1.602d33
           cgs(2,:) = 10d0**sfho(2,:)
           do  q = 1, 308
                write(unit=20,fmt='(ES14.7,1X,ES14.7)') cgs(1,q), cgs(2,q)
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



   Function p(rho)
   implicit none
     real*8 p, rho
     real*8 pk, pk1, rk, rk1
     integer :: q = 250
     ! find k such that data(2,k) <= rho < data(2,k+1)
     ! ...

     ! some shortcuts
     ! Pk  = data(1,k);  Pk1 = data(1,k+1)
     ! rk  = data(2,k);  rk1 = data(2,k+1)
      
     ! interpolate
     ! P = ...

     pk  = cgs(1,q)
     pk1 = cgs(1,q+1)
     rk  = cgs(2,q)
     rk1 = cgs(2,q+1)

     p = pk + (rho - rk)*(pk1 - pk)/(rk1 - rk)
   END    

!   Function dp0(rho)
!   implicit none
!     real*8 rho, dp0
!     integer :: k
!     dp0 = (data(1,251) - data(1,250))/(data(2,251) - data(2,250))

 !  END

  ! Function dpf(rho)
  ! implicit none
  !   real*8 rho, dpf
   !  integer :: k 

    ! dpf = (data(1,308) - data(1,307))/(data(2,308) - data(2,307))
   !END  
   
   !Function dpq(rho)
   !implicit none
   !  real*8 rho, dpq
     !integer :: k
   !  dpq = (p(rho + i) - p(rho))/((rho + i) - rho)
     
     !(data(2,k+1) - data(2,k))/(data(1,k+1) - data(1,k)) 
     !(p(y(k+1,2)) - p(y(k,2)))/(y(k+1,2) - y(k,2))    !(data(2,k+250) - data(2,k+249))
     !dpq = (p(rhio + h) - p(rho - h))/2.d0
     !(data(1,k+1) - data(1,k-1))/(data(2,k+1) - data(2,k-1))
   !END

   Function f(x,y)
   implicit none
     real*8 x, rho, dp0, dpf , rk
     real*8, dimension(1) :: dpq 
     real*8, dimension(2) :: f, y
     !integer :: q , iostat
     call read_data(eos)
     !open(unit=20, file = 'eos.txt', status = 'old', action = "readwrite")
     !do k = 250, 308
     !read(unit=20, fmt=*, iostat=iostat) data(:,q)
     IF (x.EQ.0.d0) THEN
        f(1) = 4.d0*pi*h*h*y(2)
        
        f(2) = ((cgs(2,251) - cgs(2,250))/(cgs(1,251) - cgs(1,250)))*(4.d0*pi*g*y(2)*y(2)*h)*&
                (1.d0 + p(y(2))/(y(2)*c*c))*(1.d0 + 3.d0*p(y(2))/(y(2)*c*c))/&
                ((3.d0*gma*p(y(2)))*((1.d0 - 8.d0*g*y(2)*pi*h*h/(3.d0*c*c))))
                   !((cgs(2,251) - cgs(2,250))/(cgs(1,251) - cgs(1,250)))
     ELSE 
        f(1) = 4.d0*pi*x*x*y(2)
        
        f(2) = (2.d0*h/(p(y(2)+h) - p(y(2)-h)))*(-g*y(2)*y(1))*&
                (1.d0 + p(y(2))/(y(2)*c*c))*(1.d0 + 4.d0*pi*p(y(2))*x*x*x/(y(1)*c*c))/&
                ((gma*x*x)*(1.d0 - 2.d0*g*y(1)/(x*c*c)))
        
     ENDIF
     !end do
     !close(unit=20)
     !return
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
