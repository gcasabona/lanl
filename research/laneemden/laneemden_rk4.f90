Program test_RK4

   integer, parameter :: i = 2000

! x = zi, y = theta

!y'' + (2/x)y' + y = 0

   real*8 X(i), Y(i,2), a(i), b(i), c(i), xf(1), ya(1), yf(1), y2f(1), ymin(2), R(1)
   real*8 xl, x0, h
   integer k, kl, l, s

   pi = 4.d0*DATAN(1.d0)
   x0 = 0.d0
   xl = 10.d0 ! 3.14159265358979d0
   kl = i
   h = (xl - x0)/kl
   X(1) = x0
   Y(1,1) = 1.d0
   Y(1,2) = 0.d0

   print *,'# '
   print *,'#     X                 Y estimated '
   print *,' #-------------------------------------'
   integrate: do k=1, kl - 1
      write(*,'(F9.4,5(ES23.15,1X))') X(k),Y(k,1),Y(k,2)
      IF (Y(k,1) + h*Y(k,2) < 0d0) EXIT integrate
      call RK4(X(k), Y(k,1:2), h, X(k + 1), Y(k + 1,1:2))
           xi = x(k)
           xf = x(k +1) !MAXVAL(x, DIM=1, MASK = x .LT.100) 
           !xf = ubound(X) 
           !ymin  = MINVAL(y(::), DIM=1)
           !ymin =  (ubound(Y, DIM=1))
           !a = y(:,1)
           !b = y(:,2)
           yi = y(k, 1)
           yf = y(k + 1, 1)
           y2i = y(k, 2)
           y2f = y(k + 1, 2)
           !yf = MINVAL(a, MASK = a .GT.0)   !ymin(1)
           !s = SIZE(k)
           !y2f = b(s)  !MINVAL(b, MASK =b .LT.0)  !ymin(2)  !   Y(ubound(Y, DIM=2)) 
           R = xi - yi*(xi - xf)/(yi - yf) 
   end do integrate
           
   
   
   
   print *,'# -------------------------------------'
   print *,'# '
   print *, '# Radius of star =', R
   print *, '# Final x, y, y''', xf, yf, y2f
   !print *, 'maxloc X ', a 
   !write(*,'(F9.4,5(ES23.15,1X))') y



   !IF (Y(k,1) + h*Y(k,2) == 0d0) THEN
    !    print *,'R =', X(k)
   !ENDIF

10 format(F9.4,F12.7,F15.7)

CONTAINS
   ! Computes RHS of the 1st order linear system:
   ! / y' = z,
   ! \ z' = -(2/x)*z - y**n
   !  where y = y(1), z = y(2)
   Function f(x,y)
   implicit none
     real*8 x, xf, yf, y2f, R
     real*8, dimension(2) :: y, f
     real*8, parameter :: n = 1.d0
     f(1) = y(2)
     IF (x.EQ.0.d0) THEN
        f(2) = 2.d0/3.d0 - y(1)**n
     ELSE
        f(2) = -2.d0/x*f(1) - y(1)**n
     ENDIF
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
