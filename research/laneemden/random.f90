f(x,y,z) = z
g(x,y,z) = (-2.d0/(x + 1.e-15))*y - z

INTEGER :: n, i
REAL :: k1, k2, k3, k4, l1, l2 ,l3 ,l4
Write (*, *) "LE Eqn"

x0 = 0.d0
y0 = 1.d0
z0 = -1.d0/3.d0
xn = 1.d0

READ (*,*) n
h = (xn - x0)/n
DO i=1, n
   k1 = h*f(x0,y0,z0)
   l1 = h*g(x0,y0,z0)
   k2 = h*f(x0 + h/2.d0, y0 + k1/2.d0, z0 + l1/2.d0)
   l2 = h*g(x0 + h/2.d0, y0 + k1/2.d0, z0 + l1/2.d0)
   k3 = h*f(x0 + h/2.d0, y0 + k2/2.d0, z0 + l2/2.d0)
   l3 = h*g(x0 + h/2.d0, y0 + k2/2.d0, z0 + l2/2.d0)
   k4 = h*f(x0 + h, y0 + k3, z0 + l3)
   l4 = h*g(x0 + h, y0 + l3, z0 + l3)
   yn = y0 + (k1 + 2.d0*k2 + 2.d0*k3 + k4)/6.d0
   zn = z0 + (l1 + 2.d0*l2 + 2.d0*l3 + l4)/6.d0
   x0 = x0 + h
   y0 = yn
   z0 = zn

END DO
WRITE(*,*) "xn, yn =", x0, y0
STOP
END
