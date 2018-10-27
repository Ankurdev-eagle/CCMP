       program calculate_pi
       implicit double precision(i)
       implicit double precision(s)
       implicit double precision(o)

       sum = 0
       DO i = 1,10000000
          s = s + (1/(i*i))
       ENDDO
       o = sqrt(6*s)
       print*, "Pi = ", o
       END 
        
