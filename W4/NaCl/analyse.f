        program analyse
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
30      format (1p,5E14.6)
        implicit double precision (a-h,o-z)
        dimension a(49)
        !parameter (N_max=1000000)
        !dimension a(N_max,3),v(3,3)
        
        open(10,file='STATIS',status='old')
        open(20,file='value')
        read(10,10) xnoneed
        read(10,10) xnoneed
        
        DO i=1,30
        read(10,20)n1,t,nval
        read(10,30)(a(k),k=1,nval)
        write(20,*)t,a(1)
        ENDDO
        end program analyse
