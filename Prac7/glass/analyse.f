        program analyse
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
30      format (1p,5E14.6)
        implicit double precision (a-h,o-z)
        dimension a(49)
                
        open(10,file='STATIS100',status='old')
        open(20,file='vol100')
        read(10,10) xnoneed
        read(10,10) xnoneed
        write(20,*) 'Time  Volume' 
        
        
        DO i=1,31
        read(10,20)n1,t,nval
        read(10,30)(a(k),k=1,nval)
        write(20,*)t,a(19)
        
       
        end do
        end program analyse

