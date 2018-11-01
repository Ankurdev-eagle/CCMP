        program read
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
        implicit double precision (a-h,o-z)
        dimension c(3,3)
        
        open(10,file='HIS50',status='old')
        open(20,file='position')
        !write(20,*) 'Timestep   ' , 'avsi   ', 'avo   ', 'Temperature'
        
        !Ignoring first two lines
        read(10,10) xnoneed
        read(10,10) xnoneed
        
        DO m = 1, 80  !Outer loop on number of timesteps
        
        read(10,10) xnoneed ! skips the timestep line

        vsi = 0
        vo = 0
        DO i=1,3
                read(10,*)(c(i,j),j=1,3) !stores value in c(3,3) array
        ENDDO
       
        DO k = 1, 512  !loop over number of atoms
                if(k.le.1) then
                         read(10,10) xnoneed !Skips the info of Na atom line
                         read(10,*) px,py,pz !Reads velocities
                         print *, px
                else
                         read(10, 10) xnoneed
                         read(10, 10) xnoneed
                endif
        ENDDO

        timestep = (m-1)*50
        !write(20,*) timestep , avsi, avo, temp
        
        ENDDO   !Ends the outer loop on number of timesteps
        end program read
