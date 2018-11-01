        program read
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
30      format (A70)
40      format (A70)
        implicit double precision (a-h,o-z)
                
        open(10,file='HIS50',status='old')
        open(20,file='position')
        open(30,file='HIS500',status='old')
        open(40,file='HIS5000',status='old')

        write(20,*) 'Timestep  ' , 'Px(50K)  ', 'Px(500K)  ', 'Px(500)'
        
        !Ignoring first two lines
        read(10,10) xnoneed
        read(10,10) xnoneed
        read(30,30) xnoneed
        read(30,30) xnoneed
        read(40,40) xnoneed
        read(40,40) xnoneed
        
        DO m = 1, 80  !Outer loop on number of timesteps
        timestep = (m-1)*50
        read(10,10) xnoneed ! skips the timestep line
        read(30,30) xnoneed
        read(40,40) xnoneed

        DO i=1,3
                read(10,10) xnoneed
                read(30,30) xnoneed
                read(40,40) xnoneed
        ENDDO
       
        DO k = 1, 512  !loop over number of atoms
		
                if(k.le.1) then
                         read(10,10) xnoneed !Skips the info of Na atom line
                         read(10,*) px_50 !Reads velocities
                         read(30,30) xnoneed !Skips the info of Na atom line
			 read(30,*) px_500
			 read(40,40) xnoneed !Skips the info of Na atom line
                         read(40,*) px_5000
                         !print *, px_50, px_500, px_5000
			 write(20,*) timestep, px_50, px_500, px_5000
                else
                         read(10, 10) xnoneed
                         read(10, 10) xnoneed
                         read(30, 30) xnoneed
                         read(30, 30) xnoneed
                         read(40, 40) xnoneed
                         read(40, 40) xnoneed
                endif
        ENDDO

        !write(20,*) timestep , , avo, temp
        
        ENDDO   !Ends the outer loop on number of timesteps
        end program read
