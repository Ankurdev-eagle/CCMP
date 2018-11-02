        program read
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
30      format (A70)
40      format (A70)
        implicit double precision (a-h,o-z)
                
        open(10,file='HIS50',status='old') !Open HIS50 file
        open(20,file='position')  !Open file to store data
        open(30,file='HIS500',status='old') !Open HIS500 file
        open(40,file='HIS5000',status='old') !Open HIS5000 file

        write(20,*) 'Timestep  ' , 'Px(50K)  ', 'Px(500K)  ', 'Px(500)'
        
        !Ignoring first two lines of each file
        read(10,10) xnoneed
        read(10,10) xnoneed
        read(30,30) xnoneed
        read(30,30) xnoneed
        read(40,40) xnoneed
        read(40,40) xnoneed
        
        DO m = 1, 80  !Loop over entire timeframe
        !Do this for all 3 files
        timestep = (m-1)*50
        
        ! skips the timestep line
        read(10,10) xnoneed
        read(30,30) xnoneed
        read(40,40) xnoneed

        !ignore box size info
        DO i=1,3
                read(10,10) xnoneed
                read(30,30) xnoneed
                read(40,40) xnoneed
        ENDDO
       
        DO k = 1, 512  !loop over number of atoms
                
                if(k.le.1) then
                         read(10,10) xnoneed !Skips the info of Na atom line
                         read(10,*) px_50 !Reads position Px from HIS50
                         read(30,30) xnoneed !Skips the info of Na atom line
                         read(30,*) px_500
                         read(40,40) xnoneed !Skips the info of Na atom line
                         read(40,*) px_5000
                         !print *, px_50, px_500, px_5000
                         write(20,*) timestep, px_50, px_500, px_5000
                else !ignore rest of it
                         read(10, 10) xnoneed
                         read(10, 10) xnoneed
                         read(30, 30) xnoneed
                         read(30, 30) xnoneed
                         read(40, 40) xnoneed
                         read(40, 40) xnoneed
                endif
        ENDDO

        ENDDO   !Ends the outer loop on timeframes
        end program read
