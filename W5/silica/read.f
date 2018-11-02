        program read
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
        implicit double precision (a-h,o-z)
        dimension c(3,3)
        
        open(10,file='HISTORY',status='old')
        open(20,file='velocity')
        write(20,*) 'Timestep   ' , 'avsi   ', 'avo   ', 'Temperature'
        
        !Ignoring first two lines
        read(10,10) xnoneed
        read(10,10) xnoneed
        
        DO m = 1, 60  !Loop over the time frame
        read(10,10) xnoneed ! skips the timestep line

        vsi = 0
        vo = 0
        DO i=1,3
                read(10,*)(c(i,j),j=1,3) !stores value in c(3,3) array
        ENDDO
       
        DO k = 1,648  !loop over number of atoms
                read(10,10) xnoneed !Skips the info of Si atom line
                read(10,10) xnoneed !skips the coordinate line
                read(10,*) vx,vy,vz !Reads velocities
                
                if(k.le.216) then
                        vsi = vsi + vx**2 + vy**2 + vz**2
                else
                        vo = vo + vx**2 + vy**2 + vz**2
                endif       
        ENDDO  !Ends loop on number of atoms
        amu = 1.6605E-27 !Define a.m.u in Kg
        zK_Si = (vsi*(10**4)*28.09*amu)/2 !Energy for Silicon atoms
        zK_O = (vo*(10**4)*16.0*amu)/2  !Energy for Oxygen atoms
        zKsys = zK_Si + zK_O  !Total System Energy
        zN = 648  !Total atoms in System
        zKb = 0.1381E-22  !Define Boltzman Constant
        temp = (2*zKsys)/(3*zN*zKb)  !Calculate temperate

        avsi = vsi/216
        avo = vo/432
        timestep = (m-1)*50
        write(20,*) timestep , avsi, avo, temp
        
        ENDDO   !Ends the outer loop on timeframe
        end program read
