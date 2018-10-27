        program read
10      format (A70)
20      format (I10,1p,E14.6,0p,I10)
30      format (1p,5E14.6)
        implicit double precision (a-h,o-z)
        dimension c(3,3)
        !parameter (N_max=1000000)
        !dimension a(N_max,3),v(3,3)
        
        open(10,file='HISTORY',status='old')
        open(20,file='velocity')
        write(20,*) 'timestep ' , 'avsi ', 'avo'

        read(10,10) xnoneed
        read(10,10) xnoneed
        
        DO m = 1, 60
        read(10,10) xnoneed ! skips the timestep line

        vsi = 0
        vo = 0
        DO i=1,3
                read(10,*)(c(i,j),j=1,3) !stores value in c(3,3) array
                !print *,v(i,j)
        ENDDO
        !print *,c(1,1),c(2,1),c(3,1) !prints elements of c
       
        DO k = 1,648
                read(10,10) xnoneed !Skips the info of Si atom line
                read(10,10) xnoneed !skips the coordinate line
                read(10,*) vx,vy,vz !Reads velocities
                !print *, vx,vy,vz
                
                if(k.le.216) then
                        vsi = vsi + vx**2 + vy**2 + vz**2
                        !print *, vsi
                else
                        vo = vo + vx**2 + vy**2 + vz**2
                        !print *,'Vo',vo
                endif       
        ENDDO
        
        avsi = vsi/216
        avo = vo/432
        timestep = (m-1)*50
        write(20,*) timestep , avsi, avo
        !read(10,20)n1,t,nval
        !read(10,30)(a(k),k=1,nval)
        !write(20,*)t,a(1)
        !read(10,*)(v(i,j),j=1,3)
        ENDDO
        end program read
