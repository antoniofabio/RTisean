c delay coordinates for periodic orbits
C Copyright (C) Thomas Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine upoembed(options_filename)
        character*30 options_filename
      parameter(nx=1000)
      dimension x(nx)
      character*72 file, fout
      data m/2/
      data iverb/1/
	character*255 str	
	call getparam(str,options_filename)	

      call whatido("embed using delay coordinates",iverb,str)
      id=imust("d",str)
      m=ican("m",m,str)
      ipp=ican("p",0,str)
      isout=igetout(fout,iverb,str)
      call nthstring(1,file,str)
      call infile(file,iunit,iverb)
      if(isout.eq.1) call addsuff(fout,file,"_delay")
      call outfile(fout,iunit2,iverb)

 1    read(iunit,*,err=1,end=999) ipor, dum1, dum2
      do 10 ip=1,ipor
 10      read(iunit,*,end=999) idum, x(ip)
      if(ipp.ne.0.and.ipor.ne.ipp) goto 1
      do 20 ip=1,ipor+1
 20      write(iunit2,*) (x(mod(ip-(j-1)*id-1+m*ipor,ipor)+1), j=m,1,-1)
      write(iunit2,'()')
      write(iunit2,'()')
      goto 1
 999  continue
	close(iunit)
	close(iunit2)
      return
	end

      


