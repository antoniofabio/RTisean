c delay coordinates
C Copyright (C) Thomas Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 
	
	subroutine delay(options_filename)
        character*30 options_filename

      parameter(nx=100000)
      dimension x(nx)
      character*72 file, fout
c   next line causes side effects and is replaced
c     data id/1/, m/2/
      integer id,m
	data iverb/1/
	character*255 str	

c   initializing default values
	id=1
	m=2

	call getparam(str,options_filename)		

      call whatido("embed using delay coordinates",iverb,str)
      id=ican("d",id,str)
      m=ican("m",m,str)
      if(m.gt.1000) then
         write(istderr(),'(a,a)') 
     .     "delay: cannot handle embedding dimension "//
     .     "larger than than 1000"
         m=1000
      endif
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)

      call nthstring(1,file,str)
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_delay")
      call outfile(fout,iunit,iverb)
      do 10 n=(m-1)*id+1,nmax
 10      write(iunit,'(1000g16.7)') (x(n-(j-1)*id), j=m,1,-1)

	close(iunit)
      return
	end

      

