c Statistics for time reversibility
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine timerev(options_filename)
      character*30 options_filename
      parameter(nx=100000)
      dimension x(nx)
      character*72 file,fout
      data iverb/1/
	character*255 str	
	
      call getparam(str,options_filename)	

      call whatido("time reversal asymmetry statistic",iverb,str)
      id=abs(ican("d",1,str))
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)
      call outfile(fout,iunit,iverb)


      do 10 ifi=1,1
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         t2=0
         t3=0
         do 20 n=id+1,nmax
            t2=t2+(x(n)-x(n-id))**2
 20         t3=t3+(x(n)-x(n-id))**3
 10   write(iunit,*) t3/t2
      close(iunit)
      return
	end

      
