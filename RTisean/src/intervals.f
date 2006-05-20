c convert event times to inter-event intervals
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine intervals(options_filename)
        character*30 options_filename
      parameter(nx=100000)
      dimension x(nx)
      character*72 file, fout
      data iverb/1/
	character*255 str	
	call getparam(str,options_filename)	

      call whatido("event time to interval conversion",iverb,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         nmax=nmax-1
         do 20 n=1,nmax
 20         x(n)=x(n+1)-x(n)
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_ss")
 10      call writefile(nmax,x,fout,iverb)
      return
	end

      
