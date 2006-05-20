c autocorrelation function
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 
	
      subroutine autocor(options_filename)
      character*30 options_filename
      parameter(nx=100000)
      dimension x(2*nx)
      character*72 file, fout
      data iverb/1/
      character*255 str
      
c  here we call the routine which gets the parameter string
c  from the (temporary) file written by a C string. It is needed
c  that the file consists of the parameter string followed by \n 	
      call getparam(str,options_filename)

      call whatido("autocorrelation function",iverb,str)
      ivar=lopt('v',1,str)
      iper=lopt('p',1,str)
      iexact=lopt('P',1,str)
      if(iexact.ne.0) iper=1
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)
      
      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)

         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         if(ivar.eq.0) call normal(nmax,x,sc,sd)
         if(iper.eq.0) then
            nmaxp=nmore(2*nmax)
            do 20 n=nmax+1,nmaxp
  20            x(n)=0.
            call store_spec(nmaxp,x,1)
            do 30 n=1,nmax
  30            x(n)=x(n)/real(nmax-n+1)
         else
            nmaxp=nmax
            if(iexact.eq.0) then
               nmaxp=nless(nmax)
               if(nmaxp.ne.nmax.and.iv_io(iverb).eq.1)  
     .            write(istderr(),*) "autocor: using", nmaxp
            endif
            call store_spec(nmaxp,x,1)
            do 50 n=1,nmaxp
  50            x(n)=x(n)/real(nmaxp)
         endif
         if(isout.eq.1) call addsuff(fout,file,"_co")
         call outfile(fout,iunit,iverb)
         if(ivar.eq.0) then
             if(sd.eq.0) goto 10
c            if(sd.eq.0) stop "autocor: cannot normalise - zero variance"
            fsc=1./x(1)
         else
            fsc=1.
         endif
         do 60 n=1,min(nmax,nmaxp)
 60         write(iunit,*) n-1, fsc*x(n)
 10      if(iunit.ne.istdout()) close(iunit)
      
	return
	end

      




