c Wiener filter (1): write periodogram to file
c Copyright (C) T. Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine wiener1(options_filename)
      character*30 options_filename
      parameter(nx=100000)
      dimension x(nx)
      character*72 file, fout
c     data h/1./, dh/0./
      real h,dh
      data iverb/1/
      character*255 str

      h=1.0
      dh=0.0
	
	
      call getparam(str,options_filename)	

      call whatido("Wiener filter (first part)",iverb,str)
      h=fcan("f",h,str)
      dh=fcan("w",dh,str)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)

      call nthstring(1,file,str)
      if(file.eq."-") goto 999
c      if(file.eq."-") stop "wiener1: cannot read stdin"
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      call normal(nmax,x,sc,sd)
      nmaxp=nmore(nmax)
      if(nmaxp.ne.nmax) then 
c        write(istderr(),*) "wiener1: padding zeroes to ", nmaxp
         do 10 n=nmax+1,nmaxp
 10         x(n)=0.
      endif
      if(isout.eq.1) call addsuff(fout,file,"_amp")
      call outfile(fout,iunit,iverb)
      if(dh.eq.0.) dh=h/nmaxp
      ibin=nmaxp*dh/(2*h)
      if(ibin.gt.0) write(istderr(),*) 
     .   "wiener1: binning", 2*ibin+1," frequencies"
      call store_spec(nmaxp,x,0)
      write(iunit,*) 0., x(1)
      do 20 i=2+ibin,(nmaxp+1)/2-ibin,2*ibin+1
         p=0
         do 30 ib=i-ibin,i+ibin
 30         p=p+x(2*ib-2)
 20      write(iunit,*) h*(i-1)/real(nmaxp), p
      if(mod(nmaxp,2).eq.0)  write(iunit,*) 
     .   h*(nmaxp-1)/real(nmaxp), x(nmaxp)
c     if(iunit.ne.istdout()) write(istderr(),*)  
c    .   'Now edit periodogram in file ', fout(1:index(fout," ")-1)
	close(iunit)
 999  return
	end

      
