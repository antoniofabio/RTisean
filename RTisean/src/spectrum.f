c Fourier power spectrum
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 
	
	subroutine spectrum(options_filename)
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

      call whatido("Fourier power spectrum",iverb,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      h=fcan("f",h,str)
      dh=fcan("w",dh,str)
      isout=igetout(fout,iverb,str)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_sp")
         nmaxp=nless(nmax)
         if(nmaxp.ne.nmax) 
     .      write(istderr(),*) "spectrum: using first ", nmaxp
         if(dh.eq.0.) dh=h/nmaxp
         ibin=nmaxp*dh/(2*h)
         if(ibin.gt.0) write(istderr(),*) 
     .      "spectrum: binning", 2*ibin+1," frequencies"
         call store_spec(nmaxp,x,0)
         call outfile(fout,iunit,iverb)
         write(iunit,*) 0., x(1)
         do 20 i=2+ibin,nmaxp/2+1-ibin,2*ibin+1
            p=0
            do 30 ib=i-ibin,i+ibin
 30            p=p+x(2*ib-2)
 20         write(iunit,*) h*(i-1)/real(nmaxp), p
            if(iunit.eq.istdout()) write(iunit,*)
            if(iunit.eq.istdout()) write(iunit,*)
 10         if(iunit.ne.istdout()) close(iunit)
      return
	end

      
