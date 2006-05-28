c Fourier power spectrum
c Copyright (C) T. Schreiber (1997)

      parameter(nx=1000000)
      dimension x(nx)
      character*72 file, fout
      data h/1./, dh/0./
      data iverb/1/

      call whatido("Fourier power spectrum",iverb)
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
      h=fcan("f",h)
      dh=fcan("w",dh)
      isout=igetout(fout,iverb)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file)
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
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-f# -w# -o outfile -l# -x# -c# -V# -h] file(s)")
      call popt("f","sampling rate (e.g. in Hz, default 1.)")
      call popt("w","frequency resolution (e.g. in Hz, default 1/N)")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_sp")
      call pall()
      stop
      end
