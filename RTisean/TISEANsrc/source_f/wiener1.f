c Wiener filter (1): write periodogram to file
c Copyright (C) T. Schreiber (1998)

      parameter(nx=1000000)
      dimension x(nx)
      character*72 file, fout
      data h/1./, dh/0./
      data iverb/1/

      call whatido("Wiener filter (first part)",iverb)
      h=fcan("f",h)
      dh=fcan("w",dh)
      nmax=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
      isout=igetout(fout,iverb)

      call nthstring(1,file)
      if(file.eq."-") stop "wiener1: cannot read stdin"
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      call normal(nmax,x,sc,sd)
      nmaxp=nmore(nmax)
      if(nmaxp.ne.nmax) then 
         write(istderr(),*) "wiener1: padding zeroes to ", nmaxp
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
      if(iunit.ne.istdout()) write(istderr(),*)  
     .   'Now edit periodogram in file ', fout(1:index(fout," ")-1)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-f# -w# -o outfile -l# -x# -c# -V# -h] file")
      call ptext("then edit file_amp and run: "//
     .   "wiener2 [-f# -w# -o outfile -l# -x# -c# -V# -h] file")
      call popt("f","sampling rate (e.g. in Hz, default 1.)")
      call popt("w","frequency resolution (e.g. in Hz, default 1/N)")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_amp")
      call pall()
      call ptext("Note: ""-"" not accepted as file")
      write(istderr(),'()') 
      stop
      end
