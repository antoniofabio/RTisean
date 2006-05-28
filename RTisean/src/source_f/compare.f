c compare two data sets
c Copyright (C) T. Schreiber

      parameter(nx=1000000,mx=2)
      character*72 file
      dimension x(nx,mx), icol(mx)
      data iverb/1/

      call whatido("compare time series in RMS sense",iverb)
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      call columns(mc,mx,icol)
      mcmax=mx
      if(nstrings().ne.1) call usage()
      call nthstring(1,file)

      nmax=nmaxx
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"

      call rms(nmax,x(1,1),sc1,sd1)
      call rms(nmax,x(1,2),sc2,sd2)
      do 10 n=1,nmax
 10      x(n,1)=x(n,2)-x(n,1)
      call rms(nmax,x(1,1),scd,sdd)

      write(istderr(),*)
      write(istderr(),*) "col ", icol(1), ": Mean ", sc1, 
     .   ", standard deviation ", sd1
      write(istderr(),*) "col ", icol(2), ": Mean ", sc2, 
     .   ", standard deviation ", sd2
      write(istderr(),*)
      write(istderr(),*) "mean difference              ", scd 
      write(istderr(),*)  
     .   "root mean squared difference ", sqrt(sdd**2+scd**2) 
      write(istderr(),*) "standard deviation           ", sdd
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-l# -x# -c#[,#] -V# -h] file")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","columns to be read (1,2)")
      call pall()
      stop
      end
