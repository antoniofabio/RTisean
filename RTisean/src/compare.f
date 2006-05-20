c compare two data sets
c Copyright (C) T. Schreiber

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine compare(options_filename)
        character*30 options_filename
      parameter(nx=100000,mx=2)
      character*72 file,fout
      dimension x(nx,mx), icol(mx)
      data iverb/1/
	character*255 str	
	call getparam(str,options_filename)	

      call whatido("compare time series in RMS sense",iverb,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      call columns(mc,mx,icol,str)
      mcmax=mx
c   output will be written to a file (instead of the screen as done originally)	
      isout=igetout(fout,iverb,str)
      call outfile(fout,iunit,iverb)
      
	if(nstrings().ne.1) call usage()
      call nthstring(1,file,str)

      nmax=nmaxx
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"

      call rms(nmax,x(1,1),sc1,sd1)
      call rms(nmax,x(1,2),sc2,sd2)
      do 10 n=1,nmax
 10      x(n,1)=x(n,2)-x(n,1)
      call rms(nmax,x(1,1),scd,sdd)

      write(iunit,*)
      write(iunit,*) "col ", icol(1), ": Mean ", sc1, 
     .   ", standard deviation ", sd1
      write(iunit,*) "col ", icol(2), ": Mean ", sc2, 
     .   ", standard deviation ", sd2
      write(iunit,*)
      write(iunit,*) "mean difference              ", scd 
      write(iunit,*)  
     .   "root mean squared difference ", sqrt(sdd**2+scd**2) 
      write(iunit,*) "standard deviation           ", sdd
      close(iunit)
      return
	end

      
