c spectrum of spike trains
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine spikespec(options_filename)
        character*30 options_filename
      parameter(nx=100000)
      dimension x(nx), lx(nx), sp(nx)
      character*72 file, fout
      data iverb/1/
	character*255 str	
	call getparam(str,options_filename)	

      call whatido("spectrum of spike trains",iverb,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      fmax=fcan("F",0,str)
      nfreq=min(ican("#",0,str),nx)
      fres=fcan("w",0,str)
      inter=lopt("i",1,str)
      isout=igetout(fout,iverb,str)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(inter.eq.0) goto 1
         do 20 n=2,nmax
 20         x(n)=x(n)+x(n-1)
 1       call sort(nmax,x,lx)
         if(fmax.le.0.) fmax=2*nmax/(x(nmax)-x(1))
         if(nfreq.le.0) nfreq=fmax*(x(nmax)-x(1))/2
         write(istderr(),*) "spikespec: total time covered: ", 
     .      x(nmax)-x(1)
         write(istderr(),*) "spikespec: computing ", nfreq, 
     .      " frequencies up to ", fmax
         do 30 n=1,nfreq
            f=(n*fmax)/nfreq
 30         sp(n)=sspect(nmax,x,f)
         ibin=nfreq*fres/2
         if(ibin.gt.0) write(istderr(),*) 
     .      "spikespec: binning", 2*ibin+1," frequencies"
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_ss")
         call outfile(fout,iunit,iverb)
         do 40 n=1+ibin,nfreq-ibin,2*ibin+1
            f=(n*fmax)/nfreq
            p=0
            do 50 ib=n-ibin,n+ibin
 50            p=p+sp(ib)
 40         write(iunit,*) f, p
            if(iunit.eq.istdout()) write(iunit,*)
            if(iunit.eq.istdout()) write(iunit,*)
 10      if(iunit.ne.istdout()) close(iunit)
      return
	end

      function sspect(nmax,x,f)
      dimension x(nmax)
      data pi/3.1415926/

      omega=2*pi*f
      sr=0
      si=0
      do 10 n=1,nmax
         sr=sr+cos(omega*x(n))
 10      si=si+sin(omega*x(n))
      sspect=sr**2+si**2
      end

      
