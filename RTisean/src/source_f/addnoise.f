c add Gaussian / uniform white noise
c Copyright (C) T. Schreiber (1998)

      parameter(nx=1000000)
      character*72 file, fout
      dimension x(nx)
      external rand
      data eps/0./, frac/0./, iuni/0/
      data iverb/1/

      call whatido("add Gaussian/uniform noise",iverb)
      eps=fcan("r",eps)
      frac=fcan("v",frac)
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
      if(lopt("u",1).eq.1) iuni=1
      isout=igetout(fout,iverb)
      if(eps.eq.0.and.frac.eq.0.) call usage()

      if(lopt("0",1).eq.1.and.eps.gt.0) then
         if(isout.eq.1) fout="0_noisy"
         do 10 n=1,nmaxx
            if(iuni.eq.1) then
               x(n)=rand(0.0)*eps
            else
               x(n)=rgauss(0.0,eps)
            endif
 10         continue
         call writefile(nmaxx,x,fout,iverb)
         stop
      endif
 
      do 20 ifi=1,nstrings()
         call nthstring(ifi,file)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         call rms(nmax,x,sc,sd)
         if(frac.gt.0) eps=sd*frac
         if(iuni.eq.1) then
            if(iv_io(iverb).eq.1) write(istderr(),*) 
     .      "adding uniform noise in [0,", eps,"]"
         else
            if(iv_io(iverb).eq.1) write(istderr(),*)  
     .      "adding Gaussian noise of amplitude", eps
         endif
         if(sd.gt.0.and.iv_io(iverb).eq.1) write(istderr(),*)  
     .      "that is",eps/sd,"* rms of data"
         do 30 n=1,nmax
            if(iuni.eq.1) then
               x(n)=x(n)+rand(0.0)*eps
            else
               x(n)=x(n)+rgauss(0.0,eps)
            endif
 30         continue
         if(isout.eq.1) call addsuff(fout,file,"_noisy")
 20      call writefile(nmax,x,fout,iverb)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-r# | -v#] [-u -0 -o outfile -l# -x# -c# -V# -h] file(s)")
      call ptext("either -r or -v must be present")
      call popt("r","absolute noise level")
      call popt("v","same as fraction of standard deviation")
      call popt("u","add uniform noise (default Gaussian)")
      call popt("0","do not read input, just issue random numbers")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_noisy")
      call pall()
      stop
      end

