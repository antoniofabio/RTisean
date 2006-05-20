c add Gaussian / uniform white noise
c Copyright (C) T. Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 
	

	subroutine addnoise(options_filename)
        character*30 options_filename
      parameter(nx=100000)
      character*72 file, fout
      dimension x(nx)
      external rand
c     data eps/0./, frac/0./, iuni/0/
	real eps,frac
	integer iuni
      data iverb/1/
	character*255 str

c   initializing defaults
	eps=0.0
	frac=0.0
	iuni=0
	
	call getparam(str,options_filename)	

      call whatido("add Gaussian/uniform noise",iverb,str)
      eps=fcan("r",eps,str)
      frac=fcan("v",frac,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      if(lopt("u",1,str).eq.1) iuni=1
      isout=igetout(fout,iverb,str)
      if(eps.eq.0.and.frac.eq.0.) call usage()

      if(lopt("0",1,str).eq.1.and.eps.gt.0) then
         if(isout.eq.1) fout="0_noisy"
         do 10 n=1,nmaxx
            if(iuni.eq.1) then
               x(n)=rand(0.0)*eps
            else
               x(n)=rgauss(0.0,eps)
            endif
 10         continue
         call writefile(nmaxx,x,fout,iverb)
         goto 99
c         stop
      endif
 
      do 20 ifi=1,nstrings()
         call nthstring(ifi,file,str)
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
      
 99   return
	end

     
