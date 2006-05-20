c simple nonlinear noise reduction
c see  H. Kantz, T. Schreiber, Nonlinear Time Series Analysis, Cambridge
c      University Press (1997)
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine lazy(options_filename)
        character*30 options_filename
      parameter(nx=100000)
      dimension x(nx), x0(nx), xc(nx)
      character*72 file, fout
c     data eps/0./, frac/0./, imax/1/
	real eps,frac
	integer imax
      data iverb/1/
	character*255 str	
	
	eps=0.0
	frac=0.0
	imax=1

	call getparam(str,options_filename)	

      call whatido("simple nonlinear noise reduction",iverb,str)
      m=imust("m",str)
      eps=fcan("r",eps,str)
      frac=fcan("v",frac,str)
      imax=ican("i",imax,str)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)
      if(eps.eq.0.and.frac.eq.0.) call usage()

      call nthstring(1,file,str)
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_l")
      call rms(nmax,x,sc,sd)
      if(frac.gt.0) eps=sd*frac
      do 10 n=1,nmax
 10      x0(n)=x(n)
      do 20 it=1,imax
         call nrlazy(nmax,x,xc,m,eps)
         if(fout.ne." ".or.isout.eq.1.or.it.eq.imax) then
            if(isout.eq.1) call suffix(fout,"c")
            call outfile(fout,iunit,iverb)
            do 30 n=1,nmax
 30            write(iunit,*) xc(n), x0(n)-xc(n)
            if(iunit.ne.istdout()) close(iunit)
            if(iv_io(iverb).eq.1) call writereport(nmax,fout)
         endif
         eps=0
         do 40 n=1,nmax
            eps=eps+(xc(n)-x(n))**2
 40         x(n)=xc(n)          
         eps=sqrt(eps/nmax)
         if(eps.eq.0.) goto 99
         if(eps.eq.0.) then
            if(iv_io(iverb).eq.1) write(istderr(),*) 
     .      'Zero correction, finished'
            goto 99
c            stop
         endif
 20      if(iv_io(iverb).eq.1) write(istderr(),*) 
     .      'New diameter of neighbourhoods is ', eps
 99   close(iunit)
      return
	end

      
      subroutine nrlazy(nmax,y,yc,m,eps)
      parameter(im=100,ii=100000000,nx=100000) 
      dimension y(nmax),yc(nmax),jh(0:im*im),jpntr(nx),nlist(nx)
      if(nmax.gt.nx) goto 999
c      if(nmax.gt.nx) stop "nrlazy: make nx larger."
      call base(nmax,y,1,m,jh,jpntr,eps)
      do 10 n=1,nmax
 10      yc(n)=y(n)   
      do 20 n=m,nmax           
         call neigh(nmax,y,y,n,nmax,1,m,jh,jpntr,eps,nlist,nfound)
         av=0
         do 30 nn=1,nfound            
 30         av=av+y(nlist(nn)-(m-1)/2)              ! average middle coordinate
 20      yc(n-(m-1)/2)=av/nfound
 999  return
      end

