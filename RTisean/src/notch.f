c notch filter in the time domain
c Copyright (C) T. Schreiber

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine notch(options_filename)
        character*30 options_filename
      parameter(nx=100000)
      dimension x(nx), y(nx)
      character*72 file, fout
c     data h/1./, w/0.01/, pi/3.1415926/
	real h,w,pi
      data iverb/1/
	character*255 str	
	
	h=1.0
	w=0.01
	pi=3.1415926

	call getparam(str,options_filename)	

      call whatido("notch filter",iverb,str)
      f=fmust("X",str)
      h=fcan("f",h,str)
      w=fcan("w",w,str)
      fw=tan(pi*f/h)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         d=fnotch(nmax,x,y,fw,w)
         if(isout.eq.1) call addsuff(fout,file,"_notch")
 10      call writefile(nmax,y,fout,iverb)
	return
      end

      function fnotch(nmax,x,y,fw,w)
      dimension x(nmax), y(nmax)

      a=(1+w*fw)**2+fw**2
      c0=   (1+fw**2)/a
      c1=-2*(1-fw**2)/a
      c2=c0
      d1= 2*(1-w**2*fw**2-fw**2)/a
      d2=  -((1-w*fw)**2+fw**2)/a

      y(1)=c0*x(1)
      y(2)=c0*x(2)+c1*x(1)+d1*y(1)
      do 10 n=3,nmax
 10      y(n)=c0*x(n)+c1*x(n-1)+c2*x(n-2)+d1*y(n-1)+d2*y(n-2)
      fnotch=0
      do 20 n=1,nmax
 20      fnotch=fnotch+(x(n)-y(n))**2
      fnotch=sqrt(fnotch/nmax)
      end

      
