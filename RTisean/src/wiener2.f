c Wiener filter (2): filter using periodogram in file
c Copyright (C) T. Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

      subroutine wiener2(options_filename)
      character*30 options_filename
      parameter(nx=100000)
      dimension x(nx), a(nx)
      character*72 file, fout, ffin
c     data h/1./, dh/0./
      real h,dh
      data iverb/1/
      character*255 str

      h=1.0
      dh=0.0
      call getparam(str,options_filename)	

      call whatido("Wiener filter (second part)",iverb,str)
      h=fcan("f",h,str)
      dh=fcan("w",dh,str)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isfin=igetfin(ffin,iverb,str)
      isout=igetout(fout,iverb,str)
      call nthstring(1,file,str)
      if(file.eq."-") goto 999
c      if(file.eq."-") stop "wiener2: cannot read stdin"
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      call normal(nmax,x,sc,sd)
      nmaxp=nmore(nmax)

      if(dh.eq.0.) dh=h/nmaxp
      ibin=nmaxp*dh/(2*h)
      if(ibin.gt.0) write(istderr(),*) 
     .   "wiener1: binning", 2*ibin+1," frequencies"
      if(isout.eq.1) call addsuff(fout,file,"_amp")
      if(fout.eq." ") fout="-"
      call infile(fout,iunit,iverb)
      read(iunit,*) dum, a(1)
      do 10 i=2+ibin,(nmaxp+1)/2-ibin,2*ibin+1
 10      read(iunit,*) dum, a(2*i-2)
      if(mod(nmaxp,2).eq.0)  read(iunit,*) 
     .   dum, a(nmaxp)
      d=tospec(nmaxp,a,x,ibin)
      if(iv_io(iverb).eq.1) write(istderr(),*) "rms correction: ", d
      if(isfin.eq.1) call addsuff(ffin,file,"_wc")
      do 20 n=1,nmax
         call logs("debug do 20 loop")
 20      x(n)=x(n)+sc
      call writefile(nmax,x,ffin,iverb)
      close(iunit)
 999  return
      end

      
      function igetfin(fout,iverb,str)
c gets alternate output file name, default " "
c return 1 if fout must be determined from input file name
      character*(*) fout

      igetfin=0
      call stcan("O",fout," ",str)
      if(fout.ne." ") return
      igetfin=lopt("O",1,str)
      end

