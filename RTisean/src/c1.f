c data for information dimension, fixed mass
c see  H. Kantz, T. Schreiber, Nonlinear Time Series Analysis, Cambridge
c      University Press (1997)
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine c1(options_filename)
        character*30 options_filename
      parameter(nx=100000,mx=10)
      dimension x(nx,mx), icol(mx)
      character*72 file, fout
c     data kmax/100/, res/2./
	integer kmax
	real res
      data iverb/1/
      external rand
	character*255 str

	kmax=100
	res=2.0
	
	call getparam(str,options_filename)		

      call whatido("fixed mass approach to d1 estimation",iverb,str)
      id=imust("d",str)
      mfrom=imust("m",str)
      mto=imust("M",str)
      ntmin=imust("t",str)
      ncmin=imust("n",str)
      res=fcan("#",res,str)
      r=rand(sqrt(abs(fcan("I",0.0,str))))
      kmax=ican("K",kmax,str)
      resl=log(2.)/res
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)

c   to check columns subroutine
      call columns(mc,mx,icol,str)

      mcmax=max(1,mc)
      isout=igetout(fout,iverb,str)
      if(fout.eq." ") isout=1

      call nthstring(1,file,str)
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_c1")
      call outfile(fout,iunit,iverb)
      do 10 m=mfrom,mto
         write(iunit,'(4h#m= ,i5)') m
         pr=0.
         do 20 pl=log(1./(nmax-(m-1)*id)),0.,resl
            pln=pl
            call d1(nmax,mcmax,nx,x,id,m,ncmin,pr,pln,rln,ntmin,kmax)
            if(pln.eq.pr) goto 20
            it=it+1
            pr=pln
            write(iunit,*)   exp(rln), exp(pln)
 20         continue
         write(iunit,'()')
 10      write(iunit,'()')

	close(iunit)
      return
	end


