c cross-correlation integral c2x 
c see  H. Kantz, Phys.Rev.E49, 5091 (1994)
c
c Copyright (C) T. Schreiber & H. Kantz (1998)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine xc2(options_filename)
        character*30 options_filename
      parameter(nx=100000,me=30,meps=1000,mx=2)
      dimension x(nx,mx), c(me,meps), eps(meps), mdeps(meps), icol(mx)
      character*72 file, fout
c     data ipmin/1000/, res/2./, eps0/1e-30/, epsm/1e30/, id0/1/
      integer ipmin,id0
	real res,eps0,epsm
	data iverb/1/
	character*255 str	

	ipmin=1000
	res=2.0
	eps0=1e-30
	epsm=1e30
	id0=1
        call logs("debug 0.5")  
	call getparam(str,options_filename)	

      call whatido("cross correlation sum of two data sets",iverb,str)
      id=ican("d",id0,str)
      mmax=imust("M",str)
      ntmin=imust("t",str)
      ncmin=imust("n",str)
      ipmin=ican("N",ipmin,str)
      res=fcan("#",res,str)
      feps=2**(1./res)
      eps0=fcan("r",eps0,str)
      epsm=fcan("R",epsm,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      call columns(mc,mx,icol,str)
      mcmax=mx
      isout=igetout(fout,0,str)
      if(fout.eq." ") isout=1
      call nthstring(1,file,str)
      nmax=nmaxx
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_xc2")
      call minmax(nmax,x(1,1),xmin,xmax)
      call minmax(nmax,x(1,2),ymin,ymax)
      epsmax=1.001*max(xmax-xmin,ymax-ymin)
      md=mmax
      neps=0

      call logs("debug 0.9")
      do 10 epsl=log(min(epsm,epsmax)),log(eps0),-log(feps)
         neps=neps+1
         if(neps.gt.meps) goto 999 
c         if(neps.gt.meps) stop "xc2: make meps larger"
         eps(neps)=exp(epsl)
         do 20 m=1,md
 20         c(m,neps)=0
         call logs("debug 1")
         call crosscor(nmax,x(1,1),x(1,2),eps(neps),
     .      id,md,c(1,neps),ntmin,ncmin,ipmin)
         call logs("debug 2")
         do 30 m=1,md
 30         if(c(m,neps).eq.0.) goto 1
 1       md=m-1
         if(md.eq.0) goto 999
c         if(md.eq.0) stop
         mdeps(neps)=md
         call outfile(fout,iunit,iverb)
         do 40 m=1,mdeps(1)
            write(iunit,'(4h#m= ,i5)') m
            do 50 nn=1,neps
               if(mdeps(nn).lt.m) goto 2
 50            write(iunit,*) eps(nn), c(m,nn) 
 2          write(iunit,'()')
 40         write(iunit,'()')
         close(iunit)
 10   continue   

c     write(istderr(),*) eps(neps), md, c(md,neps)
 999  return
      end

      
      subroutine crosscor(nmax,x,y,eps,id,m,c,nmin,ncmin,ipmin)
      parameter(im=100,ii=100000000,nx=100000,mm=30)
      dimension y(nmax),x(nmax),xx(mm)
      dimension jh(0:im*im),ipairs(mm),c(m),jpntr(nx),nlist(nx)
      call logs("debug 3")
      if(nmax.gt.nx.or.m.gt.mm) goto 998
      call logs("debug 4")
c      if(nmax.gt.nx.or.m.gt.mm) stop "crosscor: make mm/nx larger."
      do 10 i=1,m-1
 10      ipairs(i)=0
      mbase=min(m,2)
      call logs("debug 5")
      call base(nmax,y,id,mbase,jh,jpntr,eps)
      call logs("debug 6")
      do 20 n=(m-1)*id+1,nmax
         call logs("debug 7")
         call neigh(nmax,y,x,n,id,mbase,jh,jpntr,eps,nlist,nfound)
         call logs("debug 8")
         do 30 nn=1,nfound                   ! all neighbours in two dimensions
            np=nlist(nn)
            if(np.lt.(m-1)*id+1.or.abs(np-n).le.nmin) goto 30
            ipairs(1)=ipairs(1)+1
            do 40 i=mbase,m-1
               if(abs(x(n-i*id)-y(np-i*id)).ge.eps) goto 30
 40            ipairs(i)=ipairs(i)+1            ! neighbours in 3..m dimensions
 30         continue
 20      if(n-(m-1)*id.ge.ncmin.and.ipairs(m-1).ge.ipmin) goto 1
      n=n-1
 1    s=real(n-nmin-(m-1)*id)*real(n-nmin-1-(m-1)*id) ! normalisation
      do 50 i=1,m-1
 50      if(s.gt.0.) c(i+1)=ipairs(i)/s
 998  return
      end








