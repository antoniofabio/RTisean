c Create multivariate surrogate data
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine surrogates(options_filename)
        character*30 options_filename
      parameter(nx=10000,mx=20)
      dimension xx(nx,mx), x(nx,mx), y(nx,mx), xamp(nx,mx), 
     .   xsort(nx,mx), list(nx), icol(mx), rwork(nx)
      character*72 file, fout
c     data nsur/1/, imax/-1/
	integer nsur,imax
      external rand
      data iverb/15/
	character*255 str

	nsur=1
	imax=-1
	
	call getparam(str,options_filename)	

      call whatido("Create Multivariate Surrogate data",iverb,str)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      nsur=min(999,ican("n",nsur,str))
      imax=ican("i",imax,str)
      ispec=lopt("S",1,str)
      r=rand(sqrt(abs(fcan("I",0.0,str))))
      mcmax=ican("m",0,str)
      call columns(mc,mx,icol,str)
      if(mcmax.eq.0) mcmax=max(1,mc)
      isout=igetout(fout,iverb,str)

      call nthstring(1,file,str)
      call xreadfile(nmax,mcmax,nx,xx,nexcl,icol,file,iverb)
      nmaxp=nless(nmax)
      if(nmaxp.ne.nmax.and.iv_io(iverb).eq.1) 
     .   write(istderr(),*) "surrogates: using first ", nmaxp
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_surr")
      if(nsur.gt.1.and.isout.eq.1) call suffix(fout,"_000")

      do 10 isur=1,nsur
         if(nsur.gt.1.and.isout.eq.1) 
     .      write(fout(index(fout," ")-3:72),'(i3.3)') isur
         do 20 m=1,mcmax
            do 30 n=1,nmaxp
               x(n,m)=xx(n,m)
               y(n,m)=x(n,m)
               xamp(n,m)=x(n,m)
 30            xsort(n,m)=x(n,m)
            call store_spec(nmaxp,xamp(1,m),0)
            call sort(nmaxp,xsort(1,m),list)
            do 40 n=1,nmaxp
 40            rwork(n)=rand(0.0)
            call rank(nmaxp,rwork,list)
 20         call index2sort(nmaxp,x(1,m),list)
         it=-1
         dspec=r1mach(2)
 1       it=it+1
         do 50 m=1,mcmax
            do 50 n=1,nmaxp
 50            y(n,m)=x(n,m)
         ds0=dspec
         dspec=toxspec(nmaxp,mcmax,nx,xamp,y)
         if(imax.ge.0.and.it.ge.imax) goto 2
         do 60 m=1,mcmax
 60         call todist(nmaxp,xsort(1,m),y(1,m),x(1,m))
         if(dspec.lt.ds0) goto 1
 2       continue
         if(ispec.gt.0) then
            call xwritefile(nmaxp,mcmax,nx,y,fout,iverb)
         else
            call xwritefile(nmaxp,mcmax,nx,x,fout,iverb)
         endif
 10      if(iv_surr(iverb).eq.1) write(istderr(),*) 
     .      fout(1:index(fout," ")), ' (', it, 
     .      ' iterations, relative discrepancy ', dspec,   ')'
      return
	end

      
      function toxspec(nmax,mmax,nxx,a,x)
      parameter(nx=10000,mx=20,tol=1e-5)
      dimension x(nxx,mmax), a(nxx,mmax), w(nx,mx), w1(nx), 
     .   w2(nx), iw(15), goal(mx)
      if(nmax.gt.nx.or.mmax.gt.mx) stop "toxspec: make nx/mx larger."
      call rffti1(nmax,w2,iw)  
      do 10 m=1,mmax
         do 20 n=1,nmax
 20         w(n,m)=x(n,m)
         call rfftf1(nmax,x(1,m),w1,w2,iw)
         do 30 n=1,nmax
 30         x(n,m)=x(n,m)/real(nmax)
         x(1,m)=sqrt(a(1,m))
         do 40 n=2,(nmax+1)/2
            pha=atan2(x(2*n-1,m),x(2*n-2,m))
            x(2*n-2,m)=sqrt(a(2*n-2,m))
 40         x(2*n-1,m)=pha
 10      if(mod(nmax,2).eq.0) x(nmax,m)=sqrt(a(nmax,m))
      if(mmax.gt.1) then
         do 50 n=2,(nmax+1)/2
            do 60 m=1,mmax
 60            goal(m)=x(2*n-1,m)-a(2*n-1,m)
            alpha=alp(mmax,goal)
            do 50 m=1,mmax
 50            x(2*n-1,m)=alpha+a(2*n-1,m)
      endif
      do 70 m=1,mmax
         do 80 n=2,(nmax+1)/2
            c=x(2*n-2,m)*cos(x(2*n-1,m))
            s=x(2*n-2,m)*sin(x(2*n-1,m))
            x(2*n-1,m)=s
 80         x(2*n-2,m)=c
 70      call rfftb1(nmax,x(1,m),w1,w2,iw)
      toxspec=0
      do 90 m=1,mmax
         do 90 n=1,nmax
 90         toxspec=toxspec+(x(n,m)-w(n,m))**2
      toxspec=sqrt((toxspec/nmax)/mmax)
      end

      function alp(mmax,goal)
      dimension goal(mmax)
      data pi/3.1415926/

      f1=0
      f2=0
      do 10 m=1,mmax
         f1=f1+cos(goal(m))
 10      f2=f2+sin(goal(m))
      alp=atan2(f2,f1)
      scos=0
      do 20 m=1,mmax
 20      scos=scos+cos(alp-goal(m))
      if(scos.lt.0) alp=alp+pi
      end

      subroutine todist(nmax,dist,x,y)
      parameter(nx=10000)
      dimension x(nmax), dist(nmax), y(nmax), list(nx)
      if(nmax.gt.nx) goto 999
c      if(nmax.gt.nx) stop "todist: make nx larger."
      call rank(nmax,x,list)
      do 10 n=1,nmax
 10      y(n)=dist(list(n))
 999  return 
      end


