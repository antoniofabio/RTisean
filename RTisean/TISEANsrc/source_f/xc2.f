c cross-correlation integral c2x 
c see  H. Kantz, Phys.Rev.E49, 5091 (1994)
c
c Copyright (C) T. Schreiber & H. Kantz (1998)

      parameter(nx=1000000,me=30,meps=1000,mx=2)
      dimension x(nx,mx), c(me,meps), eps(meps), mdeps(meps), icol(mx)
      character*72 file, fout
      data ipmin/1000/, res/2./, eps0/1e-30/, epsm/1e30/, id0/1/
      data iverb/1/

      call whatido("cross correlation sum of two data sets",iverb)
      id=ican("d",id0)
      mmax=imust("M")
      ntmin=imust("t")
      ncmin=imust("n")
      ipmin=ican("N",ipmin)
      res=fcan("#",res)
      feps=2**(1./res)
      eps0=fcan("r",eps0)
      epsm=fcan("R",epsm)
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      call columns(mc,mx,icol)
      mcmax=mx
      isout=igetout(fout,0)
      if(fout.eq." ") isout=1
      call nthstring(1,file)
      nmax=nmaxx
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_xc2")
      call minmax(nmax,x(1,1),xmin,xmax)
      call minmax(nmax,x(1,2),ymin,ymax)
      epsmax=1.001*max(xmax-xmin,ymax-ymin)
      md=mmax
      neps=0

      do 10 epsl=log(min(epsm,epsmax)),log(eps0),-log(feps)
         neps=neps+1
         if(neps.gt.meps) stop "xc2: make meps larger"
         eps(neps)=exp(epsl)
         do 20 m=1,md
 20         c(m,neps)=0
         call crosscor(nmax,x(1,1),x(1,2),eps(neps),
     .      id,md,c(1,neps),ntmin,ncmin,ipmin)
         do 30 m=1,md
 30         if(c(m,neps).eq.0.) goto 1
 1       md=m-1
         if(md.eq.0) stop
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
 10      write(istderr(),*) eps(neps), md, c(md,neps)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "-M# -n# -t# [-d# -N# -## -r# -R#"//
     .   " -o outfile -l# -x# -c#[,#] -V# -h] file")
      call popt("M","maximal embedding dimension")
      call popt("t","minimal time separation")
      call popt("n","minimal number of center points")
      call popt("d","delay (1)")
      call popt("N","maximal number of pairs (1000)")
      call popt("#","resolution, values per octave (2)")
      call popt("r",
     .   "minimal length to be probed (as long as pairs found)")
      call popt("R","maximal length to be probed (xmax-xmin)")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","columns to be read (1,2)")
      call pout("file_xc2")
      call pall()
      stop
      end

      subroutine crosscor(nmax,x,y,eps,id,m,c,nmin,ncmin,ipmin)
      parameter(im=100,ii=100000000,nx=1000000,mm=30)
      dimension y(nmax),x(nmax),xx(mm)
      dimension jh(0:im*im),ipairs(mm),c(m),jpntr(nx),nlist(nx)

      if(nmax.gt.nx.or.m.gt.mm) stop "crosscor: make mm/nx larger."
      do 10 i=1,m-1
 10      ipairs(i)=0
      mbase=min(m,2)
      call base(nmax,y,id,mbase,jh,jpntr,eps)
      do 20 n=(m-1)*id+1,nmax
         call neigh(nmax,y,x,n,id,mbase,jh,jpntr,eps,nlist,nfound)
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
      end







