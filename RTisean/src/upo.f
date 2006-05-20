c locate unstable periodic points
c Copyright (C) T. Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine upo(options_filename)
        character*30 options_filename
      parameter(nx=100000,mper=20)
      dimension x(nx)
      character*72 file, fout
      common /period/ x, nmax, m, eps
c  the following line needs to be rewritten without data, as (apparently) leaving it introduces side
c  effects across repeated calls.
c     data frac/0./, iper/1/, teq/-1./, tdis/-1./, tacc/-1./, h/-1./
	real frac, teq, tdis, tacc, h
	integer iper
	data iverb/1/
	character*255 str	

	frac=0.0
	iper=1
	teq=-1.0
	tdis=-1.0
	tacc=-1.0
	h=-1.0

      
	call getparam(str,options_filename)	

      call whatido("locate unstable periodic points",iverb,str)
      m=max(imust("m",str),1)
      eps=fcan("r",0.,str)
      frac=fcan("v",frac,str)
      teq=fcan("w",teq,str)
      tdis=fcan("W",tdis,str)
      h=fcan("s",h,str)
      tacc=fcan("a",tacc,str)
      iper=ican("p",iper,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      icen=ican("n",nmaxx,str)
      isout=igetout(fout,iverb,str)
      if(eps.eq.0.and.frac.eq.0.) call usage()

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_upo_")
         call rms(nmax,x,sc,sd)
         if(frac.gt.0) eps=sd*frac
         if(teq.lt.0.) teq=eps
         if(tdis.lt.0.) tdis=eps
         if(tacc.lt.0.) tacc=eps
         if(h.lt.0.) h=eps
         if(isout.eq.1) 
     .      write(fout(index(fout,"_upo_")+5:72),'(i2.2)') iper
         call outfile(fout,iunit,iverb)
         call findupo(iper,icen,teq,tdis,tacc,h,iunit,iverb)
 10      if(iunit.ne.istdout()) close(iunit)
      return
	end


      subroutine findupo(iper,icen,teq,tdis,tacc,h,iunit,iverb)
      parameter(nx=100000,mper=20)
      external peri
      dimension x(nx),xp(mper),fvec(mper),xor(mper,nx),
     .   iw(mper),w0(mper,mper),w1(mper),w2(mper),w3(mper),
     .   w4(mper),w5(mper),w6(mper)
      common /period/ x, nmax, m, eps
      if(iper.gt.mper) goto 999
c      if(iper.gt.mper) stop "findupo: make mper larger."
      tol=sqrt(r1mach(4))
      itry=0
      ior=0
      do 10 n=iper,nmax
         if(iv_10(iverb).eq.1) then
            if(mod(n,10).eq.0) write(istderr(),'(i7)') n
         else if(iv_100(iverb).eq.1) then
            if(mod(n,100).eq.0) write(istderr(),'(i7)') n
         else if(iv_1000(iverb).eq.1) then
            if(mod(n,1000).eq.0) write(istderr(),'(i7)') n
         endif 
         if(known(n,iper,teq).eq.1) goto 10
         itry=itry+1
         if(itry.gt.icen) return
         do 20 i=1,iper
 20         xp(i)=x(n-iper+i)
         call snls1(peri,1,iper,iper,xp,fvec,w0,mper,tol,tol,0.,
     .      20*(iper+1),0.,w1,1,100.,0,info,nfev,ndum,iw,w2,w3,w4,w5,w6)
         err=enorm(iper,fvec)
         if(info.eq.-1.or.info.eq.5.or.err.gt.tacc) goto 10   ! unsuccessfull
         if(isold(iper,xp,ior,xor,tdis).eq.1) goto 10         ! already found
         ior=ior+1                                            ! a new orbit
         do 30 i=1,iper
 30         xor(i,ior)=xp(i)
         ipor=iperiod(iper,xp,tdis)
         sor=ipor*stab(iper,xp,h)/real(iper)
         call print(iper,xp,ipor,sor,err,iunit,iverb)
 10      continue
 999  return 
      end

      function known(n,iper,tol)
c return 1 if equivalent starting point has been tried
      parameter(nx=100000)
      dimension x(nx)
      common /period/ x, nmax, m, eps

      known=1
      do 10 nn=iper,n-1
         dis=0
         do 20 i=1,iper
 20         dis=dis+(x(n-iper+i)-x(nn-iper+i))**2
 10      if(sqrt(dis).lt.tol) return
      known=0
      end

      function isold(iper,xp,ior,xor,toler)
c determine if orbit is in data base
      parameter(mper=20)
      dimension xp(iper), xor(mper,*)

      isold=1
      do 10 ip=1,iper
         do 20 io=1,ior
            dor=0
            do 30 i=1,iper
 30            dor=dor+(xp(i)-xor(i,io))**2
 20            if(sqrt(dor).le.toler) return
 10      call oshift(iper,xp)
      isold=0
      end
  
      subroutine oshift(iper,xp)
c leftshift orbit circularly by one position
      dimension xp(*)

      h=xp(1)
      do 10 i=1,iper-1
 10      xp(i)=xp(i+1)
      xp(iper)=h
      end
 
      function iperiod(iper,xp,tol)
c determine shortest subperiod
      dimension xp(*)

      do 10 iperiod=1,iper
         dis=0
         do 20 i=1,iper
            il=i-iperiod
            if(il.le.0) il=il+iper
 20         dis=dis+(xp(i)-xp(il))**2
 10      if(sqrt(dis).le.tol) return
      end

      subroutine peri(iflag,mf,iper,xp,fvec,fjac,ldfjac)
c built discrepancy vector (as called by snls1)
      dimension xp(*),fvec(*)

      do 10 ip=1,iper
         fvec(ip)=xp(1)-fc(iper,xp,iflag)
 10      call oshift(iper,xp)
      end

      function fc(iper,xp,iflag)
c predict (cyclic) point 1, using iper,iper-1...
      parameter(nx=100000)
      dimension  xp(*), x(nx)
      common /period/ x, nmax, m, eps
      data cut/20/

      eps2=1./(2*eps*eps)
      ft=0
      sw=0
      fc=0
      do 10 n=m+1,nmax
         dis=0
         do 20 i=1,m
 20         dis=dis+(x(n-i)-xp(mod(m*iper-i,iper)+1))**2
         ddis=dis*eps2
         w=0
         if(ddis.lt.cut) w=exp(-ddis)
         ft=ft+w*x(n)
 10      sw=sw+w
      iflag=-1
      if(sw.eq.0) return   ! fc undefined, stop minimising
      fc=ft/sw
      iflag=1
      end

      function stab(ilen,xp,h)
c compute cycle stability by iteration of a tiny perturbation
      parameter(nx=100000,mper=20,maxit=1000)
      dimension xp(*), x(nx), xcop(mper)
      common /period/ x, nmax, m, eps

      if(mper.lt.ilen) stop "stability: make mper larger."
      iflag=1
      stab=0
      do 10 i=2,m
 10      xcop(i)=xp(mod(i-1,ilen)+1)
      xcop(1)=xp(1)+h
      do 20 it=1,maxit
         do 30 itt=1,ilen
            xx=fc(m,xcop,iflag)
            if(iflag.eq.-1) goto 1
            call oshift(m,xcop)
 30         xcop(m)=xx
         dis=0
         do 40 i=1,m
 40         dis=dis+(xcop(i)-xp(mod(i-1,ilen)+1))**2
         dis=sqrt(dis)
         stab=stab+log(dis/h)
         do 20 i=1,m
 20         xcop(i)=xp(mod(i-1,ilen)+1)*(1-h/dis) + xcop(i)*h/dis
 1    stab=stab/max(it-1,1)
      end

      subroutine print(iper,xp,ipor,sor,err,iunit,iverb)
c write orbit to iunit and to stdout
      dimension xp(*)

      write(iunit,*)
      write(iunit,*) "period / accuracy / stability"
      write(iunit,*) ipor, err, exp(sor)
      do 10 i=1,ipor
 10      write(iunit,*) i, xp(i)
      if(iv_upo(iverb).eq.0) return
      write(istderr(),*)
      write(istderr(),*) "period / accuracy / stability"
      write(istderr(),*) ipor, err, exp(sor)
      do 20 i=1,ipor
 20      write(istderr(),*) i, xp(i)
      end

