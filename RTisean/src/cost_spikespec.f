c cost function
c spike train power spectrum
c Copyright (C) T. Schreiber (1999)

c-------------------------------------------------------------------
c get cost function specific options
c
      subroutine opts_cost_spikespec(ncol)
      parameter(mfreq=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight

      iweight=ican('W',0)
      fmax=fcan("F",0)
      nfreq=ican("#",0)
      inter=lopt("i",1)
      ncol=1
      end

c-------------------------------------------------------------------
c print version information on cost function
c
      subroutine what_cost_spikespec()
      call ptext("Cost function: spike train power spectrum")
      end

c-------------------------------------------------------------------
c print cost function specific usage message
c
      subroutine usage_cost_spikespec()
      call ptext("Cost function options: [-F# -## -w# -i]")
      call popt("W",
     .   "average: 0=max(s) 1=|s|/f 2=(s/f)**2 3=|s| (0)")
      call popt("F","maximal frequency (2*l / total time)")
      call popt("#","number of frequencies (F* total time /2)")
      call popt("w","frequency resolution (0)")
      call popt("i","expect intervals rather than times")
      end

c-------------------------------------------------------------------
c initialise all that is needed for cost function
c
      subroutine cost_init_spikespec()
      parameter(nx=100000,mfreq=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight
      dimension x(nx)
      common nmax,cost,temp,cmin,rate,x

      if(fmax.le.0.) fmax=2*nmax/(x(nmax)-x(1))
      if(nfreq.le.0) nfreq=fmax*(x(nmax)-x(1))/2
      if(nfreq.gt.mfreq) write(istderr(),'(a)') 
     .   "truncated to ", mfreq," frequencies"
      nfreq=min(mfreq,nfreq)
      write(istderr(),*) "randomize_spikespec: total time covered: ", 
     .   x(nmax)-x(1)
      write(istderr(),*) "randomize_spikespec: computing ", nfreq, 
     .   " frequencies up to ", fmax
      call sspect_spikespec(nfreq,fmax/nfreq,sp0r,sp0i,sp0)
      end

c-------------------------------------------------------------------
c initial transformation on time series and its inverse
c
      subroutine cost_transform_spikespec(nmax,mcmax,nxdum,x)
      parameter(mfreq=100000,nx=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight
      dimension x(nx), lx(nx)

      if(inter.eq.0) goto 1
      do 10 n=2,nmax
 10      x(n)=x(n)+x(n-1)
 1    call sort(nmax,x,lx)
      end

      subroutine cost_inverse_spikespec(nmax,mcmax,nxdum,x,y)
      parameter(mfreq=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight
      dimension x(nmax), y(nmax)
      
      do 10 n=1,nmax
 10      y(n)=x(n)
      if(inter.ne.1) return
      do 20 n=nmax,2,-1
 20      y(n)=y(n)-y(n-1)
      end

c-------------------------------------------------------------------
c compute full cost function from scratch
c
      subroutine cost_full_spikespec(iv)
      parameter(mfreq=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight
      common nmax,cost

      call sspect_spikespec(nfreq,fmax/nfreq,spr,spi,sp)

      cc=0
      do 10 i=1,nfreq
 10      call aver_spikespec(cc,sp(i)-sp0(i),i)
      cost=cc
      if(iv.ne.0) call dump_spikespec()
      end

c-------------------------------------------------------------------
c compute changed cost function on exchange of n1 and n2 
c
      subroutine cost_update_spikespec(n1,n2,cmax,iaccept,iv)
      parameter(mfreq=100000,nx=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight
      dimension sprcop(mfreq), spicop(mfreq), spcop(mfreq), x(nx)
      common nmax,cost,temp,cmin,rate,x
      data pi/3.1415926/

      comp=0
      iaccept=0
      do 10 i=1,nfreq
         f=i*(fmax/nfreq)
         omega=2*pi*f
         xx=x(n1-1)+x(n1+1)-x(n1)
         sprcop(i)=spr(i)-cos(omega*x(n1))+cos(omega*xx)
         spicop(i)=spi(i)-sin(omega*x(n1))+sin(omega*xx)
         spcop(i)=sprcop(i)**2+spicop(i)**2
         call aver_spikespec(comp,sp0(i)-spcop(i),i)
 10      if(comp.ge.cmax) return
      cost=comp  ! if got here: accept
      iaccept=1
      call exch_event(n1,n2)
      if(iv.ne.0) call logs("panic(spcop)")
      do 20 i=1,nfreq
         spr(i)=sprcop(i)
         spi(i)=spicop(i)
 20      sp(i)=spcop(i)
      end

c-------------------------------------------------------------------
c compute spectrum from scratch
c
      subroutine sspect_spikespec(nfreq,fres,spr,spi,sp)
      parameter(nx=100000)
      dimension spr(*), spi(*), sp(*), x(nx)
      common nmax,cost,temp,cmin,rate,x
      data pi/3.1415926/

      do 10 i=1,nfreq
         f=i*fres
         omega=2*pi*f
         sr=0
         si=0
         do 20 n=1,nmax
            sr=sr+cos(omega*x(n))
 20         si=si+sin(omega*x(n))
         spr(i)=sr
         spi(i)=si
 10      sp(i)=sr**2+si**2
      end

c-------------------------------------------------------------------
c weighted average of autocorrelation 
c
      subroutine aver_spikespec(cav,dc,n)
      parameter(mfreq=100000)
      dimension sp0r(mfreq), sp0i(mfreq), spr(mfreq), spi(mfreq),
     .   sp0(mfreq), sp(mfreq)
      common /costcom/ nfreq, fmax, inter,
     .    sp0r, sp0i, sp0, spr, spi, sp, sd, sc, iweight

      if(iweight.eq.0) then
         cav=max(cav,abs(dc))    ! max (L-infinity) norm
      else if(iweight.eq.1) then
         cav=cav+abs(dc)/n       ! L-1 norm (sum of moduli), weighted by freq.
      else if(iweight.eq.2) then
         cav=cav+(dc/n)**2       ! L-2 norm (sum of squares), weighted by freq.
      else if(iweight.eq.2) then
         cav=cav+abs(dc)         ! L-1 norm (sum of moduli)
      endif
      end

c-------------------------------------------------------------------
c diagnostic output
c
      subroutine dump_spikespec()
      end

