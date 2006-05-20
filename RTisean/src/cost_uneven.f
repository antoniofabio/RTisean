c cost function
c binned autocorrelation function of unevenly sampled data
c Copyright (C) T. Schreiber (1999)

c-------------------------------------------------------------------
c get cost function specific options
c
      subroutine opts_cost_uneven(ncol)
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist)
      character*80 filet
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight

      iweight=ican('W',0)
      bininv=1./fmust("d")
      totbin=fmust("D")
      nbin=min(int(totbin*bininv)+1,nhist)
      ncol=2
      end

c-------------------------------------------------------------------
c print version information on cost function
c
      subroutine what_cost_uneven()
      call ptext("Cost function: binned autocorrelation function")
      end

c-------------------------------------------------------------------
c print cost function specific usage message
c
      subroutine usage_cost_uneven()
      call ptext("Cost function options: -d# -D# [-W#]")
      call popt("d","time span of one bin")
      call popt("D","total time spanned")
      call popt("W",
     .   "average: 0=max(c) 1=|c|/lag 2=(c/lag)**2 (0)")
      end

c-------------------------------------------------------------------
c initialise all that is needed for cost function
c
      subroutine cost_init_uneven()
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist), x(nx,2)
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight
      common nmax,cost,temp,cmin,rate,x

      do 10 i=1,nbin
 10      hnorm(i)=0.
      do 20 n1=1,nmax
         do 30 n2=n1,nmax
            il=int((x(n2,2)-x(n1,2))*bininv)+1
            if(il.gt.nbin) goto 20
 30         hnorm(il)=hnorm(il)+1.
 20      continue
      do 40 i=1,nbin
 40      if(hnorm(i).gt.0.) hnorm(i)=1./hnorm(i)
      call sauto_uneven(nbin,bininv,h0)
      end

c-------------------------------------------------------------------
c initial transformation on time series and its inverse
c here: sort by increasing sample times, no inversion necessary
c also normalise to unit variance, zero mean
c
      subroutine cost_transform_uneven(nmax,mcmax,nxdum,x)
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist)
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight
      dimension x(nxdum,2), lx(nx)

      call indexx(nmax,x(1,2),lx)
      call index2sort(nmax,x(1,2),lx)
      call index2sort(nmax,x,lx)
      call normal1(nmax,x,sc,sd)
      end

      subroutine cost_inverse_uneven(nmax,mcmax,nxdum,x,y)
      dimension x(nxdum,2), y(nxdum,2)
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist)
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight

      do 10 n=1,nmax
         y(n,2)=x(n,2)
 10      y(n,1)=x(n,1)*sd+sc
      end

c-------------------------------------------------------------------
c compute full cost function from scratch
c
      subroutine cost_full_uneven(iv)
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist)
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight
      common nmax,cost

      call sauto_uneven(nbin,bininv,h)
      cost=aver_uneven(h0,h)
      if(iv.ne.0) call dump_uneven()
      end

c-------------------------------------------------------------------
c compute changed cost function on exchange of n1 and n2 
c
      subroutine cost_update_uneven(nn1,nn2,cmax,iaccept,iv)
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist)
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight
      dimension hcop(nhist), x(nx,2)
      common nmax,cost,temp,cmin,rate,x

      n1=min(nn1,nn2)
      n2=max(nn1,nn2)
      comp=0
      iaccept=0
      do 10 i=1,nbin
 10      hcop(i)=h(i)
      do 20 nn=n1-1,1,-1
         il=int((x(n1,2)-x(nn,2))*bininv)+1
         if(il.gt.nbin) goto 1
 20      hcop(il)=hcop(il)-x(n1,1)*x(nn,1)
 1    continue
      do 30 nn=n1,nmax
         il=int((x(nn,2)-x(n1,2))*bininv)+1
         if(il.gt.nbin) goto 2
 30      if(nn.ne.n2) hcop(il)=hcop(il)-x(nn,1)*x(n1,1)
 2    continue
      do 40 nn=n2-1,1,-1
         il=int((x(n2,2)-x(nn,2))*bininv)+1
         if(il.gt.nbin) goto 3
 40      hcop(il)=hcop(il)-x(n2,1)*x(nn,1)
 3    continue
      do 50 nn=n2,nmax
         il=int((x(nn,2)-x(n2,2))*bininv)+1
         if(il.gt.nbin) goto 4
 50      hcop(il)=hcop(il)-x(nn,1)*x(n2,1)
 4    call exch_random(n1,n2)
      do 60 nn=n1-1,1,-1
         il=int((x(n1,2)-x(nn,2))*bininv)+1
         if(il.gt.nbin) goto 5
 60      hcop(il)=hcop(il)+x(n1,1)*x(nn,1)
 5    continue
      do 70 nn=n1,nmax
         il=int((x(nn,2)-x(n1,2))*bininv)+1
         if(il.gt.nbin) goto 6
 70      if(nn.ne.n2) hcop(il)=hcop(il)+x(nn,1)*x(n1,1)
 6    continue
      do 80 nn=n2-1,1,-1
         il=int((x(n2,2)-x(nn,2))*bininv)+1
         if(il.gt.nbin) goto 7
 80      hcop(il)=hcop(il)+x(n2,1)*x(nn,1)
 7    continue
      do 90 nn=n2,nmax
         il=int((x(nn,2)-x(n2,2))*bininv)+1
         if(il.gt.nbin) goto 8
 90      hcop(il)=hcop(il)+x(nn,1)*x(n2,1)
 8    comp=aver_uneven(h0,hcop)
      if(comp.ge.cmax) then
         call exch_random(n1,n2)
         return
      endif
      cost=comp  ! if got here: accept
      iaccept=1
      if(iv.ne.0) call logs("panic(hcop)")
      do 100 i=1,nbin
 100     h(i)=hcop(i)
      end

c-------------------------------------------------------------------
c compute autocorrealtion from scratch
c
      subroutine sauto_uneven(nbin,bininv,h)
      parameter(nx=100000)
      dimension h(*)
      common nmax,cost,temp,cmin,rate,x
      dimension x(nx,2)

      do 10 i=1,nbin
 10      h(i)=0
      do 20 n1=1,nmax
         do 30 n2=n1,nmax
            il=int((x(n2,2)-x(n1,2))*bininv)+1
            if(il.gt.nbin) goto 20
 30         h(il)=h(il)+x(n2,1)*x(n1,1)
 20      continue
      end

c-------------------------------------------------------------------
c weighted average of autocorrelation 
c
      function aver_uneven(h1,h2)
      parameter(nhist=100000,nx=100000)
      dimension hnorm(nhist), h0(nhist), h(nhist), 
     .   h1(*), h2(*)
      common /costcom/ bininv, nbin, hnorm, h0, h, sd, sc, iweight

      aver_uneven=0
      if(iweight.eq.0) then
         do 10 i=1,nbin
 10         aver_uneven=max(aver_uneven,abs((h1(i)-h2(i))*hnorm(i)))
      else if(iweight.eq.1) then
         do 20 i=1,nbin
 20         aver_uneven=aver_uneven+abs((h1(i)-h2(i))*hnorm(i))/real(i)
      else if(iweight.eq.2) then
         do 30 i=1,nbin
 30         aver_uneven=aver_uneven+((h1(i)-h2(i))*hnorm(i))**2/real(i)
      endif
      end

c-------------------------------------------------------------------
c diagnostic output
c
      subroutine dump_uneven()
      end

