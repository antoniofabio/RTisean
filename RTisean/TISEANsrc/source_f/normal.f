c utilities for normalisation of time series
c Copyright (C) T. Schreiber (1997)

      subroutine rms(nmax,x,sc,sd)
c  return mean sc and rms amplitude sd
      dimension x(nmax)

      sc=0.
      do 10 n=1,nmax
 10      sc=sc+x(n)
      sc=sc/nmax
      sd=0.
      do 20 n=1,nmax
 20      sd=sd+(x(n)-sc)**2
      sd=sqrt(sd/nmax)
      end

      subroutine normal(nmax,x,sc,sd)
c  subtract mean, return mean sc and rms amplitude sd
      dimension x(nmax)

      call rms(nmax,x,sc,sd)
      do 10 n=1,nmax
 10      x(n)=x(n)-sc
      end

      subroutine normal1(nmax,x,sc,sd)
c  subtract mean, rescale to unit variance, 
c  return mean sc and rms amplitude sd
      dimension x(nmax)

      call rms(nmax,x,sc,sd)
      if(sd.eq.0.) stop 
     .   "normal1: zero variance, cannot normalise"
      do 10 n=1,nmax
 10      x(n)=(x(n)-sc)/sd
      end

      subroutine minmax(nmax,x,xmin,xmax)
c  obtain smallest and  largest value in x
      dimension x(nmax)

      xmin=x(1)
      xmax=x(1)
      do 10 n=2,nmax
         xmin=min(x(n),xmin)
 10      xmax=max(x(n),xmax)
      end

