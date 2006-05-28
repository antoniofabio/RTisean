c store data periodogram of x 
c if iback.ne.0 transform back to get autocorrelation instead
C Copyright (C) Thomas Schreiber (1998)

      subroutine store_spec(nmax,x,iback)
      parameter(nx=1000000)
      dimension x(nmax), w1(nx), w2(nx), iw(15)
      save w2, iw

      if(nmax.gt.nx) stop "store_spec: make nx larger."
      call rffti1(nmax,w2,iw)  
      call rfftf1(nmax,x,w1,w2,iw)
      do 10 n=1,nmax
 10      x(n)=x(n)/real(nmax)
      x(1)=x(1)**2
      do 20 n=2,(nmax+1)/2
         amp=x(2*n-2)**2+x(2*n-1)**2
         pha=atan2(x(2*n-1),x(2*n-2))
         x(2*n-2)=amp
 20      x(2*n-1)=pha
      if(mod(nmax,2).eq.0) x(nmax)=x(nmax)**2
      if(iback.eq.0) return
      do 30 n=1,nmax
 30      x(n)=x(n)*nmax
      do 40 n=2,(nmax+1)/2
 40      x(2*n-1)=0
      call rfftb1(nmax,x,w1,w2,iw)
      end
