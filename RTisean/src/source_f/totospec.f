c Force x to have spectrum a
C Copyright (C) Thomas Schreiber (1999)

      function totospec(nmax,a,x)
      parameter(nx=1000000)
      dimension x(nmax), a(nmax), w(nx), w1(nx), w2(nx), iw(15)
      save w2, iw

      if(nmax.gt.nx) stop "totospec: make nx larger."
      do 10 n=1,nmax
 10      w(n)=x(n)
      call rffti1(nmax,w2,iw)  
      call rfftf1(nmax,x,w1,w2,iw)
      do 20 n=1,nmax
 20      x(n)=x(n)/real(nmax)
      x(1)=sqrt(a(1))
      do 30 n=2,(nmax+1)/2
         ab=a(2*n-2)/(x(2*n-2)**2+x(2*n-1)**2)
         x(2*n-2)=x(2*n-2)*sqrt(ab)
 30      x(2*n-1)=x(2*n-1)*sqrt(ab)
      if(mod(nmax,2).eq.0) x(nmax)=sqrt(a(nmax))
      call rfftb1(nmax,x,w1,w2,iw)
      totospec=0
      do 40 n=1,nmax
 40      totospec=totospec+(x(n)-w(n))**2
      totospec=sqrt(totospec/nmax)
      end
