c Wiener filter x according to spectrum a
C Copyright (C) Thomas Schreiber (1998)

      function tospec(nmax,a,x,ibin)
      parameter(nx=1000000)
      dimension x(nmax), a(nmax), w(nx), w1(nx), w2(nx), iw(15)
      save w2, iw

      if(nmax.gt.nx) stop "tospec: make nx larger."
      do 10 n=1,nmax
 10      w(n)=x(n)
      call rffti1(nmax,w2,iw)  
      call rfftf1(nmax,x,w1,w2,iw)
      do 20 n=1,nmax
 20      x(n)=x(n)/real(nmax)
      x(1)=x(1)*(a(1)/x(1)**2)
      do 30 i=2+ibin,(nmax+1)/2-ibin,2*ibin+1
         p=0
         do 40 ib=i-ibin,i+ibin
 40         p=p+x(2*ib-2)**2+x(2*ib-1)**2
         ab=a(2*i-2)/p
         do 30 ib=i-ibin,i+ibin
            x(2*ib-2)=x(2*ib-2)*ab
 30         x(2*ib-1)=x(2*ib-1)*ab
      if(mod(nmax,2).eq.0) x(nmax)=x(nmax)*(a(nmax)/x(nmax)**2)
      call rfftb1(nmax,x,w1,w2,iw)
      tospec=0
      do 50 n=1,nmax
 50      tospec=tospec+(x(n)-w(n))**2
      tospec=sqrt(tospec/nmax)
      end
