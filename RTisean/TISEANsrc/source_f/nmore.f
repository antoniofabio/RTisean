      function nmore(n)
c find smallest factorisable number .ge.n

      nmore=n
 1    if(isfact(nmore).eq.1) return
      nmore=nmore+1
      goto 1
      end

      function nless(n)
c find largest factorisable number .le.n

      nless=n
 1    if(isfact(nless).eq.1) return
      nless=nless-1
      goto 1
      end

      function isfact(n)
c determine if n is factorisable using the first nprimes primes
      parameter(nprimes=3)
      dimension iprime(nprimes)
      data iprime/2,3,5/

      isfact=1
      ncur=n
 1    if(ncur.eq.1) return
      do 10 i=1,nprimes
         if(mod(ncur,iprime(i)).eq.0) then
            ncur=ncur/iprime(i)
            goto 1
         endif
 10      continue
      isfact=0
      end
