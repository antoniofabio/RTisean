c utilities for neighbour search
c see  H. Kantz, T. Schreiber, Nonlinear Time Series Analysis, Cambridge
c      University Press (1997)
c Copyright (C) T. Schreiber (1999)

      subroutine base(nmax,y,id,m,jh,jpntr,eps)
      parameter(im=100,ii=100000000) 
      dimension y(nmax),jh(0:im*im),jpntr(nmax)

      do 10 i=0,im*im
 10      jh(i)=0
      do 20 n=(m-1)*id+1,nmax                                  ! make histogram
         i=mod(int(y(n)/eps)+ii,im)
         if(m.gt.1) i=im*i+mod(int(y(n-(m-1)*id)/eps)+ii,im)
 20      jh(i)=jh(i)+1
      do 30 i=1,im*im                                           ! accumulate it
 30      jh(i)=jh(i)+jh(i-1)
      do 40 n=(m-1)*id+1,nmax                           ! fill list of pointers

         i=mod(int(y(n)/eps)+ii,im)
         if(m.gt.1) i=im*i+mod(int(y(n-(m-1)*id)/eps)+ii,im)
         jpntr(jh(i))=n
 40      jh(i)=jh(i)-1
      end

      subroutine neigh(nmax,y,x,n,nlast,id,m,jh,jpntr,eps,nlist,nfound)
      parameter(im=100,ii=100000000) 
      dimension y(nmax),x(nmax),jh(0:im*im),jpntr(nmax),nlist(nmax)

      nfound=0
      kloop=1
      call logs("debug neigh start")
      if(m.eq.1) kloop=0
      call logs("debug neigh 1")
      jj=int(y(n)/eps)
      call logs("debug neigh 2")

      kk=int(y(n-(m-1)*id)/eps)
      do 10 j=jj-1,jj+1                               ! scan neighbouring boxes
         do 20 k=kk-kloop,kk+kloop
            jk=mod(j+ii,im)
            if(m.gt.1) jk=im*jk+mod(k+ii,im)
            do 30 ip=jh(jk+1),jh(jk)+1,-1               ! this is in time order
               np=jpntr(ip)
               if(np.gt.nlast) goto 20
               do 40 i=0,m-1
 40               if(abs(y(n-i*id)-x(np-i*id)).ge.eps) goto 30
               nfound=nfound+1
               nlist(nfound)=np                       ! make list of neighbours
 30            continue
 20         continue
 10      continue
      end

c versions for multivariate series
c Copyright (C) T. Schreiber (1999)

      subroutine mbase(nmax,mmax,nxx,y,id,m,jh,jpntr,eps)
      parameter(im=100,ii=100000000) 
      dimension y(nxx,mmax),jh(0:im*im),jpntr(nmax)

      if(mmax.eq.1) then
         call base(nmax,y,id,m,jh,jpntr,eps)
         return
      endif
      mt=(m-1)/mmax+1
      do 10 i=0,im*im
 10      jh(i)=0
      do 20 n=(mt-1)*id+1,nmax                                 ! make histogram
        i=im*mod(int(y(n,1)/eps)+ii,im)+mod(int(y(n,mmax)/eps)+ii,im)
 20     jh(i)=jh(i)+1
      do 30 i=1,im*im                                          ! accumulate it
 30     jh(i)=jh(i)+jh(i-1)
      do 40 n=(mt-1)*id+1,nmax                          ! fill list of pointers
        i=im*mod(int(y(n,1)/eps)+ii,im)+mod(int(y(n,mmax)/eps)+ii,im)
        jpntr(jh(i))=n
 40     jh(i)=jh(i)-1
      end

      subroutine mneigh(nmax,mmax,nxx,y,n,nlast,id,m,jh,jpntr,eps,
     .   nlist,nfound)
      parameter(im=100,ii=100000000) 
      dimension y(nxx,mmax),jh(0:im*im),jpntr(nmax),nlist(nmax)

      if(mmax.eq.1) then
         call neigh(nmax,y,y,n,nlast,id,m,jh,jpntr,eps,nlist,nfound)
         return
      endif
      mt=(m-1)/mmax+1
      nfound=0
      jj=int(y(n,1)/eps)
      kk=int(y(n,mmax)/eps)
      do 10 j=jj-1,jj+1                               ! scan neighbouring boxes
         do 20 k=kk-1,kk+1
            jk=im*mod(j+ii,im)+mod(k+ii,im)
            do 30 ip=jh(jk+1),jh(jk)+1,-1               ! this is in time order
               np=jpntr(ip)
               if(np.gt.nlast) goto 20
               mcount=0
               do 40 i=mt-1,0,-1
                  do 40 is=1,mmax
                     mcount=mcount+1
                     if(mcount.gt.m) goto 1
 40                  if(abs(y(n-i*id,is)-y(np-i*id,is)).ge.eps) goto 30
 1             nfound=nfound+1
               nlist(nfound)=np                       ! make list of neighbours
 30            continue
 20         continue
 10      continue
      end


