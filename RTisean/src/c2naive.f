c correlation integral c2, no fast neighbour search
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine c2naive(options_filename)
        character*30 options_filename
      parameter(nx=100000,me=30,meps=800)
      dimension x(nx), c(0:meps,me)
c j=a*(log(d)-log(xmax-xmin))    d=xmax-xmin -> j=0
c a=-rs/log(2.)                  d=s*2**(-j/res)
      character*72 file, fout
c     data res/2./
	real res
      data iverb/1/
	character*255 str	
	
	res=2.0

	call getparam(str,options_filename)	

c     call whatido("correlation sum, no fast neighbour search",iverb,str)
	call whatido("correlation sum, no fast neighbour search",iverb,str)

      id=imust("d",str)
      mmax=imust("M",str)
      mmin=ican("m",1,str)
      ntmin=imust("t",str)
      ntmax=ican("T",nx,str)
      res=fcan("#",res,str)
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)
      if(fout.eq." ") isout=1

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_c2")
         call minmax(nmax,x,xmin,xmax)
         sc=xmax-xmin
         a=-res/log(2.)
         do 20 m=mmin,mmax
            do 20 j=0,meps
 20            c(j,m)=0.
         call d2naive(nmax,x,id,mmin,mmax,c,meps,log(sc),a,ntmin,ntmax)
         call outfile(fout,iunit,iverb)
         do 30 m=mmin,mmax
            write(iunit,'(4h#m= ,i5)') m
            do 40 j=meps-1,0,-1
 40            c(j,m)=c(j,m)+c(j+1,m)
            do 50 j=0,meps
               if(c(j,m).eq.0.) goto 1
 50            write(iunit,*) sc*2**(-j/res), c(j,m)/c(0,m)
 1          write(iunit,'()')
 30         write(iunit,'()')
         close(iunit)
 10      continue
	return
      end

      
      subroutine d2naive(nmax,x,id,mmin,mmax,c,meps,scl,a,ntmin,ntmax)
      parameter(nx=100000,tiny=1e-30)
      dimension x(nmax),c(0:meps,mmax),d(nx)
      if(nmax.gt.nx) goto 99 
c      if(nmax.gt.nx) stop "d2naive: make nx larger."
      nlast=min(nmax-(mmax-1)*id-1,ntmax)
      do 10 ndt=ntmin,nlast
         do 20 n=ndt+1,nmax
 20         d(n)=max(abs(x(n)-x(n-ndt)),tiny)
         do 10 n=ndt+1+(mmax-1)*id,nmax
            dmax=d(n)
            do 30 m=2,mmin-1
 30            dmax=max(dmax,d(n-(m-1)*id))
            j=int(a*(log(dmax)-scl))
            do 10 m=mmin,mmax
               if(d(n-(m-1)*id).gt.dmax) then
                  dmax=d(n-(m-1)*id)
                  j=int(a*(log(dmax)-scl))
               endif
 10            c(j,m)=c(j,m)+1
 99   return
      end

