c embed using principal components
C Copyright (C) Thomas Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine pc(options_filename)
        character*30 options_filename
      parameter(nx=100000, me=500)
      dimension x(nx), c(me,me), d(me), xc(me), z(me,me)
      character*72 file, fout
c     data id/1/, isvd/2/
	integer id, isvd
      data iverb/1/
	character*255 str

	id=1
	isvd=2
	
	call getparam(str,options_filename)	

      call whatido("embed using principal components",iverb,str)
      m=imust("m",str)
      if(m.gt.me) goto 999
c      if(m.gt.me) stop "svd: make me larger."
      id=ican("d",id,str)
      isvd=min(ican("q",isvd,str),m)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)

      call nthstring(1,file,str)
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      call normal(nmax,x,sc,sd)
      call svd_vectors(nmax,m,id,x,c,z,d)
      if(iv_io(iverb).eq.1) write(istderr(),*) 
     .   "#, fraction of variance, accumulative fraction"
      ctot=0.
      do 10 i=1,m
 10      ctot=ctot+d(m+1-i)
      cacc=0.
      do 20 i=1,m
         cacc=cacc+d(m+1-i)
 20      if(iv_io(iverb).eq.1) 
     .      write(istderr(),*) i, d(m+1-i)/ctot, cacc/ctot
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_pc")
      call outfile(fout,iunit,iverb)
      do 30 n=(m-1)*id+1,nmax
         do 40 i=1,isvd
            s=0
            do 50 j=1,m
 50            s=s+z(j,m+1-i)*x(n-(j-1)*id)
 40         xc(i)=s
 30      write(iunit,*) (xc(i),i=1,isvd)
	close(iunit)
 999  return
	end

      
      subroutine svd_vectors(nmax,m,id,x,c,z,d)
      parameter(me=500)
      dimension x(nmax), c(me,*), d(m), w1(me), w2(me), z(me,*)
      if(m.gt.me) goto 998
c      if(m.gt.me) stop "svd_vectors: make me larger."
      do 10 i=1,m
         do 10 j=i,m
            s=0.
            do 20 n=(m-1)*id+1,nmax
 20            s=s+x(n-(i-1)*id)*x(n-(j-1)*id)
            c(i,j)=s/(nmax-(m-1)*id)
 10         c(j,i)=c(i,j)
      call rs(me,m,c,d,1,z,w1,w2,ierr)
 998  return
      end

