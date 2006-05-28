c local slopes from c2
c Copyright (C) T. Schreiber (1997)

      parameter(meps=1000)
      dimension e(meps), c(meps)
      character*72 file, fout, aline
      data iav/1/
      data iverb/1/

      call whatido("local slopes from c1/c2 correlation sum data",iverb)
      iav=ican('a',iav)
      isout=igetout(fout,iverb)
      if(nstrings().eq.0) call usage()
      call nthstring(1,file)
      call infile(file,iunit,iverb)
      if(isout.eq.1) call addsuff(fout,file,"_d")
      call outfile(fout,iunit2,iverb)
 1    read(iunit,'(a)',end=999) aline
 4    if(aline(1:1).ne."#") goto 1
      if(aline(1:1).eq."#") 
     .   read(aline(index(aline,"m=")+2:72),'(i20)',err=1) m
      me=0
 2    read(iunit,'(a)') aline
      if(aline(1:72).eq." ") goto 3
      read(aline,*,err=999,end=999) ee, cc
      if(cc.le.0.) goto 3
      me=me+1
      e(me)=log(ee)
      c(me)=log(cc)
      goto 2
 3    write(iunit2,'(4h#m= ,i5)') m
      do 30 j=iav+1,me-iav
         call slope(e(j-iav),c(j-iav),2*iav+1,s)
 30      if(s.gt.0.) write(iunit2,*) exp(0.5*(e(j+iav)+e(j-iav))),  s
      write(iunit2,'()') 
      write(iunit2,'()') 
      goto 4
 999  stop
      end

      subroutine slope(x,y,n,a)
      dimension x(n),y(n)

      sx=0.
      sa=0
      a=0.
      do 10 i=1,n
 10      sx=sx+x(i)
      do 20 i=1,n
         sa=sa+(x(i)-sx/n)**2
 20      a=a+y(i)*(x(i)-sx/n)
      a=a/sa
      end


      subroutine usage()
c usage message

      call whatineed(
     .   "[-a# -o outfile -V# -h] file")
      call popt("a","average using -#,...,+# (1)")
      call pout("file_d")
      call pall()
      stop
      end


