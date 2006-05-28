c Takens' estimator from c2
c Copyright (c) T. Schreiber (1998)

      parameter(meps=1000)
      dimension e(meps), c(meps), lw(meps)
      double precision a, b
      character*72 file, fout, aline
      data iverb/1/

      call whatido("Takens' estimator from correlation sum data",iverb)
      isout=igetout(fout,iverb)
      if(nstrings().eq.0) call usage()
      call nthstring(1,file)
      call infile(file,iunit,iverb)
      if(isout.eq.1) call addsuff(fout,file,"_t")
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
      call indexx(me,e,lw)
      call index2sort(me,e,lw)
      call index2sort(me,c,lw)
      cint=0
      do 10 i=2,me
         b=(e(i)*c(i-1)-e(i-1)*c(i))/(e(i)-e(i-1))
         a=(c(i)-c(i-1))/(e(i)-e(i-1))
         if(a.ne.0) then
            cint=cint+(exp(b)/a)*(exp(a*e(i))-exp(a*e(i-1)))
         else
            cint=cint+exp(b)*(e(i)-e(i-1))
         endif
 10      write(iunit2,*) exp(e(i)), exp(c(i))/cint
      write(iunit2,'()') 
      write(iunit2,'()') 
      goto 4
 999  stop
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-o outfile -V# -h] file")
      call pout("file_t")
      call pall()
      stop
      end
