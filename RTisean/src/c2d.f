c local slopes from c2
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine c2d(options_filename)
        character*30 options_filename
      parameter(meps=1000)
      dimension e(meps), c(meps)
      character*72 file, fout, aline
c     data iav/1/
	integer iav
      data iverb/1/
	character*255 str	
	
	iav=1

	call getparam(str,options_filename)	

	call whatido("local slopes from c1/c2 correlation sum data",iverb,str)

      iav=ican('a',iav,str)
      isout=igetout(fout,iverb,str)
      if(nstrings().eq.0) call usage()
      call nthstring(1,file,str)
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
 999  close(iunit2)
	close(iunit)
	return
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


      


