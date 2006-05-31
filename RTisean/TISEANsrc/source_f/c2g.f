c Gaussian kernel correlation integral from c2
c Copyright (c) T. Schreiber (1997)

      parameter(meps=1000)
      dimension e(meps), c(meps), lw(meps)
      character*72 file, fout, aline
      double precision g,gd,h,d,f,func,gg,ggd,err,dum1,dum2,a,b,dc,de
      external func, funcd
      common h,d,f
      data iverb/1/

      call whatido("Gaussian kernel correlation sum from c2",iverb)
      isout=igetout(fout,iverb)
      if(nstrings().eq.0) call usage()
      call nthstring(1,file)
      call infile(file,iunit,iverb)
      if(isout.eq.1) call addsuff(fout,file,"_g")
      call outfile(fout,iunit2,iverb)
 1    read(iunit,'(a)',end=999) aline
 4    if(aline(1:1).ne."#") goto 1
      if(aline(1:1).eq."#") 
     .   read(aline(index(aline,"m=")+2:72),'(i20)',err=1) m
      me=0
 2    read(iunit,'(a)') aline
      if(aline(1:72).eq." ") goto 3
      me=me+1
      read(aline,*,err=999,end=999) ee, cc
      if(cc.le.0.) goto 3
      e(me)=log(ee)
      c(me)=log(cc)
      goto 2
 3    write(iunit2,'(4h#m= ,i5)') m
      call indexx(me,e,lw)
      call index2sort(me,e,lw)
      call index2sort(me,c,lw)
      do 10 j=1,me
         h=exp(e(j))
         g=0
         gd=0
         do 20 k=1,me-1
            f=exp((e(k+1)*c(k)-e(k)*c(k+1))/(e(k+1)-e(k)))
            d=(c(k+1)-c(k))/(e(k+1)-e(k))
            a=e(k)
            b=e(k+1)
            gg=0.
            ggd=0.
            if(b.ne.a) call dqk15(func,a,b,gg,err,dum1,dum2)
            if(b.ne.a) call dqk15(funcd,a,b,ggd,err,dum1,dum2)
            g=g+gg
 20         gd=gd+ggd
         dc=c(me)
         de=e(me)
         cgauss=g/(h**2)+exp(-exp(2*de)/(2*h**2))
         cgd=gd/(h**4)+(2+exp(2*de)/h**2)*exp(-exp(2*de)/(2*h**2))
 10      write(iunit2,*) h, cgauss, -2+cgd/cgauss
      write(iunit2,'()') 
      write(iunit2,'()') 
      goto 4
 999  stop
      end

      double precision function func(u)
      double precision h,d,f,u
      common h,d,f

      func=f*exp((2+d)*u-exp(2*u)/(2*h**2))      
      end

      double precision function funcd(u)
      double precision h,d,f,u
      common h,d,f

      funcd=f*exp((4+d)*u-exp(2*u)/(2*h**2))      
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-o outfile -V# -h] file")
      call pout("file_g")
      call pall()
      stop
      end



