c delay coordinates for periodic orbits
C Copyright (C) Thomas Schreiber (1998)

      parameter(nx=1000)
      dimension x(nx)
      character*72 file, fout
      data m/2/
      data iverb/1/

      call whatido("embed using delay coordinates",iverb)
      id=imust("d")
      m=ican("m",m)
      ipp=ican("p",0)
      isout=igetout(fout,iverb)
      call nthstring(1,file)
      call infile(file,iunit,iverb)
      if(isout.eq.1) call addsuff(fout,file,"_delay")
      call outfile(fout,iunit2,iverb)

 1    read(iunit,*,err=1,end=999) ipor, dum1, dum2
      do 10 ip=1,ipor
 10      read(iunit,*,end=999) idum, x(ip)
      if(ipp.ne.0.and.ipor.ne.ipp) goto 1
      do 20 ip=1,ipor+1
 20      write(iunit2,*) (x(mod(ip-(j-1)*id-1+m*ipor,ipor)+1), j=m,1,-1)
      write(iunit2,'()')
      write(iunit2,'()')
      goto 1
 999  continue
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "-d# [-m# -p# -o outfile -l# -x# -c# -V# -h] file")
      call popt("d","delay")
      call popt("m","embedding dimension (2)")
      call popt("p","period of orbit (1)")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_delay")
      call pall()
      stop
      end


