c Choose columns and sub-sequences from a file
c Copyright (C) T. Schreiber (1999)

      parameter(nx=1000000,mx=5)
      dimension x(nx,mx), icol(mx)
      character*72 file, fout
      data iverb/15/

      call whatido("Choose columns and sub-sequences from a file",iverb)
      nmax=ican("l",nx)
      nexcl=ican("x",0)
      mcmax=ican("m",0)
      call columns(mc,mx,icol)
      if(mcmax.eq.0) mcmax=max(1,mc)
      isout=igetout(fout,iverb)

      call nthstring(1,file)
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_select")
      call outfile(fout,iunit,iverb)
      call xwritefile(nmax,mcmax,nx,x,fout,iverb)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-o outfile -l# -x# -m# -c#[,#] -V# -h] file")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("m","number of columns to be read (1)")
      call popt("c","columns to be read (1)")
      call pout("file_select")
      call pall()
      stop
      end
