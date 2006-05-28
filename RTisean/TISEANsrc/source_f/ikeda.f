c iterate Ikeda map
c Copyright (C) Thomas Schreiber (1998)

      double precision xo, yo, xn, yn, a, b, c, s, cs, ss
      character*72 fout
      data a/0.4/,b/6.0/,c/0.9/,
     .   ntrans/10000/,xo/.68587/,yo/.65876/
      data iverb/1/

      call whatido("Ikeda map",iverb)
      nmax=imust('l')
      ntrans=ican('x',ntrans)
      a=fcan('A',real(a))
      b=fcan('B',real(b))
      c=fcan('C',real(c))
      xo=fcan('X',real(xo))
      yo=fcan('Y',real(yo))
      isout=igetout(fout,iverb)

      if(isout.eq.1) fout="ikeda.dat"
      call outfile(fout,iunit,iverb)
      n=-ntrans
 1    n=n+1
      s=a-b/(1.+xo**2+yo**2)
      cs=cos(s)
      ss=sin(s)
      xn=1.+c*(xo*cs-yo*ss)
      yn=c*(xo*ss+yo*cs)
      xo=xn
      yo=yn
      if(n.lt.1) goto 1
      write(iunit,*) real(xn), real(yn)
      if(nmax.eq.0.or.n.lt.nmax) goto 1
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "-l# [-A# -B# -C# -R# -I# -o outfile -x# -V# -h]")
      call popt("l","number of points x,y (l=0: infinite)")
      call popt("A","parameter a (0.4)")
      call popt("B","parameter b (6.0)")
      call popt("C","parameter c (0.9)")
      call popt("R","initial Re(z)")
      call popt("I","initial Im(z)")
      call popt("x","number of transients discarded (10000)")
      call pout("ikeda.dat")
      call pall()
      stop
      end


