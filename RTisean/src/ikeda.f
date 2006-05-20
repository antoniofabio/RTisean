c iterate Ikeda map
c Copyright (C) Thomas Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

      subroutine ikeda(options_filename)
      character*30 options_filename
      double precision xo, yo, xn, yn, a, b, c, s, cs, ss
      character*72 fout
c      data a/0.4/,b/6.0/,c/0.9/,
c     .   ntrans/10000/,xo/.68587/,yo/.65876/
      data iverb/1/
      character*255 str
      
      a=0.4
      b=6.0
      c=0.9
      ntrans=10000
      x0=0.68587
      y0=0.65876

      call getparam(str,options_filename)
      call whatido("Ikeda map",iverb,str)
      nmax=imust('l',str)
      ntrans=ican('x',ntrans,str)
      a=fcan('A',real(a),str)
      b=fcan('B',real(b),str)
      c=fcan('C',real(c),str)
      xo=fcan('X',real(xo),str)
      yo=fcan('Y',real(yo),str)
      isout=igetout(fout,iverb,str)

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




