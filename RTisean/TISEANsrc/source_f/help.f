c Utilities for usage message
c Copyright (C) T. Schreiber (1998)

      subroutine whatido(text,iverb)
      character*72 progname
      character*(*) text

      call getarg(0,progname)
      call argdel(0)
      iverb=igetv(iverb)
      if(iv_io(iverb).eq.1) then
         write(istderr(),'()')  
         write(istderr(),'(a)') 
     .      "TISEAN 2.1 (C) R. Hegger, H. Kantz, T. Schreiber"
         write(istderr(),'()')  
         write(istderr(),'(a,a,a)') 
     .      progname(1:index(progname," ")-1), ": ", text
      endif
      if(lopt("h",1).eq.1) call usage()
      end

      subroutine whatineed(text)
      character*72 progname
      character*(*) text

      call getarg(0,progname)
      write(istderr(),'()') 
      write(istderr(),'(a,a,x,a)') 
     .   "Usage: ", progname(1:index(progname," ")-1),  text
      end

      subroutine popt(c,text)
      character*(*) c,text

      write(istderr(),'(5h    -,a,x,1h<,a,1h>)') c, text
      end

      subroutine ptext(text)
      character*(*) text

      write(istderr(),'(3x,a)') text
      end

      subroutine pout(text)
      character*(*) text

      write(istderr(),'(8h    -o <,a,a,1h>)') 
     .   "output file name, just -o means ", text
      end

      subroutine pall()

      call popt("V","verbosity level (0 = only fatal errors)")
      call popt("h","show this message")
      write(istderr(),'()')
      end

