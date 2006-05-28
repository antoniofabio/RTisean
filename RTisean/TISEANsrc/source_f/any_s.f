c extract numbers from strings
c Copyright (C) T. Schreiber (1998)

      function i_s(s,ierr)
      character*(*) s

      ierr=0
      read(s,'(i20)',err=777) i_s
      if(s.ne.'-'.and.s.ne.'+') return   ! reject a solitary - or +
 777  ierr=1
      end

      function f_s(s,ierr)
      character*(*) s

      ierr=0
      read(s,'(f20.0)',err=777) f_s
      if(s.ne.'-'.and.s.ne.'+') return   ! reject a solitary - or +
 777  ierr=1
      end
