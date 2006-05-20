c logging routines

	subroutine logs(str)
	character*(*) str	
	open(33,file='log.$$$',access='APPEND',status='UNKNOWN')
	write(33,*) str
	close(33)
	end

	subroutine logi(i)
	integer i
	open(33,file='log.$$$',access='APPEND',status='UNKNOWN')
	
	write(33,*) i
	close(33)
	end

	subroutine logr(r)
	real r
	open(33,file='log.$$$',access='APPEND',status='UNKNOWN')
	
	write(33,*) r
	close(33)
	end

	subroutine logd(d)
	double precision d
	open(33,file='log.$$$',access='APPEND',status='UNKNOWN')
	
	write(33,*) d
	close(33)
	end	
