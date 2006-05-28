      function igetv(idef)
c get verbosity level

      igetv=ican("V",-1)
      if(igetv.eq.-1.and.lopt("V",1).eq.1) igetv=2**15-1
      if(igetv.eq.-1) igetv=idef
      end

      function iexv(iverb,item)
c 1 if verbosity level includes item

      iexv=iand(iverb,item)/item
      end

c the following functions test for specific numerical verbosity values

      function iv_io(iverb)           ! report i/o activity
      iv_io=iexv(iverb,1)
      end

      function iv_echo(iverb)         ! echo first line of data read
      iv_echo=iexv(iverb,128)
      end

      function iv_cost(iverb)         ! current value of cost function 
      iv_cost=iexv(iverb,2)
      end

      function iv_match(iverb)        ! cost mismatch 
      iv_match=iexv(iverb,4)
      end

      function iv_cool(iverb)         ! temperature etc. at cooling 
      iv_cool=iexv(iverb,8)
      end

      function iv_vcost(iverb)        ! verbose cost if improved 
      iv_vcost=iexv(iverb,16)
      end

      function iv_vmatch(iverb)       ! verbose cost mismatch
      iv_vmatch=iexv(iverb,32)
      end

      function iv_10(iverb)           ! upo status after 10 points
      iv_10=iexv(iverb,16)
      end

      function iv_100(iverb)          ! upo status after 100 points
      iv_100=iexv(iverb,8)
      end

      function iv_1000(iverb)         ! upo status after 1000 points
      iv_1000=iexv(iverb,4)
      end

      function iv_upo(iverb)          ! print orbits found
      iv_upo=iexv(iverb,2)
      end

      function iv_surr(iverb)         ! print iterations / discrepancy
      iv_surr=iexv(iverb,2)           
      end

      function iv_uncorr(iverb)       ! neighbour search status
      iv_uncorr=iexv(iverb,2)           
      end

      function iv_clust(iverb)        ! clustering status
      iv_clust=iexv(iverb,2)           
      end




