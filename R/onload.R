



.onLoad = function(libname, pkgname){
    
  # To circumvent a peculiar behavior from pkgdown
  fix_pkgwdown_path()
  
  if(is_r_check()){
		data.table::setDTthreads(1)
	}

  invisible()
}

