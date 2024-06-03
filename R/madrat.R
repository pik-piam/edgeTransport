.onAttach <- function(libname, pkgname) {
  if ('try-error' != class(try(find.package('madrat'), TRUE)))
    madrat::madratAttach(pkgname)
}

.onDetach <- function(libpath) {
  if ('try-error' != class(try(find.package('madrat'), TRUE)))
    madrat::madratDetach(libpath)
}
