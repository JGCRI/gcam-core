#ifndef UBLAS_HELPERS_HH_
#define UBLAS_HELPERS_HH_

#include <iostream>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>

#define UBLAS boost::numeric::ublas

template <class FTYPE>
std::ostream & operator<<(std::ostream &ostrm, const UBLAS::vector<FTYPE> &v) {
  ostrm << "(";
  for(size_t i=0; i<v.size(); ++i) {
    if(i>0) ostrm << ", ";
    ostrm << v[i];
  }
  ostrm << ")";
  return ostrm;
}

template <class FTYPE>
std::ostream & operator<<(std::ostream &ostrm, const UBLAS::matrix<FTYPE> &M) {
  int m = M.size1();
  int n = M.size2();
  
  for(int i=0;i<m;++i) {
    for(int j=0;j<n;++j)
      ostrm << M(i,j) << "  ";
    ostrm << "\n";
  }
  
  return ostrm;
}


#undef UBLAS

#endif
