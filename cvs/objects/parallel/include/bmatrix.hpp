#ifndef BMATRIX_HH_
#define BMATRIX_HH_


#include <iostream>
#include <assert.h>

/* boolean arithmetic matrix class.  This is kind of a naive
   implementation, but it should get the job done for now. */
class bmatrix {
  unsigned m,n;                      // size
  unsigned *data;
public:
  bmatrix(unsigned nr=0, unsigned nc=0) : m(nr), n(nc) {
    unsigned ntot = nr*nc;
    if(ntot > 0)
      data = new unsigned[nr*nc];
    else
      data = 0;
  }
  bmatrix(const bmatrix &B) {
      m = B.m;
      n = B.n;
      unsigned ntot = m*n;
      data = new unsigned[ntot];
      for(unsigned i=0; i<ntot; ++i)
        data[i] = B.data[i]; 
  }
  bmatrix &operator=(const bmatrix &B) {
    if(this != &B) {
      delete [] data;
      m = B.m;
      n = B.n; 
      unsigned ntot = m*n;
      data = new unsigned[ntot];
      for(unsigned i=0; i<ntot; ++i)
        data[i] = B.data[i]; 
    }
    return *this;
  } 
  ~bmatrix(void) {delete [] data;}
  void resize(unsigned nr, unsigned nc) {
    delete [] data;
    m = nr;
    n = nc;
    data = new unsigned[m*n];
  }

  // primary accessors
  int nrow(void) const {return m;}
  int ncol(void) const {return n;}
  unsigned *operator[](unsigned i) {return data+i*n;}
  const unsigned *operator[](unsigned i) const {return data+i*n;}

  // arithmetic operators:
  // a+b  <--> a | b
  // a-b  <--> a | ~b
  // a*b  <--> a & b
  // Note we don't check for any dimensional compatibility in production code.
  const bmatrix &operator+=(const bmatrix &B) {
    assert(m == B.m && n == B.n);
    unsigned ntot = m*n;
    for(unsigned i=0;i<ntot;++i)
      data[i] |= B.data[i];
    return *this;
  }
  bmatrix operator+(const bmatrix &B) const { // note this returns a copy
    bmatrix C(*this);
    return C += B;
  }

  bmatrix &operator-=(const bmatrix &B) {
    assert(m == B.m && n == B.n);
    unsigned ntot = m*n;
    for(unsigned i=0; i<ntot; ++i)
      data[i] = (data[i] && !B.data[i]) ? 1 : 0;
    //      data[i] |= ~B.data[i];
    return *this;
  }
  bmatrix operator-(const bmatrix &B) const {
    bmatrix C(*this);
    return C -= B;
  }

  // infix operator is really not the best way to do this, but we'll live with it for now
  bmatrix operator*(const bmatrix &B) const {
    assert(n == B.m);
    unsigned cm = m, cn = B.n, kmx = n;

    bmatrix C(cm,cn);

    // naive matrix multiply -- terrible cache performance.  Fix if
    // this subroutine ever an important contributor to run time
    for(unsigned i=0; i<cm; ++i) {
      const unsigned *ai = this->operator[](i);
      unsigned *ci = C[i];
      for(unsigned j=0; j<cn; ++j) {
        ci[j] = 0;
        for(unsigned k=0; k<kmx; ++k)
          ci[j] |= ai[k] & B[k][j];
      }
    }
    return C;
  }
};

inline std::ostream &operator<<(std::ostream &out, const bmatrix &B)
{
  for(int i=0;i<B.nrow();++i) {
    for(int j=0; j<B.ncol(); ++j)
      out << B[i][j] << "  ";
    out << "\n";
  }
  return out;
}



#endif
