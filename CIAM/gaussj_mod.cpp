#include "Definitions.h"
#include <cmath>
//#include "MatrixC.h"
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std; // enables elimination of std::


//void gaussj(Matrix& a,int n,Matrix& b,int m)
void gaussj(double** a,int n,double** b,int m)
//double **a,**b;
//int n,m;
{
	//int *indxc,*indxr,*ipiv;
	int i,icol,irow,j,k,l,ll;
	vector<int> indxc(n), indxr(n), ipiv(n);
	double big,dum,pivinv;
	//void nrerror(),free_ivector();

	//indxc=ivector(1,n);
	//indxr=ivector(1,n);
	//ipiv=ivector(1,n);
	for (j=0;j<n;j++) ipiv[j]=0;
	for (i=0;i<n;i++) {
		big=0.0;
		for (j=0;j<n;j++)
			if (ipiv[j] != 1)
				for (k=0;k<n;k++) {
					if (ipiv[k] == 0) {
						if (fabs(a[j][k]) >= big) {
							big=fabs(a[j][k]);
							irow=j;
							icol=k;
						}
					} else if (ipiv[k] > 1) cerr<<"GAUSSJ: Singular Matrix-1\n";
				}
		++(ipiv[icol]);
		if (irow != icol) {
			for (l=0;l<n;l++) {
				swap(a[irow][l],a[icol][l]);
			}
			for (l=0;l<m;l++){
				swap(b[irow][l],b[icol][l]);
			}
		}
		indxr[i]=irow;
		indxc[i]=icol;
		if (a[icol][icol] == 0.0) cerr<<"GAUSSJ: Singular Matrix-2\n";
		pivinv=1.0/a[icol][icol];
		a[icol][icol]=1.0;
		for (l=0;l<n;l++) a[icol][l] *= pivinv;
		for (l=0;l<m;l++) b[icol][l] *= pivinv;
		for (ll=0;ll<n;ll++)
			if (ll != icol) {
				dum=a[ll][icol];
				a[ll][icol]=0.0;
				for (l=0;l<n;l++) a[ll][l] -= a[icol][l]*dum;
				for (l=0;l<m;l++) b[ll][l] -= b[icol][l]*dum;
			}
	}
	for (l=(n-1);l>=0;l--) {
		if (indxr[l] != indxc[l])
			for (k=0;k<n;k++)
				swap(a[k][indxr[l]],a[k][indxc[l]]);
	}
	//free_ivector(ipiv,1,n);
	//free_ivector(indxr,1,n);
	//free_ivector(indxc,1,n);
}