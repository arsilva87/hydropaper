#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector inout(IntegerMatrix z, NumericMatrix ab) {
   int n = z.nrow();
   LogicalVector out(n);
   for(int i = 0; i < n; i++) {
     out(i) = z(i,0) >= ((z(i,1) - ab(0,2))/ab(1,2)) &&
		  z(i,0) <= ((z(i,1) - ab(0,0))/ab(1,0)) &&
		  z(i,1) >= (ab(0,1) + ab(1,1)*z(i,0)) &&
		  z(i,1) <= (ab(0,3) + ab(1,3)*z(i,0));
   }
   return out;
}
