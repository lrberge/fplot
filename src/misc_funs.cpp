#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector cpp_unclassFactor(NumericVector x){
    // x: a sorted integer vector

    int N = x.size();

    IntegerVector res(N);
    int k=1;
    res[0] = 1;

    for(int i=1 ; i<N ; i++){
        if(x(i-1)!=x(i)) k++;
        res[i] = k;
    }

    return(res);
}

// [[Rcpp::export]]
IntegerVector cpp_unik(NumericVector x_sorted, int k_max){
    // x_sorted: a sorted numeric vector

    int n = x_sorted.size();

    IntegerVector res(k_max);
    int k = 1;
    res[0] = x_sorted[0];

    for(int i=1 ; i<n ; i++){
        if(x_sorted(i - 1) != x_sorted(i)){
            res[k] = x_sorted(i);
            k++;
            if(k == k_max) break;
        }
    }

    return(res);
}






























