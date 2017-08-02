#include<Rcpp.h>
#include<random>
#include<cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector EPSPBinomial(List theta, int N, int n) {
    NumericVector result(n);

    double p = theta["p"], mu = theta["mu"], sigma = sqrt(theta["sigma"]);

    std::default_random_engine generator;
    std::bernoulli_distribution bernoulli(p);
    std::lognormal_distribution<double> lognormal(mu, sigma);

    for (int i = 0; i < n; i++) {
        double z = 0;
        for (int j = 0; j < N; j++) {
            if (bernoulli(generator)) z += lognormal(generator);
        }
        result[i] = z;
    }
    return result;
}
