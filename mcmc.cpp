#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double logAPrioriNorm(double x, double mean , double sd) {
  return R::dnorm(x, mean, sd, 1);
}
// [[Rcpp::export]]
double logAPrioriGamma(double x, double shape, double scale) {
  return R::dgamma(x, shape, scale, 1);
}
// [[Rcpp::export]]
double logAPriori(NumericVector theta, 
                   double alphaMean, 
                   double alphaSd, 
                   double betaMean, 
                   double betaSd,
                   double shape,
                   double scale) {
  double alpha = theta[0];
  double beta = theta[1];
  double sigma = theta[2];
  return logAPrioriNorm(alpha, alphaMean, alphaSd) + 
         logAPrioriNorm(beta, betaMean, betaSd) + 
         logAPrioriGamma(sigma, shape, scale);
}
// [[Rcpp::export]]
double logLikelihood(NumericVector theta,
                  NumericVector x,
                  NumericVector y) {
  double alpha = theta[0];
  double beta = theta[1];
  double sigma = theta[2];
  int n = x.size();
  NumericVector error(n);
  for (int i=0; i<n; i++) {
    error[i] = y[i] - (alpha + x[i]*beta);
  }
  
  double loglikelihood = 0;
  for (int i=0; i<n; i++) {
    loglikelihood += R::dnorm(error[i], 0, sqrt(sigma), 1);
  }
  return loglikelihood;
}
// [[Rcpp::export]]
double logPosterior(NumericVector theta,
                    NumericVector x,
                    NumericVector y) {
    return logLikelihood(theta, x, y) + logAPriori(theta, 0, 3, 0, 3, 2, 2);

}
// [[Rcpp::export]]
NumericVector proposal(NumericVector theta, NumericVector sigma) {
	int n = theta.size();
	NumericVector proposeTheta(n);
	for (int i = 0; i < n; i++) {
		proposeTheta[i] = R::rnorm(theta[i], sigma[i]);
	}
	return proposeTheta;
}


// [[Rcpp::export]]
NumericVector test(int n) {
	NumericMatrix myMatrix(Dimension(n, 3));

	for (int i = 0; i < n; i++) {
		myMatrix(i,0) = R::rnorm(0, 1);
		myMatrix(i,1) = R::rnorm(0, 2);
		myMatrix(i,2) = R::rnorm(0, 3);
	}
	return myMatrix;
}

// [[Rcpp::export]]
List mcmc(int n_sim,
              NumericVector theta,
              NumericVector x,
              NumericVector y) {
	NumericMatrix chain(Dimension(n_sim, theta.size()));
	chain(0,_) = theta;
	NumericVector sigma(3);
	NumericVector eta;
	sigma[0] = 3;
	sigma[1] = 3;
	sigma[2] = 3;
    double U;
    bool accepted;
    NumericVector acceptance_rate(n_sim);
    acceptance_rate[0] = 1;
    int attempts = 0;
    for (int i=0; i < n_sim - 1; i++) {
      do {
        eta = proposal(chain(i,_), sigma); // genera el candidato
        U = (runif(1))[0];
        accepted = (log(U) <= logPosterior(eta, x, y) - logPosterior(chain(i,_), x, y));
        attempts++;
      } while (!accepted);
      acceptance_rate[i + 1] = (double) i / attempts;
      chain(i + 1,_) = eta;
    }
    return List::create(Named("chain") = chain, Named("acceptance_rate") = acceptance_rate);
}
