#include <Rcpp.h>
using namespace Rcpp;

//' Assert a function argument is of correct type
//'
//' \itemize{
//'  \item \code{testarg_num()} asserts that the argument is a numeric value.
//'  Also rejects \code{NaNs} and \code{NAs} by default.
//'  \item \code{testarg_prop()} asserts that the argument value lies between 0
//'  and 1.
//'  \item \code{testarg_pos()} asserts that the argument is positive.
//'  \item \code{testarg_not_this()} asserts that the argument is different
//'  from one or several forbidden values.
//'  \item \code{testarg_length()} asserts that the argument has the correct
//'  length.
//' }
//'
//' @param arg value of the asserted argument.
//'
//' @author Theo Pannetier
//'
//' @name testargs

// [[Rcpp::export]]
void testarg_not_this(NumericVector vec, double forbidden)
{
  Rcpp::Function testarg_not_this("testarg_not_this");
  testarg_not_this(vec, _["forbidden"] = forbidden);
}

// [[Rcpp::export]]
void testarg_num(NumericVector vec)
{
  Rcpp::Function testarg_num("testarg_num");
  testarg_num(vec, _["allow_nan"] = false, _["allow_na"] = false);
}

// [[Rcpp::export]]
void testarg_pos(NumericVector vec) {
  Rcpp::Function testarg_pos("testarg_pos");
  testarg_pos(vec);
}

// [[Rcpp::export]]
void testarg_length_num(NumericVector vec, int correct_length) {
    Rcpp::Function testarg_length("testarg_length");
    testarg_length(vec, _["correct_length"] = correct_length);
}

// [[Rcpp::export]]
void testarg_length_char(CharacterVector vec, int correct_length) {
  Rcpp::Function testarg_length("testarg_length");
  testarg_length(vec, _["correct_length"] = correct_length);
}
