#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void testarg_not_this(NumericVector vec, double not_this)
{
  Rcpp::Function testarg_not_this("testarg_not_this");
  testarg_not_this(vec, _["forbidden"] = not_this);
}

// [[Rcpp::export]]
void testarg_num(NumericVector vec)
{
  Rcpp::Function testarg_num("testarg_not_this");
  testarg_num(vec);
}

// [[Rcpp::export]]
void testarg_pos(NumericVector vec) {
  Rcpp::Function testarg_num("testarg_pos");
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
