#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int test_int(){
	int i=0;
	return(i);
}

// [[Rcpp::export]]
int test_double(){
	double i=0;
	return(i);
}

// [[Rcpp::export]]
int test_char(){
	char i='0';
	return(i);
}

// [[Rcpp::export]]
int test_string(){
	char i[]="zero";
	return(i);
}


