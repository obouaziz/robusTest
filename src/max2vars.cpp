#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cmath>

//' @useDynLib robusTest, .registration=TRUE
//' @importFrom Rcpp sourceCpp

using namespace Rcpp;
using namespace std;

class ArgComparator {
private:
  const NumericVector& _v;
public:
  ArgComparator(const NumericVector& v) : _v(v) {}
  bool operator()(size_t i1, size_t i2) {
    return _v[i1] < _v[i2];
  }
};

vector<size_t> argsort(const NumericVector& v) {
  vector<size_t> idx(v.size());
  ArgComparator cmp(v);
  for(size_t i = 0; i < v.size(); i++) {
    idx[i] = i;
  }
  stable_sort(idx.begin(), idx.end(), cmp);
  return idx;
}

//' Compute the test statistic for the robust independence test of Kolmogorov-Smirnov's type
//'
//' Compute the maximum distance between the joint empirical cumulative
//' distribution function (e.c.d.f.) and the product of the marginals e.c.d.f.
//' multiplied by square root of n
//' @param x,y the two sequences of vector that are applied to the function.
//' @export
// [[Rcpp::export]]
double max2D_cpp(NumericVector const& x, NumericVector const& y) {
  size_t n = x.size();
  vector<size_t> tri_x = argsort(x);
  vector<size_t> tri_y = argsort(y);
  double max = 0;
  for (size_t i = 0; i < n-1; i++) {
    int cpt = 0;
    for (size_t j = 0; j < n-1; j++) {
      if (y[tri_x[j]] <= y[tri_y[i]]) cpt += 1;
      double v = abs(cpt/double(n) - ((i+1)/double(n))*((j+1)/double(n)));
      if (v > max) max = v;
    }
  }
  return sqrt(n)*max;
}
