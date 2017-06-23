/*
 * Testing the speed of the lognormal random number generator in cpp.
 */
#include <iostream>
#include <random>

int main() {
  const int nrolls = 1000000;
  double results[nrolls];

  std::default_random_engine generator;
  std::lognormal_distribution<double> distribution(0.0, 1.0);

  for (int i = 0; i < nrolls; i++) {
    double number = distribution(generator);
    results[i] = number;
  }

  std::cout << "done\n";
}
