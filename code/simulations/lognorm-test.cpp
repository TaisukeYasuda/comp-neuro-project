/*
 * Testing the speed of the lognormal random number generator in cpp.
 */
#include <iostream>
#include <random>

int main() {
  const int nrolls = 1e6;
  double results[nrolls];

  std::default_random_engine generator;
  std::lognormal_distribution<double> distribution(0.0, 1.0);

  for (int i = 0; i < 100; i++) {
      for (int j = 0; j < nrolls; j++) {
        double number = distribution(generator);
        results[j] = number;
      }
  }

  std::cout << "done\n";
}
