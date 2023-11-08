//
// APFEL++ 2017
//
// Author: Valerio Bertone: valerio.bertone@cern.ch
//

#include <apfel/apfelxx.h>

int main()
{
  apfel::Banner();

  // Initial scale
  const double mu0 = sqrt(2);

  // Resummation scale
  const double xi = 1;
  //const double xi = sqrt(2);
  //const double xi = sqrt(0.5);

  // Vectors of masses and thresholds
  const std::vector<double> Thresholds = {0, 0, 0, sqrt(2), 4.5, 175};
  //const std::vector<double> Thresholds = {0, 0, 0, 0}; // Nf = 4

  // Perturbative order
  const int pto = 0;
  //const int pto = 1;
  //const int pto = 2;

  // x-space grid
  const apfel::Grid g{{apfel::SubGrid{200, 1e-7, 3}, apfel::SubGrid{100, 1e-1, 3}, apfel::SubGrid{100, 6e-1, 3}, apfel::SubGrid{80, 8.5e-1, 5}}};

  // Tabulate PDFs
  apfel::AlphaQCD a{0.35, sqrt(2), Thresholds, pto};
  const apfel::TabulateObject<double> Alphas{a, 100, 0.9, 1001, 3};
  const apfel::TabulateObject<apfel::Set<apfel::Distribution>> TabulatedPDFs{*BuildDglap(InitializeDglapObjectsQCDpol(g, Thresholds), apfel::LHToyPDFsPol, mu0, pto, [&] (double const& mu) -> double{ return Alphas.Evaluate(mu); }, xi), 50, 1, 1000, 3};

  // Final scale
  const double mu = 100;

  // Print results
  const std::vector<double> xlha = {1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 3e-1, 5e-1, 7e-1, 9e-1};
  std::cout << std::scientific;
  std::cout << "\nAlphaQCD(Q) = " << Alphas.Evaluate(mu)   << std::endl;
  std::cout << "x\t\tu-ubar\t\t-d+dbar\t\t-L^-\t\t-2L^+\t\ts+sbar\t\tc+cbar\t\tb+bbar\t\tgluon" << std::endl;
  for (auto const& x : xlha)
    {
      const std::map<int, double> DistMap = apfel::QCDEvToPhys(TabulatedPDFs.EvaluateMapxQ(x,mu));
      std::cout.precision(1);
      std::cout << x;
      std::cout.precision(4);
      std::cout << "\t\t" << DistMap.at(2) - DistMap.at(-2)
                << "\t" << - DistMap.at(1) + DistMap.at(-1)
		<< "\t" << - 2 * ( DistMap.at(-1) - DistMap.at(-2) )
                << "\t" << - 2 * ( DistMap.at(-1) + DistMap.at(-2) )
		<< "\t" << DistMap.at(3) + DistMap.at(-3)
                << "\t" << DistMap.at(4) + DistMap.at(-4)
		<< "\t" << DistMap.at(5) + DistMap.at(-5)
                << "\t" << DistMap.at(0)
                << std::endl;
    }

  return 0;
}
