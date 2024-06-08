//
// APFEL++ 2017
//
// Author: Valerio Bertone: valerio.bertone@cern.ch
//

#include <apfel/apfelxx.h>

#include <fstream>

int main()
{
  apfel::Banner();

  // Initial scale
  const double mu0 = sqrt(2);

  // Final scale
  const double mu = 100;

  // Reference for alpha_s
  const double a0  = 0.35;

  // x-space grid
  const apfel::Grid g{{apfel::SubGrid{200, 1e-2, 3}, apfel::SubGrid{100, 1e-1, 3}, apfel::SubGrid{100, 6e-1, 3}, apfel::SubGrid{80, 8.5e-1, 5}}};

  // Vectors of thresholds for the VFNS and the NF = 4 scheme
  const std::vector<double> MassesVFN = {0, 0, 0, sqrt(2), 4.5, 175};
  const std::vector<double> MassesNF4 = {0, 0, 0, 0};
  const std::vector<double> ThsVFN  = {0, 0, 0, sqrt(2), 4.5, 175};
  const std::vector<double> ThsVFNt = {0, 0, 0, sqrt(2) * sqrt(2), sqrt(2) * 4.5, sqrt(2) * 175};
  const std::vector<double> ThsVFNh = {0, 0, 0, sqrt(0.5) * sqrt(2), sqrt(0.5) * 4.5, sqrt(0.5) * 175};
  const std::vector<double> ThsNF4  = {0, 0, 0, 0};
  const std::vector<double> ThsNF3  = {0, 0, 0};

  // Tables 16-17-18 of https://arxiv.org/pdf/hep-ph/0511119.pdf
  const std::vector<int> ptov{0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2};
  const std::vector<std::vector<double>> Massesv{MassesNF4, MassesVFN, MassesNF4, MassesNF4, MassesNF4, MassesVFN, MassesVFN, MassesVFN, MassesNF4, MassesNF4, MassesNF4, MassesVFN, MassesVFN, MassesVFN};
  const std::vector<std::vector<double>> Thresholdsv{ThsNF4, ThsVFN, ThsNF4, ThsNF4, ThsNF4, ThsVFN, ThsVFNt, ThsVFNh, ThsNF4, ThsNF4, ThsNF4, ThsVFN, ThsVFNt, ThsVFNh};
  const std::vector<std::vector<double>> ThresholdsRefv{ThsNF4, ThsNF3, ThsNF4, ThsNF4, ThsNF4, ThsNF3, ThsNF3, ThsNF3, ThsNF4, ThsNF4, ThsNF4, ThsNF3, ThsNF3, ThsNF3};
  const std::vector<double> xiv{1, 1, 1, sqrt(2), sqrt(0.5), 1, sqrt(2), sqrt(0.5), 1, sqrt(2), sqrt(0.5), 1, sqrt(2), sqrt(0.5)};
  const std::vector<std::string> fnamev{
    "../results/apfelxx-table16-part2-tl.csv",
    "../results/apfelxx-table16-part3-tl.csv",
    "../results/apfelxx-table17-part1-tl.csv",
    "../results/apfelxx-table17-part2-tl.csv",
    "../results/apfelxx-table17-part3-tl.csv",
    "../results/apfelxx-table18-part1-tl.csv",
    "../results/apfelxx-table18-part2-tl.csv",
    "../results/apfelxx-table18-part3-tl.csv",
    "../results/apfelxx-table19-part1-tl.csv",
    "../results/apfelxx-table19-part2-tl.csv",
    "../results/apfelxx-table19-part3-tl.csv",
    "../results/apfelxx-table20-part1-tl.csv",
    "../results/apfelxx-table20-part2-tl.csv",
    "../results/apfelxx-table20-part3-tl.csv"
      };

  for (int i = 0; i < (int) ptov.size(); i++)
    {
      // Alpha_s nf = 3 to compute the value of alpha_s below the
      // charm threshold.
      apfel::AlphaQCD aRef{a0, mu0, ThresholdsRefv[i], ptov[i]};

      // Tabulate PDFs
      apfel::AlphaQCD a{aRef.Evaluate(mu0 * xiv[i]), mu0 * xiv[i], Massesv[i], Thresholdsv[i], ptov[i]};
      const apfel::TabulateObject<double> Alphas{a, 200, 0.5, 1001, 3};
      const apfel::TabulateObject<apfel::Set<apfel::Distribution>> TabulatedPDFs{*BuildDglap(InitializeDglapObjectsQCDT(g, Massesv[i]), apfel::LHToyFFs, mu0, ptov[i], [&] (double const& mu) -> double{ return Alphas.Evaluate(mu); }, xiv[i]), 50, 1, 1000, 3};

      // Print results
      const std::vector<double> xlha = {1e-2, 5e-2, 1e-1, 2e-1, 3e-1, 4e-1, 5e-1, 6e-1, 7e-1, 8e-1, 9e-1};
      std::cout << std::scientific;
      std::cout.precision(5);
      std::cout << "\nAlphaQCD(Q) = " << Alphas.Evaluate(mu) << std::endl;
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
      std::cout << "\n";

      // Print to file
      std::ofstream fout(fnamev[i]);
      fout << ",u_v,d_v,L_m,L_p,s_p,c_p,b_p,g\n";
      for (int i = 0; i < (int) xlha.size(); i++)
	{
	  const std::map<int, double> DistMap = apfel::QCDEvToPhys(TabulatedPDFs.EvaluateMapxQ(xlha[i], mu));
	  fout << i
	       << "," << DistMap.at(2) - DistMap.at(-2)
	       << "," << - DistMap.at(1) + DistMap.at(-1)
	       << "," << - 2 * ( DistMap.at(-1) - DistMap.at(-2) )
	       << "," << - 2 * ( DistMap.at(-1) + DistMap.at(-2) )
	       << "," << DistMap.at(3) + DistMap.at(-3)
	       << "," << DistMap.at(4) + DistMap.at(-4)
	       << "," << DistMap.at(5) + DistMap.at(-5)
	       << "," << DistMap.at(0)
	       << std::endl;
	}
      fout.close();
    }

  return 0;
}
