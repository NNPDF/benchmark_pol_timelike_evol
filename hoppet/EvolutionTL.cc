/// An example in C++ similar to the one found in examples_f90
///
#include <hoppet_v1.h>
#include <apfel/apfelxx.h>
#include <iostream>
#include <cmath>
#include <cstdio>

#include <fstream>

// definition of the initial condition function
void  lha_unpolarized_dummy_pdf(const double & x, const double & Q, double * pdf)
{
  for (auto const& f : apfel::QCDEvToPhys(apfel::LHToyFFs(x, Q)))
    pdf[f.first + 6] = f.second;
}

//----------------------------------------------------------------------
int main()
{
  // Evolution parameters
  const double mc = 1.414213563;   
  const double mb = 4.5;
  const double mt = 175.0;
  const std::vector<double> Thresholds = {0, 0, 0, mc, mb, mt};

  const double Qmax    = 13000.0;
  const double Qmin    = 1.0;
  const int order      = -6; 
  const double ymax    = 16.5;
  const double dy      = 0.05;
  const double dlnlnQ  = dy/8.0;

  const double asQ   = 0.35;
  const double Q0    = sqrt(2.0);
  const double muR_Q = 1.0;
  const double Q     = 100;

  const bool exact_nfthreshold = true;
  const bool exact_splitting = false;

  const std::vector<int> nloopv{1, 1, 2, 2};
  const std::vector<bool> vfnsv{false, true, false, true};
  const std::vector<std::string> fnamev{
    "../results/hoppet-table16-part2-tl.csv",
    "../results/hoppet-table16-part3-tl.csv",
    "../results/hoppet-table17-part1-tl.csv",
    "../results/hoppet-table18-part1-tl.csv",
      };

  for (int i = 0; i < (int) nloopv.size(); i++)
    {
      vfnsv[i] ? hoppetSetPoleMassVFN(mc,mb,mt) : hoppetSetFFN(4);
      hoppetSetExactDGLAP(exact_nfthreshold,exact_splitting);
      hoppetStartExtended(ymax,dy,Qmin,Qmax,dlnlnQ,nloopv[i],order,factscheme_FragMSbar);
      hoppetEvolve(asQ, Q0, nloopv[i], muR_Q, lha_unpolarized_dummy_pdf, Q0);

      // output the results
      const std::vector<double> xvals{1e-2, 5e-2, 1e-1, 2e-1, 3e-1, 4e-1, 5e-1, 6e-1, 7e-1, 8e-1, 9e-1};
      double pdf[13];
      std::cout << std::scientific;
      std::cout.precision(5);
      std::cout << "\nAlphaQCD(Q) = " << hoppetAlphaS(Q) << std::endl;
      std::cout << "x\t\tu-ubar\t\t-d+dbar\t\t-L^-\t\t-2L^+\t\ts+sbar\t\tc+cbar\t\tb+bbar\t\tgluon" << std::endl;
      for (double x : xvals)
	{
	  hoppetEval(x, Q, pdf);
	  std::cout.precision(1);
	  std::cout << x;
	  std::cout.precision(4);
	  std::cout << "\t\t" << pdf[6+2] - pdf[6-2]
		    << "\t" << - pdf[6+1] + pdf[6-1]
		    << "\t" << - 2 * ( pdf[6-1] - pdf[6-2] )
		    << "\t" << - 2 * ( pdf[6-1] + pdf[6-2] )
		    << "\t" << pdf[6+3] + pdf[6-3]
		    << "\t" << pdf[6+4] + pdf[6-4]
		    << "\t" << pdf[6+5] + pdf[6-5]
		    << "\t" << pdf[6+0]
		    << std::endl;
	}
      std::cout << "\n";

      // Print to file
      std::ofstream fout(fnamev[i]);
      fout << ",u_v,d_v,L_m,L_p,s_p,c_p,b_p,g\n";
      for (int i = 0; i < (int) xvals.size(); i++)
	{
	  hoppetEval(xvals[i], Q, pdf);
	  fout << i
	       << "," << pdf[6+2] - pdf[6-2]
	       << "," << - pdf[6+1] + pdf[6-1]
	       << "," << - 2 * ( pdf[6-1] - pdf[6-2] )
	       << "," << - 2 * ( pdf[6-1] + pdf[6-2] )
	       << "," << pdf[6+3] + pdf[6-3]
	       << "," << pdf[6+4] + pdf[6-4]
	       << "," << pdf[6+5] + pdf[6-5]
	       << "," << pdf[6+0]
	       << std::endl;
	}
      fout.close();
    }
}
