// APFEL++ libs
#include "apfel/apfelxx.h"

// QCDNUM libs
#include "QCDNUM/QCDNUM.h"

#include <fstream>

// Function used by QCDNUM as an input
double func(int* ipdf, double* x) {
  int i = *ipdf;
  double xb = *x;

  const std::map<int, double> xpdf = apfel::QCDEvToPhys(apfel::LHToyPDFsPol(xb, 1));
  double f = 0;
  if(i ==  0) f = xpdf.at(0);
  if(i ==  1) f = xpdf.at(1) - xpdf.at(-1);
  if(i ==  2) f = xpdf.at(2) - xpdf.at(-2);
  if(i ==  3) f = xpdf.at(3) - xpdf.at(-3);
  if(i ==  4) f = xpdf.at(-1);
  if(i ==  5) f = xpdf.at(-2);
  if(i ==  6) f = xpdf.at(-3);
  if(i ==  7) f = xpdf.at(4) - xpdf.at(-4);
  if(i ==  8) f = xpdf.at(-4);
  if(i ==  9) f = 0;
  if(i == 10) f = 0;
  if(i == 11) f = 0;
  if(i == 12) f = 0;
  return f;
}

int main()
{
  // Initial scale
  const double Qin = sqrt(2) - 0.001;

  // Final scale
  double mu = 100;

  // Retrieve evolution parameters
  const int    pto   = 2;
  const double Qref  = sqrt(2);
  const double asref = 0.35;
  const double mc    = sqrt(2);
  const double mb    = 4.5;
  const double mt    = 175;

  const std::vector<int> ptov{1, 1, 2, 2};
  const std::vector<int> nfinv{4, 0, 4, 0};
  const std::vector<std::string> fnamev{
    "../results/qcdnum-table16-part2.csv",
    "../results/qcdnum-table16-part3.csv",
    "../results/qcdnum-table17-part1.csv",
    "../results/qcdnum-table18-part1.csv",
      };

  for (int i = 0; i < (int) ptov.size(); i++)
    {
      int    ityp   = 2, iord = ptov[i], nfin = nfinv[i];                //pol, order, VFNS
      double as0    = asref, r20 = pow(Qref, 2);              //input alphas
      double xmin[] = {1.e-7, 1e-1, 0.8};                                      //x-grid
      int    iwt[]  = {1, 3, 9}, ng = 3, nxin = 300, iosp = 3;             //x-grid
      int    nqin   = 240;                                           //mu2-grid
      double qq[]   = {pow(Qin, 2), pow(mc, 2), pow(mb, 2), 1e4}, wt[] = { 1e0, 1e0, 1e0, 1e0};              //mu2-grid
      double q2c = pow(mc, 2), q2b = pow(mb, 2), q0 = pow(Qin, 2);  //thresholds, mu20
    
      double def[] =                             //input flavour composition
      // tb  bb  cb  sb  ub  db   g   d   u   s   c   b   t
	{ 0., 0., 0., 0., 0.,-1., 0., 1., 0., 0., 0., 0., 0.,      // 1=dval
	  0., 0., 0., 0.,-1., 0., 0., 0., 1., 0., 0., 0., 0.,      // 2=uval
	  0., 0., 0.,-1., 0., 0., 0., 0., 0., 1., 0., 0., 0.,      // 3=sval
	  0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0.,      // 4=dbar
	  0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0.,      // 5=ubar
	  0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0.,      // 6=sbar
	  0., 0.,-1., 0., 0., 0., 0., 0., 0., 0., 1., 0., 0.,      // 7=cval
	  0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,      // 8=cbar
	  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,      // 9=zero
	  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,      //10=zero
	  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,      //11=zero
	  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.};     //12=zero
    
      int nx, nq, id1, id2, nw, nfout, ierr ; double eps;
      int lun = 6 ; string outfile = " ";
    
      QCDNUM::qcinit(lun,outfile);                              //initialize
      QCDNUM::gxmake(xmin,iwt,ng,nxin,nx,iosp);                     //x-grid
      QCDNUM::gqmake(qq,wt,4,nqin,nq);                            //mu2-grid
      QCDNUM::fillwt(ityp,id1,id2,nw);                      //calculate weights
      QCDNUM::setord(iord);                                  //LO, NLO, NNLO
      QCDNUM::setalf(as0,r20);                                //input alphas
      int iqc = QCDNUM::iqfrmq(q2c);                      //charm threshold
      int iqb = QCDNUM::iqfrmq(q2b);                     //bottom threshold
      QCDNUM::setcbt(nfin,iqc,iqb,999);             //thresholds in the VFNS
      int iq0 = QCDNUM::iqfrmq(q0);                           //start scale
      QCDNUM::evolfg(ityp,func,def,iq0,eps);                 //evolve all pdf's

      // output the results
      const std::vector<double> xvals{1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 0.1, 0.3, 0.5, 0.7, 0.9};
      double pdf[13];
      std::cout << std::scientific;
      std::cout.precision(5);
      std::cout << "\nAlphaQCD(Q) = " << QCDNUM::asfunc(pow(mu, 2),nfout,ierr) << std::endl;
      std::cout << "x\t\tu-ubar\t\t-d+dbar\t\t-L^-\t\t-2L^+\t\ts+sbar\t\tc+cbar\t\tb+bbar\t\tgluon" << std::endl;
      for (double x : xvals)
	{
	  QCDNUM::allfxq(ityp, x, pow(mu, 2), pdf, 0, 1);
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
	  QCDNUM::allfxq(ityp, xvals[i], pow(mu, 2), pdf, 0, 1);
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

  return 0;
}
