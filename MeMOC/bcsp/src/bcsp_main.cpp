/**@file bcsp_main.cpp
 * @brief main file for the Bidimensional Cutting Stock Problem
 * @author Alberto De Bortoli
 */

#include "master.hpp"
#include "slave.hpp"
#include <cstdlib>
#include <iostream>
#include "scip_exception.hpp"
#include <time.h>

using namespace std;
using namespace scipMeMOC;

/** main function for bcsp */
int main(int args, char ** argv)
{
	cout << "*********************************" << endl;
	cout << "* bcsp solver based on SCIP     *" << endl;
	cout << "*                               *" << endl;
	cout << "* (c) Alberto De Bortoli (2010) *" << endl;
	cout << "*********************************" << endl << endl;

	if (args < 2) {
		cerr << "call " << argv[0] << " <data file>" << endl;
		exit(1);
	}

	try {
		cout << "*** SOLVING CONTINUOUS RELAXATION ***" << endl;

		time_t start, end;
		time(&start);
		
		// initialize the master
		CSMaster master(argv[1]);

		// generate columns (cutting patterns)
		SCIP_Real slaveObj = 0.0;
		int maxiter = 100;

		// test if demanded pieces fit the stock rectangles
		bool feasible = true;
		for (unsigned int i=0; i<master.getWidths().size() && feasible; i++){
			if (!(master.getWidths()[i] <= master.getRectWidth()) ) {
				cout << "stock rectangular too much small, w[i] = " << master.getWidths()[i] << " can't fill (W = " << master.getRectWidth() << ")\n\n";
					feasible = false;
				}
		}
      
		// iif all the pieces fit the stock rectangles
		if (feasible) {
			do {
				cout << "-------------- iter" << -maxiter << " ----------------------" << endl;

				// solve the master problem
				master.solveLP();
				
				// display the solution on stdout
				master.display();
				
				// create a vector storing the Obj values obtained from the slave problems
				vector<double> sol_Obj_slaves(master.getWidths().size());
				
				// create a vector of vector storing the patterns of the slave problems 
				vector<vector<int> > patternsSlave;
				
				for (unsigned int i=0; i<master.getWidths().size(); i++){
					cout << "\n\nSOLVING SLAVE PROBLEM - slave-" << i << endl;
					
					// create a vector storing the *ad hoc* master dual variables
					vector<double> _u_adhoc(master.getWidths().size());
					
					/* fill the vector
					 * (set a negative value for demanded pieces that can't be filled in the strip
					 * so that the associated z soluzion var would not be considered) */
					for (unsigned int j = 0; j < master.getWidths().size(); j++){
						if (master.getWidths()[j] <= master.getWidths()[i])
							_u_adhoc[j] = master.getDualValues()[j];
						else
							_u_adhoc[j] = 0;
					}
							
					/* call the slave problems (one dimensional problems along L) with
					 * 1. master dual *ad hoc* variables
					 * 2. lengths of the pieces
					 * 3. rectangular length */
					CSSlave slave(_u_adhoc, master.getLengths(), master.getRectLength());
					slave.solve();
					slave.display();
					
					vector<int> _patternSlave(master.getWidths().size());
					// retrieve the solution (f.o.) and the pattern solution of the slave problem
					sol_Obj_slaves[i] = slave.getSolution(_patternSlave);
					// store the pattern for later access
					patternsSlave.push_back(_patternSlave);
				}
			
				cout << "\n\n*** SOLVING FINAL SLAVE (slave along W) ***" << endl;
		
				/* call the slave problem (one dimensional problem along W) with
				* 1. the profits retrieved form previous slave problems
				* 2. widths of the pieces (used for each slave problems)
				* 3. rectangular width */
				CSSlave slave(sol_Obj_slaves, master.getWidths(), master.getRectWidth() );
				slave.solve();
				slave.display();
					
				vector<int> newPattern(master.getWidths().size());
		    	// retrieve the solution (f.o.) and the pattern solution of the slave problem
				slaveObj = slave.getSolution(newPattern);
				
				
				/* if the value is greater than 1 is good to add the pattern to the master for a improvement of the solution
				 * violated dual constraint -> negative coefficient for associated variable for the new patter */
				if (slaveObj > 1) {
					// there is a strip pattern suitable for improvement 
					// need to create it form pattern solutions retrieved from slaves s1..m
					
					// create a vector storing the new pattern
					vector<int> newSumPattern(master.getWidths().size());
					
					for (unsigned int i = 0; i < newSumPattern.size(); i++) {
						newSumPattern[i] = 0;
						for (unsigned int j = 0; j < patternsSlave.size(); j++) {
							newSumPattern[i] = newSumPattern[i] + (patternsSlave[j][i] * newPattern[j]);
						}
					}
					
					// add the new calculated pattern to the master problem
		        	master.addPattern(newSumPattern);
		        }
				maxiter--;
			} while (slaveObj > 1 && maxiter > 0);
      
		// solve the master to integer      
		cout << "\n\n*** SOLVING TO INTEGRALITY ***" << endl;
		master.solveInteger();
		time(&end);
		master.display();

		/* print out the running time */
		printf("Running time: %.2lf seconds.\n\n", difftime(end, start));
	}
      
	} catch(SCIPException& exc) {
		cerr << exc.what() << endl;
		exit(exc.getRetcode());
	}
	
	return EXIT_SUCCESS;
}
