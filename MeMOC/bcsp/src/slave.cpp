/**@file  slave.cpp
 * @brief  slave bidimensional cutting stock (CS) problem (LP relaxation)
 * @author Alberto De Bortoli
 * implementation
 */

#include "slave.hpp"
#include <sstream>
#include "scip_exception.hpp"

using namespace std;
using namespace scipMeMOC;

/* constructor */

scipMeMOC::CSSlave::CSSlave( const vector<double> & profit, const vector<double> & length, const double & L ) : _scip(0) {
	// initialize scip
	SCIP_CALL_EXC( SCIPcreate(& _scip) );

	// load default plugins like separators, heuristics, etc.
	SCIP_CALL_EXC( SCIPincludeDefaultPlugins(_scip) );

	// disable scip output to stdout
	SCIP_CALL_EXC( SCIPsetMessagehdlr(NULL) );

	// create an empty problem
	SCIP_CALL_EXC( SCIPcreateProb(_scip, "queens", NULL, NULL, NULL, NULL, NULL, NULL) );

	// set the objective sense to maximize
	SCIP_CALL_EXC( SCIPsetObjsense(_scip, SCIP_OBJSENSE_MAXIMIZE) );

	// create an integer variable for every item/width
	ostringstream namebuf;
	for(unsigned int i = 0; i < profit.size(); ++i) {
		SCIP_VAR * var;
		namebuf.str("");
		namebuf << "x#" << i;
		       
		// create the SCIP_VAR object
		SCIP_CALL_EXC( SCIPcreateVar(_scip, & var, namebuf.str().c_str(), 0.0, SCIPinfinity(_scip), profit[i], SCIP_VARTYPE_INTEGER, TRUE, FALSE, NULL, NULL, NULL, NULL) );
		       
		// add the SCIP_VAR object to the scip problem
		SCIP_CALL_EXC( SCIPaddVar(_scip, var) );
		       
		// storing the SCIP_VAR pointer for later access
		_x.push_back(var);
	}

	// create KP constraint
	SCIP_CONS * cons;
	namebuf.str("");
	namebuf<<"KP";

	// create SCIP_CONS object
	SCIP_CALL_EXC( SCIPcreateConsLinear(_scip, & cons, namebuf.str().c_str(), 0, NULL, NULL, 0, L,
						  TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE) );
	
	// add the vars belonging to field in this row to the constraint
	for (unsigned int i = 0; i < profit.size(); ++i) {
		SCIP_CALL_EXC( SCIPaddCoefLinear(_scip, cons, _x[i], length[i]) );
	}

	// add the constraint to scip
	SCIP_CALL_EXC( SCIPaddCons(_scip, cons) );
	
	// store the constraint for later on
	_cons = cons;	
	     
}

/* display the solution */
void scipMeMOC::CSSlave::display(ostream& out) {
	// get the best found solution from scip
	SCIP_SOL * sol = SCIPgetBestSol(_scip);
	// when SCIP did not succeed then sol is NULL
	if (sol == NULL) {
		out << "no solution found" << endl;
		return;
	}
 
	out << "\tdual constraint value: " << SCIPgetSolOrigObj(_scip, sol) << endl;
	out << "\tNew pattern [ ";

	for ( unsigned int i=0; i < _x.size()-1; i++ ){
		out << SCIPgetSolVal(_scip, sol, _x[i]) << " : "; 
	}
	out << SCIPgetSolVal(_scip, sol, _x[_x.size()-1]) << " ]" << endl;
}
	
SCIP_Real scipMeMOC::CSSlave::getSolution (vector<int> & varSol) {
	SCIP_SOL * sol = SCIPgetBestSol(_scip);
	for ( unsigned int i=0; i < _x.size(); i++ ){
		varSol[i] = scipMeMOC::L_roundDoubleToInt(_scip,SCIPgetSolVal(_scip, sol, _x[i]));
	}
	return SCIPgetSolOrigObj(_scip, sol);
}

/* destructor */
scipMeMOC::CSSlave::~CSSlave(void) {
	// since the SCIPcreateVar captures all variables, we have to release them now
	for (unsigned int i = 0; i < _x.size(); ++i) {
		SCIP_CALL_EXC( SCIPreleaseVar(_scip, & _x[i]) );
	}
	_x.clear();
	
	// the same for the constraints
	SCIP_CALL_EXC( SCIPreleaseCons(_scip, &_cons) );
	
	// after releasing all vars and cons we can free the scip problem
	// remember this has allways to be the last call to scip
	SCIP_CALL_EXC( SCIPfree( & _scip) );
	
	_scip = NULL;
}

/* solve the slave problem */
void scipMeMOC::CSSlave::solve(void) {
	// this tells scip to start the solution process
	SCIP_CALL_EXC( SCIPsolve(_scip) );
}

inline int scipMeMOC::L_roundDoubleToInt(SCIP * scip, double x) {
	if ( SCIPisFeasEQ(scip, x, int(x)) ) {
		return int(x);
	}
	return int(x) + 1;
}
