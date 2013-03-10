/**@file  master.cpp
 * @brief  master bidimensional cutting stock (CS) problem (LP relaxation)
 * @author Alberto De Bortoli
 * implementation
 */


#include "master.hpp"
#include <sstream>
#include "scip_exception.hpp"

using namespace std;
using namespace scipMeMOC;


/* constructor */
scipMeMOC::CSMaster::CSMaster(char * fname) : _scip(0), _cons() {
	// initialize instance data
	readData(fname);

	// create a mono-width pattern for every width
	for (int j = 0; j < _numPieces; j++ ) {
		vector<int> p;
		for (int i = 0; i < _numPieces; i++ ) {
			if (i == j) {
				p.push_back(int(_rectLength / _length[i]) * int(_rectWidth / _width[i]) );
			} else {
			p.push_back(0);
		}
      }
      _pattern.push_back(p);
	}
	_numPatterns = _numPieces;

	_scip = NULL;
	_flagIntegerSolution = false;
	updateScipLP();
}

void scipMeMOC::CSMaster::updateScipLP() {
	if (_scip != NULL) {
		cleanScip();
	}

	// initialize scip
	SCIP_CALL_EXC( SCIPcreate(& _scip) );

	// load default plugins like separators, heuristics, etc.
	SCIP_CALL_EXC( SCIPincludeDefaultPlugins(_scip) );

	// disable scip output to stdout
	SCIP_CALL_EXC( SCIPsetMessagehdlr(NULL) );

	// disable presolving: neccesary to obtain usable dual data!
	SCIP_CALL_EXC( SCIPsetIntParam(_scip, "presolving/maxrounds", 0) );
	SCIP_CALL_EXC( SCIPsetIntParam(_scip, "propagating/maxroundsroot", 0) );

	// create an empty problem
	SCIP_CALL_EXC( SCIPcreateProb(_scip, "Cs1dmaster", NULL, NULL, NULL, NULL, NULL, NULL) );

	// set the objective sense to minimize
	SCIP_CALL_EXC( SCIPsetObjsense(_scip, SCIP_OBJSENSE_MINIMIZE) );

	// create a continuous variable for every pattern
	ostringstream namebuf;
   
	for(int j = 0; j < _numPatterns; ++j) {
		SCIP_VAR * var;
		namebuf.str("");
		namebuf << "x#" << j;
 
		// create the SCIP_VAR object
		SCIP_Vartype varType;
		if (_flagIntegerSolution ) {
			varType = SCIP_VARTYPE_INTEGER;
		} else {
			varType = SCIP_VARTYPE_CONTINUOUS;
		}
		SCIP_CALL_EXC( SCIPcreateVar(_scip, & var, namebuf.str().c_str(), 0.0, SCIPinfinity(_scip), 1.0, varType, TRUE, FALSE, NULL, NULL, NULL, NULL) );

		// add the SCIP_VAR object to the scip problem
		SCIP_CALL_EXC( SCIPaddVar(_scip, var) );

		// storing the SCIP_VAR pointer for later access
		_x.push_back(var);
	}

	// create constraints
	for (int i = 0; i < _numPieces; ++i) {
		SCIP_CONS * cons;
		namebuf.str("");
		namebuf<<"dem_"<<i;

		// create SCIP_CONS object
		SCIP_CALL_EXC( SCIPcreateConsLinear(_scip, & cons, namebuf.str().c_str(), 0, NULL, NULL, double(_demand[i]), SCIPinfinity(_scip), TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE/*necessary avoid prepocessing deletion and obtain dual data*/, FALSE, FALSE, FALSE) );

		// add the vars belonging to field in this row to the constraint
		for (int j = 0; j < _numPatterns; ++j) {
			SCIP_CALL_EXC( SCIPaddCoefLinear(_scip, cons, _x[j], MAX(_pattern[j][i],_zero)) );
		}

		// add the constraint to scip
		SCIP_CALL_EXC( SCIPaddCons(_scip, cons) );

		// store the constraint for later on
		_cons.push_back(cons);
	}
}

/* display the solution */
void scipMeMOC::CSMaster::display(ostream& out) {
	// get the best found solution from scip
	SCIP_SOL * sol = SCIPgetBestSol(_scip);
	// when SCIP did not succeed then sol is NULL
	if (sol == NULL) {
		out << "no solution found" << endl;
		return;
	}

	out << "Number of sheets to use: " << SCIPgetSolOrigObj(_scip, sol) << endl;
	
	for (int j = 0; j < _numPatterns; ++j) {
		out << SCIPgetSolVal(_scip, sol, _x[j]) << "\tof patterns [ ";
		for ( int i=0; i < _numPieces-1; i++ ){
			out << _pattern[j][i] << " : "; 
		}
		out << _pattern[j][_numPieces-1] << " ]" << endl;
	}
	out << endl;
}

void scipMeMOC::CSMaster::cleanScip(void) {
	if (_scip == NULL) {
		return;
	}

	// since the SCIPcreateVar captures all variables, we have to release them now
	for (vector<int>::size_type j = 0; j < _x.size(); ++j) {
		SCIP_CALL_EXC( SCIPreleaseVar(_scip, & _x[j]) );
	}
	_x.clear();

	// the same for the constraints
	for (vector<SCIP_CONS *>::size_type i = 0; i < _cons.size(); ++i)
		SCIP_CALL_EXC( SCIPreleaseCons(_scip, &_cons[i]) );
	_cons.clear();

	// after releasing all vars and cons we can free the scip problem
	// remember this has allways to be the last call to scip
	SCIP_CALL_EXC( SCIPfree( & _scip) );

	_scip = NULL;
}

/* destructor */
scipMeMOC::CSMaster::~CSMaster(void) {
	if (_scip != NULL) {
		cleanScip();
	}

	_pattern.clear();
	_demand.clear();
	_width.clear();
	_length.clear();
}

/* solve the master problem */
void scipMeMOC::CSMaster::solveLP(void) {
	setFlagInteger(false);
	// this tells scip to start the solution process
	SCIP_CALL_EXC( SCIPsolve(_scip) );
}

/* solve the (restricted) master problem to integrality*/
void scipMeMOC::CSMaster::solveInteger(void) {
	setFlagInteger(true);
	// this tells scip to start the solution process
	SCIP_CALL_EXC( SCIPsolve(_scip) );
}

void scipMeMOC::CSMaster::setFlagInteger ( bool isInteger ) {
	if ( _flagIntegerSolution == isInteger )
		return;
	_flagIntegerSolution = isInteger;
	updateScipLP();
}

const vector<double> & scipMeMOC::CSMaster::getDualValues ( void ) {
	_u.clear();
	for ( int i=0; i < _numPieces; i++ ){
		SCIP_CONS* cons;
		SCIPgetTransformedCons(_scip, _cons[i], &cons );
		_u.push_back(SCIPgetDualsolLinear(_scip, cons));
	}
	return _u;
}

/* add an item */
int scipMeMOC::CSMaster::addPattern ( const vector<int> & newPattern ) {
	_pattern.push_back(newPattern);
	_numPatterns++;
	updateScipLP();
	return 1;
}

/* read data from file */
int scipMeMOC::CSMaster::readData(char * fname) {
	int iBuf;
	double wBuf, lBuf;
	FILE * fpdata = fopen(fname,"r");
	if ( fpdata == NULL ) {
		cout << "ERROR reading " << fname << endl;
		exit(-1);
	}
	fscanf(fpdata,"param zero := %lf ;\n",&_zero);
	fscanf(fpdata,"param rect_width := %lf ;\n",&_rectWidth);
	fscanf(fpdata,"param rect_length := %lf ;\n",&_rectLength);
	fscanf(fpdata,"param numPieces := %d ;\n",&_numPieces);

	_width.reserve(_numPieces);
	_length.reserve(_numPieces);
	_demand.reserve(_numPieces);   
	fscanf(fpdata,"param WIDTHS: LENGTHS: ORDERS :=\n");
	for (int i = 0; i<_numPieces; i++) {
		fscanf(fpdata,"%lf",&wBuf);
		fscanf(fpdata,"%lf",&lBuf);
		fscanf(fpdata,"%d\n",&iBuf);
		_width.push_back(wBuf);
		_length.push_back(lBuf);
		_demand.push_back(iBuf);
	}
	fscanf(fpdata,"%*s");
	fclose(fpdata);
 
#ifdef L_DEBUG
cout << _rectLength << " " << _numPieces << endl;
for (int i=0; i<_numPieces; i++) {
	cout << i << ") " << _width[i] << "\t" << _demand[i] << endl;
}
#endif   
	return _numPieces;
}
