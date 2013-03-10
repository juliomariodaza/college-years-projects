/**@file  slave.hpp
 * @brief  slave bidimensional cutting stock (CS) problem (LP relaxation)
 * @author Alberto De Bortoli
 */

#ifndef SLAVE_H
#define SLAVE_H

#include <vector>
#include <iostream>

#include <scip/scip.h>
#include <scip/scipdefplugins.h>
#include "lgeneral.hpp"

namespace scipMeMOC
{
	/**@class CSSlave
	 * @brief solver class for the LP relaxation of CS slave problem
	 *
	 *  this class implements a pattern generator for the LP relaxation of CS1D master problem, which will be solved using SCIP
	 */
	class CSSlave
	{
	private:

	/** @brief pointer to scip structure */
	SCIP * _scip;

	/** @brief one variable for each item/width */
	std::vector<SCIP_VAR *> _x;

	/** @brief KP constraint */
	SCIP_CONS * _cons;

	public:
	/** @brief constructs the slave programming model (KP)
	 *
	 * @param[in] fname the name of the file containing the instance
	 */
	CSSlave( const std::vector<double> & profit, const std::vector<double> & length, const double & L );

	/** @brief destructor */
	~CSSlave();

	void solve(void); ///< solves the slave problem using SCIPsolve

	/** @brief display the solution
	 *
	 * a simplex ASCII output function to display the solution of the slave problem
	 * @param[in,out] out ostream class for output(default cout)
	 */

	void display(std::ostream & out = std::cout);
	SCIP_Real getSolution ( std::vector<int> & varSol );
   
	};
}

#endif
