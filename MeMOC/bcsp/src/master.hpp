/**@file  master.hpp
 * @brief  master bidimensional cutting stock (CS) problem (LP relaxation)
 * @author Alberto De Bortoli
 */

#ifndef MASTER_H
#define MASTER_H

#include <vector>
#include <iostream>

#include <scip/scip.h>
#include <scip/scipdefplugins.h>

#include "lgeneral.hpp"

namespace scipMeMOC {
	/**@class CSMaster
	 * @brief solver class for the LP relaxation of CS master problem
	 *
	 *  this class implements a solver for the LP relaxation of CS1D master problem as an LP model, which will be solved using SCIP
	 */

	class CSMaster {
	private:

	/** @brief pointer to scip structure */
	SCIP * _scip;

	private:
		/** @brief number of widths  */
		int _numPieces;
		/** @brief offered widths  */
		std::vector<double> _width;
		/** @brief offered lengths  */
		std::vector<double> _length;
		/** @brief ordered units per width  */
		std::vector<int> _demand;
	
		/** @brief rect width   */
		double _rectWidth;
		/** @brief rect length   */
		double _rectLength;
	      
		/** @brief cutting patterns   */
		int _numPatterns;
		/** @brief profit per item  */
		std::vector<std::vector<int> > _pattern;

	public:
		const std::vector<double> & getWidths ( void ) {
			return _width;
		}
		const std::vector<double> & getLengths ( void ) {
			return _length;
		}
		const std::vector<double> & getDualValues ( void );
		const double & getRectWidth ( void ) {
			return _rectWidth;
		}
		const double & getRectLength ( void ) {
			return _rectLength;
		}

	private:
		/** @brief one variable for each pattern */
		std::vector<SCIP_VAR *> _x;

		/** @brief constraints for order satisfaction */
		std::vector<SCIP_CONS *> _cons;
		
		std::vector<double> _u;

	public:
	/** @brief constructs the integer programming model for the CS problem
	 *
	 * @param[in] fname the name of the file containing the instance
	 */

		CSMaster(char * fname );

		/** @brief destructor */
		~CSMaster();

		void solveLP(void);

		/** @brief display the solution
		 *
		 * a simplex ASCII output function to display the solution of the multi-KP problem
		 * @param[in,out] out ostream class for output(default cout)
		 */
		void display(std::ostream & out = std::cout);
		int addPattern ( const std::vector<int> & newPattern );

		void solveInteger ( void );

	private:
		int readData ( char * fname );
		double _zero;

		void updateScipLP ( void );
		void cleanScip ( void );

		bool _flagIntegerSolution;
		void setFlagInteger ( bool isInteger );
	};
}

#endif
