/*! 
* \file SavePoint.cpp
* \ingroup CIAM
* \brief SavePoint class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include "SavePoint.h"
#include "XMLHelper.h"
#include "Logger.h"

using namespace std;

//! get the price
double SavePoint::getPrice() const {
   return price;
}

/*! \brief Write out XML for debugging purposes.
*
* This method is called by the Market::toDebugXML method to write out information for each individual SavePoint.
*
* \param out Output stream to print to.
* \return void
*/
void SavePoint::toDebugXML( ostream& out ) const {
   
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<SavePoint>" << endl;
   
   // increase the indent.
   Tabs::increaseIndent();

   XMLWriteElement( price, "price", out );
   XMLWriteElement( demand, "demand", out );
   XMLWriteElement( supply, "supply", out );
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</SavePoint>" << endl;
}

// Print the point in a csv format.
void SavePoint::print( Logger* sdLog ) const {
   LOG( sdLog, Logger::WARNING_LEVEL ) << price << "," << demand << "," << supply << endl;
}

