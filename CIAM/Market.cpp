#include "Definitions.h"
#include <iostream>
#include <vector>
#include "modeltime.h"  // model runtime info
#include "XMLHelper.h"
#include "Market.h"

using namespace std; // enables elimination of std::

//! Default constructor.
Market::Market(){
	
	year = 0;
	price = 0;
	tprice = 0;
	demand = 0;
	tdemand = 0;
	supply = 0;
	demMktSupply = 0;
	tsupply = 0;
	exdmd = 0;
	lexdmd = 0;
	texdmd = 0;
	dexdmd = 0;
	ldem = 0;
	lsup = 0;
	type = NORMAL;
	solveMarket = false;
	
}

//! Write out XML for debugging purposes.
void Market::toDebugXML( const int period, ostream& out ) const {
	
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<Market name=\""<< name << "\">" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	XMLWriteElement( region, "region", out );
	XMLWriteElement( type, "type", out );
	XMLWriteElement( year, "year", out );
	XMLWriteElement( price, "price", out );
	XMLWriteElement( tprice, "tprice", out );
	XMLWriteElement( demand, "demand", out );
	XMLWriteElement( tdemand, "tdemand", out );
	XMLWriteElement( supply, "supply", out );
	XMLWriteElement( tsupply, "tsupply", out );
	XMLWriteElement( exdmd, "exdmd", out );
	XMLWriteElement( lexdmd, "lexdmd", out );
	XMLWriteElement( texdmd, "texdmd", out );
	XMLWriteElement( dexdmd, "dexdmd", out );
	XMLWriteElement( ldem, "ldem", out );
	XMLWriteElement( lsup, "lsup", out );
	
	for( vector<string>::const_iterator i = containedRegionNames.begin(); i != containedRegionNames.end(); i++ ) {
		XMLWriteElement( *i, "ContainedRegion", out );
	}
	// maybe add more to write out.
	
	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</Market>" << endl;
}
