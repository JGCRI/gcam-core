/*! 
* \file output_helper.cpp
* \ingroup CIAM
* \brief Contians the functions to write out single records to output file or table in the database.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

#if(__HAVE_DB__)
#include <afxdisp.h>
#include <dbdao.h>
#include <comdef.h>
#endif

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <ctime>
#include "containers/include/scenario.h" 
#include "containers/include/world.h"

using namespace std;

#if(__HAVE_DB__)
extern CdbDatabase db;
extern CdbRecordset DBoutrst;
#endif

extern time_t ltime;
extern ofstream outFile;
extern Scenario* scenario;

/*! Output single records to file.
*
* Names of categories, subcategories, and variables are written
* as strings in the argument.  Values are also passed as arguments.
*
*/

void fileoutput3( string var1name,string var2name,string var3name,
			  string var4name,string var5name,string uname,vector<double> dout) {
	outFile <<var1name<<","<<var2name<<","<<var3name<<","
			<<var4name<<","<<var5name<<","<<uname<<",";
	for (int i=0;i< static_cast<int>( dout.size() );i++) {
		outFile << dout[i]<<",";
	}
	outFile <<ctime(&ltime);
}

/*! Output single records MiniCAM style to the database. 
*
* Names of categories, subcategories, and variables are written
* as strings in the argument.  Values are also passed as arguments.
*
*/
void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout)
{
#if(!__HAVE_DB__)
	fileoutput3( var1name,var2name,var3name,var4name,"",uname,dout);
#else
    const map<string,int> regionMap = scenario->getWorld()->getOutputRegionMap();

    // only output regions already defined in regionMap
    map<string,int>::const_iterator iter = regionMap.find( var1name );
    if( iter != regionMap.end()) {
	    int i=0,j;
	    // COleVariant does not take int, must be short or long
	    // now write to the database
	    // MUST Update before adding another
        try {
	        DBoutrst.AddNew(); // now the current record is this empty new one
        
	        tm* lt = localtime(&ltime); // struct for time components
	        // run id MiniCAM style
	        long RunID = lt->tm_sec + lt->tm_min * 100 + lt->tm_hour * 10000
                   + lt->tm_mday * 1000000 
				   + (lt->tm_mon * 100000000 + 100000000); // 0 is january

	        DBoutrst.SetField(0L, COleVariant(RunID,VT_I4)); // Bad line.
	        DBoutrst.SetField(1L, COleVariant(short(iter->second)));
	        DBoutrst.SetField(3L, COleVariant(var2name.c_str(), VT_BSTRT));
	        DBoutrst.SetField(4L, COleVariant(var3name.c_str(), VT_BSTRT));
	        DBoutrst.SetField(5L, COleVariant(var4name.c_str(), VT_BSTRT));
	        DBoutrst.SetField(6L, COleVariant(uname.c_str(), VT_BSTRT));
            // get scenario name and output to dbout
            string scenarioName = scenario->getName();
	        DBoutrst.SetField(7L, COleVariant(scenarioName.c_str(), VT_BSTRT));
	        for (i=0;i< static_cast<int>( dout.size() );i++) {
		        j = 8+i;
		        DBoutrst.SetField(j, COleVariant(dout[i]));
	        }
	        DBoutrst.Update(); // save and write the record
        
            } catch( _com_error e ){
                printf("Error:*************************************************\n");
                printf("Code = %08lx\n", e.Error());
                printf("Message = %s\n", e.ErrorMessage());
                printf("Source = %s\n", (LPCSTR) e.Source());
                printf("Description = %s\n", (LPCSTR) e.Description());
            }
        }
    // if region not found in regionMap, do nothing
#endif
}

