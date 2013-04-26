/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/

/*
 *  MAGICC_IO_helpers.cpp
 *  magicc++
 *
 *  Created by d3x290-local on 10/9/09.
 *
 */

#include <stdlib.h>
#include <iostream>
#include <string>
#include <fstream>

using namespace std;

// Small helper functions related to file I/O

void openfile_read( ifstream* infile, const string& f, bool echo )
{
    (*infile).open( f.c_str(), ios::in );
    if ( !infile ) {
        cerr << "Unable to open file " << f << " for read\n";
        exit( 1 ); 
    }
    if ( echo ) cout << "Opened file " << f << " for read OK\n";
}

void skipline( istream* infile, bool echo )
{
    string line;
    getline( *infile, line );
    
    if( echo ) cout << "Skipping line: " << line << endl;
}

float read_csv_value( istream* infile, bool echo )
{
    float f;
    char comma;
    (*infile) >> f >> comma;
    
    if ( echo ) cout << "Read " << f << ",";
    return f;
}

float read_and_discard( istream* infile, bool echo )
{
    float f;
    string line;
    
    getline( *infile, line );
    if (EOF == sscanf( line.c_str(), "%f", &f ) ) {
        cerr << "Error in reading data from line: " << line << "\n";
        exit( 1 );
    }
    if ( echo ) cout << "Read value " << f << " from line: " << line << endl;
    return f;
}

void openfile_write( ofstream* outfile, const string& f, bool echo )
{
    (*outfile).open( f.c_str(), ios::out );
    if ( !outfile ) {
        cerr << "Unable to open file " << f << " for write\n";
        exit( 1 ); 
    }
    if ( echo ) cout << "Opened file " << f << " for write OK\n";
}


