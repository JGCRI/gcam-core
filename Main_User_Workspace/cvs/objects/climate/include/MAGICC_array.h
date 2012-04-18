#ifndef _MAGICC_array_H_
#define _MAGICC_array_H_

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
 *  MAGICC_array.h
 *  magicc++
 *
 *  Created by d3x290-local on 5/6/10.
= *
 */
 

/*  MAGICC uses a whole series of arrays to track data with variable indices
    (typically from 226 to iTp). The magicc_array class mimics this functionality
    for us in C++, allowing a one- or two-dimensional array with arbitrary
    index ranges.
*/

#define MA_NAMELEN 20

class magicc_array {
    int initialized, low1, high1, low2, high2;
    float* data;
    char name[ MA_NAMELEN ];
private:
    int computepos( int, int );
    void copy( const magicc_array& array );
public:
    magicc_array();
    magicc_array( const magicc_array& array );
    ~magicc_array();
    magicc_array& operator=( const magicc_array& array );

    void init( const char*, int, int, int=0, int=0 );
    void setval( float, int, int=0 );
    float getval( int, int=0 );    
    float* getptr( int, int=0 );    
    void print();
};


#endif // _MAGICC_array_H_
