#ifndef _UNMANAGED_LAND_LEAF_H_
#define _UNMANAGED_LAND_LEAF_H_
#if defined(_MSC_VER)
#pragma once
#endif

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



/*!
 * \file unmanaged_land_leaf.h
 * \ingroup Objects
 * \brief The LandAllocatorLeaf class header file.
 * \author James Blackwood, Kate Calvin
 */
#include <memory>
#include "land_allocator/include/land_leaf.h"
class LandUseHistory;

/*!
 * \brief A type of leaf which contains unmanaged land.
 * \details Unmanaged land leaves represent land that is not currently used for
 *          crops or grazing, such as unmanaged forests. 
 *
 *          <b>XML specification for UnmanagedLandLeaf</b>
 *          - XML name: \c UnmanagedLandLeaf
 *          - Contained by: LandNode
 *          - Parsing inherited from class: None
 *          - Attributes:
 *              - \c name ALandAllocatorItem::mName
 */

class UnmanagedLandLeaf : public LandLeaf {
public:
    explicit UnmanagedLandLeaf( const ALandAllocatorItem* aParent );
    explicit UnmanagedLandLeaf();
    virtual ~UnmanagedLandLeaf();
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;
    
    virtual void setUnmanagedLandProfitRate( const std::string& aRegionName,
                                             double aAverageProfitRate,
                                             const int aPeriod ); 
   
    virtual double getCalLandAllocation( const LandAllocationType aType,
                                         const int aPeriod ) const;
    
    virtual void setProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const double aProfitRate,
                                   const int aPeriod );
	
	virtual bool isUnmanagedLandLeaf( )  const;
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        LandLeaf
    )

};

#endif // _UNMANAGED_LAND_LEAF_H_
