#ifndef _APARSABLE_H_
#define _APARSABLE_H_
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
* \file aparsable.h  
* \ingroup Objects
* \brief Header file for the AParsable interface.
* \author Josh Lurz
*/

#ifdef USE_STANDALONE_RAPIDXML
namespace rapidxml {
template<typename Ch> class xml_node;
};
#else
namespace boost { namespace property_tree { namespace detail {
namespace rapidxml {
template<typename Ch> class xml_node;
};
} } }
namespace rapidxml = boost::property_tree::detail::rapidxml;
#endif

/*!
* \ingroup Objects
* \brief An interface to a class which can be parsed by the XMLParser.
* \details A classes should inherit from this class if they require some customized
*          behavior to parse it's data structures from XML.
* \author Pralit Patel
*/
class AParsable {
public:
    //! Virtual destructor so that instances of the interface may be deleted
    //! correctly through a pointer to the interface.
    inline virtual ~AParsable() {}
    
    /*!
     * \brief Perform custom behavior to parse Data out of XML DOM element.
     * \details Classes that implement this method will get called with a *reference*
     *          to the child XML nodes.  If the subclass is able to parse that node
     *          they should return true, otherwise default parsing behavior will be
     *          attempted to parse that XML node.  Because classes they will get access
     *          by reference they could, for instance, skip ahead and parse multiple
     *          child nodes at a time to avoid having them parsed by default behavior
     *          as well.
     * \param aNode A reference to the XML element which can be chnaged as needed to
     *              ensure all data gets processed correctly and default parsing behavior
     *              is not applied where not appropriate.
     * \return If true is returned the parser will move on to the next element without
     *         attempting to further parse.  If false is returned the element will still
     *         be attempted to parse using default behavior.
     */
    virtual bool XMLParse( rapidxml::xml_node<char>* & aNode ) {
        return false;
    }
};

#endif // _APARSABLE_H_
