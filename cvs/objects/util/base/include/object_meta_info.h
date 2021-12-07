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
 * object_meta_info.h
 * Created: 03/02/2007
 * Version: 04/20/2007
 *
 * This software, which is provided in confidence, was prepared by employees
 * of Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software
 * which should not be copied or otherwise disseminated outside your
 * organization without the express written authorization from Battelle.
 * All rights to the software are reserved by Battelle.   Battelle makes no
 * warranty, express or implied, and assumes no liability or responsibility
 * for the use of this software.
 */

#if !defined( __OBJECT_META_INFO_H )
#define __OBJECT_META_INFO_H    // prevent multiple includes

// include files ***********************************************************

#include "util/base/include/xml_helper.h"
#include "util/base/include/inamed.h"
#include "util/base/include/data_definition_util.h"

// namespaces **************************************************************

namespace ObjECTS {

// class: TObjectMetaInfo **************************************************

class TObjectMetaInfo : public INamed
{
public :

   typedef double   value_type;

   //! Default constructor
   TObjectMetaInfo(void) : mName(), mValue() {}
   /*! Copy constructor
    *  \param other the instance to copy
    */
   TObjectMetaInfo(const TObjectMetaInfo& other)
      : mName( other.mName ), mValue( other.mValue ) {}

   //! Destructor
   virtual ~TObjectMetaInfo(void) {}

   /*! Assignment operator
    *  \param other the instance to copy
    *  \return *this
    */
   TObjectMetaInfo& operator = (const TObjectMetaInfo& other)
   {
      if ( &other != this )
      {
         mName  = other.mName;
         mValue = other.mValue;
      }
      return *this;
   }

   /*! Get the name
    *  \return the name
    */
   virtual const std::string& getName( void ) const { return mName; }

   /*! Get the value
    *  \return the value
    */
   virtual const value_type& getValue( void ) const { return mValue; }

   /*! Get the XML tag name
    *  \return the XML tag name
    */
   static const std::string& getXMLNameStatic( void );
    
    const std::string& getXMLName( void ) const;

   /*! Set the name
    *  \param aName the name to set
    */
   virtual void setName( const std::string& aName ) { mName = aName; }

   /*! Set the value
    *  \param aValue the value to set
    */
   virtual void setValue( const value_type& aValue ) { mValue = aValue; }

   /*! \brief Serialize the object to an output stream in an XML format.
    *  \details Function which writes out all data members of an object which are
    *           necessary to duplicate a model run. This should not include
    *           internal state variables, only variables that were read-in or
    *           changed by calibration.
    *  \param aPeriod the period for which to report.
    *  \param aOut Stream into which to write.
    *  \param aTabs Object which controls formatting of the file.
    */
   virtual void toDebugXML(
      const int     aPeriod,
      std::ostream& aOut,
      Tabs*         aTabs ) const;

protected :

    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( TObjectMetaInfo ),

        //! Sector name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
    
        DEFINE_VARIABLE( SIMPLE, "value", mValue, value_type )
    )
};

// TObjectMetaInfoGetXMLName ***********************************************

/*! Get the XML tag name
 *  \return the XML tag name
 */
inline const std::string& TObjectMetaInfoGetXMLName( void )
{
   static const std::string XMLName = "object-meta-info";
   return XMLName;
}

// TObjectMetaInfo::getXMLNameStatic ************************************

/*! Get the XML tag name
 *  \return the XML tag name
 */
inline const std::string& TObjectMetaInfo::getXMLNameStatic( void )
{
   return TObjectMetaInfoGetXMLName();
}

inline const std::string& TObjectMetaInfo::getXMLName( void ) const
{
   return getXMLNameStatic();
}

// TObjectMetaInfo::toDebugXML ******************************************

/*! \brief Serialize the object to an output stream in an XML format.
 *  \details Function which writes out all data members of an object which are
 *           necessary to duplicate a model run. This should not include
 *           internal state variables, only variables that were read-in or
 *           changed by calibration.
 *  \param aPeriod the period for which to report.
 *  \param aOut Stream into which to write.
 *  \param aTabs Object which controls formatting of the file.
 */
inline void TObjectMetaInfo::toDebugXML( const int aPeriod,
   std::ostream& aOut, Tabs* aTabs ) const
{
   XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
   XMLWriteElement( mValue, "value", aOut, aTabs );
   XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

}  // namespace ObjECTS

#endif   // __OBJECT_META_INFO_H

// end of object_meta_info.h ***********************************************

