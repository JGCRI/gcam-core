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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
 * \file stub_technology_container.cpp
 * \ingroup Objects
 * \brief StubTechnologyContainer class source file.
 * \author Pralit Patel
 */
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMImplementation.hpp>

#include "technologies/include/stub_technology_container.h"
#include "technologies/include/global_technology_database.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

//! Constructor
StubTechnologyContainer::StubTechnologyContainer() {
}

//! Destructor
StubTechnologyContainer::~StubTechnologyContainer() {
    delete mTechnology;
    
    // Note the XML adjustments's memory will be managed by their document.
}


/*!
 * \brief Cloning of stubs are not allowed.
 * \return Null.
 */
ITechnologyContainer* StubTechnologyContainer::clone() const {
    return 0;
}

const string& StubTechnologyContainer::getXMLNameStatic() {
    const static string XML_NAME = "stub-technology";
    return XML_NAME;
}

bool StubTechnologyContainer::XMLParse( const DOMNode* aNode ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( aNode );
    
    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, XMLHelper<void>::name() );
    
    // store the XML for later processing
    /*!
     * \warning This may shift some parsing errors to completeInit.
     */
    mXMLAdjustments.push_back( getDocumentToHoldNodes()->importNode( aNode, true ) );
    
    return true;
}

void StubTechnologyContainer::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    
    // The technology does not get written back out here, rather that will occur
    // in the global technology.  The XML adjustments do need to get written back
    // out however.
    for( CXMLIterator xmlIter = mXMLAdjustments.begin(); xmlIter != mXMLAdjustments.end(); ++xmlIter ) {
        // Note that we really want to write all the child nodes here instead of
        // the stub technology tag so that they all collapse into a single element.
        DOMNodeList* childNodes = ( *xmlIter )->getChildNodes();
        for( int childIndex = 0; childIndex < childNodes->getLength(); ++childIndex ) {
            // tell it to serialize the entire subtree of this child node
            XMLHelper<void>::serializeNode( childNodes->item( childIndex ), aOut, aTabs, true );
        }
    }
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void StubTechnologyContainer::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    mTechnology->toDebugXML( aPeriod, aOut, aTabs );
}

const string& StubTechnologyContainer::getName() const {
    return mName;
}

void StubTechnologyContainer::completeInit( const string& aRegionName,
                                            const string& aSectorName,
                                            const string& aSubsectorName,
                                            DependencyFinder* aDependencyFinder,
                                            const IInfo* aSubsecInfo,
                                            ILandAllocator* aLandAllocator )
{
    // get the technology from the global technology database
    const ITechnologyContainer* temp =
        GlobalTechnologyDatabase::getInstance()->getTechnology( aSectorName, aSubsectorName, mName );
    if( temp ) {
        mTechnology = temp->clone();
    }
    else {
        // If the global technology did not exist we can not go forward.  Note the
        // error message was printed by the global technology database.
        exit( 1 );
    }
    
    // Make the XML adjustments note that this may produce parsing errors.
    for( CXMLIterator xmlIter = mXMLAdjustments.begin(); xmlIter != mXMLAdjustments.end(); ++xmlIter ) {
        mTechnology->XMLParse( *xmlIter );
    }
    
    // now call complete init on the completed technology
    mTechnology->completeInit( aRegionName, aSectorName, aSubsectorName, aDependencyFinder,
                               aSubsecInfo, aLandAllocator );
}

void StubTechnologyContainer::initCalc( const string& aRegionName, const string& aSectorName,
                                        const IInfo* aSubsecInfo, const Demographic* aDemographic,
                                        const int aPeriod )
{
    mTechnology->initCalc( aRegionName, aSectorName, aSubsecInfo, aDemographic, aPeriod );
}

void StubTechnologyContainer::postCalc( const string& aRegionName, const int aPeriod ) {
    mTechnology->postCalc( aRegionName, aPeriod );
}

ITechnology* StubTechnologyContainer::getNewVintageTechnology( const int aPeriod ) {
    return mTechnology->getNewVintageTechnology( aPeriod );
}

const ITechnology* StubTechnologyContainer::getNewVintageTechnology( const int aPeriod ) const {
    return mTechnology->getNewVintageTechnology( aPeriod );
}

ITechnologyContainer::TechRangeIterator StubTechnologyContainer::getVintageBegin( const int aPeriod ) {
    return mTechnology->getVintageBegin( aPeriod );
}

ITechnologyContainer::CTechRangeIterator StubTechnologyContainer::getVintageBegin( const int aPeriod ) const {
    return mTechnology->getVintageBegin( aPeriod );}

ITechnologyContainer::TechRangeIterator StubTechnologyContainer::getVintageEnd() {
    return mTechnology->getVintageEnd();
}

ITechnologyContainer::CTechRangeIterator StubTechnologyContainer::getVintageEnd() const {
    return mTechnology->getVintageEnd();
}

void StubTechnologyContainer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    mTechnology->accept( aVisitor, aPeriod );
}

/*!
 * \brief Get a document so that we can control our own memory to store DOMNodes.
 * \details We have the DOM implementation create a single temporary document to
 *          store our temporary XML.
 * \return A document that will not get deleted.
 */
DOMDocument* StubTechnologyContainer::getDocumentToHoldNodes() {
    static DOMDocument* doc = DOMImplementation::getImplementation()->createDocument();
    
    return doc;
}
