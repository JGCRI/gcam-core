#ifndef _ILANDALLOCATOR_H_
#define _ILANDALLOCATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file iland_allocator.h
 * \ingroup Objects
 * \brief The ILandAllocator interface file.
 * \author Josh Lurz
 */
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"

// Forward declarations
class Summary;
class Tabs;
class IInfo;
class GDP;

/*!
 * \brief The interface to a land allocation system.
 * \details This interface represents a method for agricultural production
 *          technologies to interact with a system for distributing land between
 *          usages.
 */
class ILandAllocator : public IVisitable,
                       public IParsable,
                       public IRoundTrippable {
public:
    inline ILandAllocator();
    
    inline virtual ~ILandAllocator();

    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    /*!
     * \brief An enum which represents the various types of land usages which
     *        may be dynamically added to the land allocator by production
     *        technologies.
     */
    enum LandUsageType {
        //! The land will be used for a food crop.
        eCrop,

        //! The land will be used for managed forestry.
        eForest 
    };

    /*!
     * \brief Add a product which will use land to the land allocator.
     * \details Informs the land allocator that a technology exists which will
     *          require a land leaf. This function must be called before any
     *          other functions are allowed to be called.
     * \param aLandType Land type the product will use.
     * \param aProductName Name of the product.
     * \param aLandUsageType The type of the land usage.
     */
    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName,
                               const LandUsageType aLandUsageType ) = 0;

    virtual void setCalObservedYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aCalObservedYield, 
                                      const int aPeriod ) = 0;

    /*!
     * \brief Set the intrinsic rate
     */
    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate,
                                   const int aPeriod ) = 0;

    /*!
     * \brief Returns the average observed rate for calibration.
     * \details During calibration the average observed rate is equal to the
     *          intrinsic rate of unmanaged land over its share raised to sigma.
     * \author Steve Smith
     * \return The average observed rate for this land allocator for
     *         calibration.
     */
    virtual double getCalAveObservedRate( const std::string& aLandType,
                                          const int aPeriod ) const = 0;
    
    /*!
     * \brief Get the amount of land allocated for a type of land.
     * \param aProductName Product name.
     * \param aPeriod Model period.
     * \return The land allocated for the product.
     */
    virtual double getLandAllocation( const std::string& aProductName,
                                      const int aPeriod ) const = 0;

    /*!
     * \brief Apply agricultural productivity change to the land associated with
     *        a product.
     * \param aLandType The land type.
     * \param aProductName Name of the product.
     */
    virtual void applyAgProdChange( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const double aAgProdChange,
                                    const int aPeriod ) = 0;

    /*!
     * \brief Calculate the yield for a land and product type given a profit
     *        rate.
     * \param aLandType Land type.
     * \param aProductName Product name.
     * \param aRegionName Region name.
     * \param aProfitRate Profit rate.
     * \param aPeriod Current period.
     * \param aHarvestPeriod Period in which the crop will be harvested.
     * \todo Should this also return the yield? That would help the technology.
     */
    virtual void calcYield( const std::string& aLandType,
                            const std::string& aProductName,
                            const std::string& aRegionName,
                            const double aProfitRate,
                            const int aHarvestPeriod,
                            const int aCurrentPeriod ) = 0;

    /*!
     * \brief Get the yield of a land and crop type.
     * \param aLandType Land type.
     * \param aProductName Product name.
     * \param aPeriod Model period.
     * \return The yield of the product.
     */
    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const = 0;

    /*!
     * \brief Set the known land allocation for a given product.
     * \param aLandType Land type.
     * \param aProductName Product name.
     * \param aCalLandUsed Quantity of land.
     * \param aHarvestPeriod Period in which the land will be harvested.
     * \param aPeriod Current model period.
     */
    virtual void setCalLandAllocation( const std::string& aLandType,
                                       const std::string& aProductName,
                                       const double aCalLandUsed,
                                       const int aHarvestPeriod, 
                                       const int aCurrentPeriod ) = 0;
    
    /*!
     * \brief Calculate the final land allocation once all yields are known.
     * \param aRegionName Region name.
     * \param aPeriod Model period.
     */
    virtual void calcFinalLandAllocation( const std::string& aRegionName, 
                                          const int aPeriod ) = 0;

    /*!
     * \brief Complete the initialization of the land allocator.
     * \param aRegionName Region name.
     * \param aInfo Local info object.
     */
    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo ) = 0;
    
    /*!
     * \brief Set the above and below ground carbon for the land type.
     * \details Agricultural technologies determine the above and below ground
     *          carbon content for the land they use. This function allows the
     *          agricultural technologies to set those values.
     * \param aLandType Land type.
     * \param aProductName Product name.
     * \param aAboveGroundCarbon Above ground carbon per unit of land.
     * \param aBelowGroundCarbon Below ground carbon per unit of land.
     * \param aPeriod Period.
     */
    virtual void setCarbonContent( const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aAboveGroundCarbon,
                                   const double aBelowGroundCarbon,
                                   const int aPeriod ) = 0;

    /*!
     * \brief Output information to the output CSV file.
     * \param aRegionName Name of the region.
     */
    virtual void csvOutput( const std::string& aRegionName ) const = 0; 
    
    /*!
     * \brief Output information to the Access database.
     * \param aRegionName Name of the region.
     */
    virtual void dbOutput( const std::string& aRegionName ) const = 0;
    
    /*!
     * \brief Calculate emissions from the land allocator.
     * \param aRegionName Region name.
     * \param aGDP Regional GDP container.
     * \param aPeriod Model period.
     */
    virtual void calcEmission( const std::string& aRegionName,
                               const GDP* aGDP,
                               const int aPeriod ) = 0;
    
    /*!
     * \brief Update the summary object with information about the land
     *        allocator for reporting.
     * \param aSummary Summary container.
     * \param aPeriod Period for which to update information.
     */
    virtual void updateSummary( Summary& aSummary, const int aPeriod ) = 0;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;
};

// Inline function definitions.

//! Constructor
ILandAllocator::ILandAllocator(){
}

//! Destructor.
ILandAllocator::~ILandAllocator(){
}

#endif // _ILANDALLOCATOR_H_
