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
class Tabs;
class IInfo;

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
    ILandAllocator();
    
    virtual ~ILandAllocator();

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
     * \param aPeriod The period corresponding to the agricultural production
     *        technology which is adding a land use.
     */
    virtual void addLandUsage( const std::string& aLandType,
                               const std::string& aProductName,
                               const LandUsageType aLandUsageType,
                               const int aPeriod ) = 0;

    /*!
     * \brief Set the calibrated observed yield for a given product within a land type.
     * \details Determines the appropriate land leaf and sets the calibrated
     *          observed yield for a given product.
     * \param aLandType Land type of the product.
     * \param aProductName Name of the product.
     * \param aCalObservedYield Calibrated observed yield.
     * \param aPeriod Period.
     * \author James Blackwood
     */
    virtual void setCalObservedYield( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const double aCalObservedYield, 
                                      const int aPeriod ) = 0;

    /*!
     * \brief Sets the intrinsic rate for a given product.
     * \details Determines the appropriate land leaf and sets the intrinsic rate
     *          for a given period.
     * \param aRegionName Name of the containing region.
     * \param aLandType Land type of the product.
     * \param aProductName Name of the product.
     * \param aIntrinsicRate Intrinisic rate of the product.
     * \param aPeriod Model period.
     * \author James Blackwood
     */
    virtual void setIntrinsicRate( const std::string& aRegionName,
                                   const std::string& aLandType,
                                   const std::string& aProductName,
                                   const double aIntrinsicRate,
                                   const int aPeriod ) = 0;

    /*!
     * \brief Returns the calibrated average observed intrinsic rate for
     *        unmanaged land.
     * \details The calibrated average observed Intrinsic rate for the unmanaged
     *          land nest is equal to the intrinsic rate for the unmanaged land
     *          node successively divided by its share to the power of the sigma
     *          parameter for each level.  When there are multiple unmanaged land
     *          nests this function finds the unmanaged land nest that
     *          corresponds to the passed in land type. In that case the
     *          intrinsic rate will be successively divided up until the
     *          conceptual root is reached.
     * \author James Blackwood
     * \author Steve Smith
     * \author Jim Naslund
     * \param aPeriod model period.
     * \param aLandType The type of land to find the rate for.
     * \return The average observed rate for the subtree containing this land
     *         type.
     */
    virtual double getUnmanagedCalAveObservedRate( const int aPeriod,
                                                   const std::string& aLandType ) const = 0;
    
    /*!
     * \brief Get the amount of land allocated for a type of land.
     * \param aLandType The land type.
     * \param aProductName Product name.
     * \param aPeriod Model period.
     * \return The land allocated for the product.
     */
    virtual double getLandAllocation( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const int aPeriod ) const = 0;

    /*!
     * \brief Apply an annual agricultural productivity change to the land
     *        associated with the given product.
     * \details Increases the cummulative technical change for the leaf using
     *          the supplied annual technical change for the years specified by
     *          the given period. The cummulative technical change is then
     *          applied to the intrinsic yield mode.
     * \param aLandType The land type.
     * \param aProductName Name of the product.
     * \param aAgProdChange Amount of technical change to apply.
     * \param aPeriod Period.
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
     * \param aHarvestPeriod Period in which the crop will be harvested.
     * \param aCurrentPeriod The current period.
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
     * \brief Sets the calibrated land allocation for a given product.
     * \details Determines the appropriate land leaf and sets the calibrated
     *          quantity of land use for a given period.
     * \param aLandType Land type of the product.
     * \param aProductName Name of the product.
     * \param aCalLandUsed Calibrated quantity of land.
     * \param aHarvestPeriod Period in which the land will be harvested.
     * \param aCurrentPeriod Period in which the calibrated quantity was
     *        observed.
     * \note This function must be called in order of increasing periods.
     * \author James Blackwood
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

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;
};

// Inline function definitions.

//! Constructor
inline ILandAllocator::ILandAllocator(){
}

//! Destructor.
inline ILandAllocator::~ILandAllocator(){
}

#endif // _ILANDALLOCATOR_H_
