#ifndef _ALAND_ALLOCATOR_ITEM_H_
#define _ALAND_ALLOCATOR_ITEM_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file aland_allocator_item.h
 * \ingroup Objects
 * \brief The ALandAllocatorItem class header file.
 * \author James Blackwood
 */

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <boost/noncopyable.hpp>

#include "containers/include/tree_item.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"

// For LandUsageType enum.
#include "land_allocator/include/iland_allocator.h"

// Forward declarations
class IInfo;
class Tabs;
class LandUseHistory;
class LandNode;

/*!
* \brief An enum containing the possible types for items in a tree.
* \note This enum is defined outside the ALandAllocatorItem because
*       ALandAllocatorItem is a template class, so the enum would be a
*       dependent type.
*/
enum TreeItemType {
    /*!
    * \brief Node type.
    */
    eNode,

    /*!
    * \brief Leaf type.
    */
    eLeaf,

    /*!
    * \brief Any type, either node or leaf.
    */
    eAny
};

/*!
 * \brief A single item in the land allocator tree.
 * \details This is the abstract base class of all nodes and leaves in the land
 *          allocation tree, including the root. It inherits from the TreeItem
 *          class so that it can make use of the tree library functions.
 *
 *          <b>XML specification for ALandAllocatorItem</b>
 *          - XML name: Not parsed
 *          - Contained by: TreeLandAllocator
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 */
class ALandAllocatorItem : public TreeItem<ALandAllocatorItem>,
                           public IVisitable,
                           public IParsable,
                           public IRoundTrippable,
                           private boost::noncopyable
{
    friend class XMLDBOutputter;
public:
    typedef TreeItem<ALandAllocatorItem> ParentTreeType;

    explicit ALandAllocatorItem( const ALandAllocatorItem* aParent,
                                 const TreeItemType aType );
   
    virtual ~ALandAllocatorItem();

    // TreeItem methods
    virtual size_t getNumChildren() const = 0;

    virtual const ALandAllocatorItem* getChildAt( const size_t aIndex ) const = 0;
    
    virtual ALandAllocatorItem* getChildAt( const size_t aIndex ) = 0;
    
    // IParsable
    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    
    // IRoundTrippable
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const = 0;
    
    // IVisitable
    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;

    /*!
     * \brief Write datamembers to datastream in XML format for debugging
     *        purposes.  
     * \param aPeriod Model time period
     * \param aOut Output file for debugging purposes in XML format
     * \param aTabs Tabs object used to track the number of tabs to print.
     */
    void toDebugXML( const int aPeriod,
                     std::ostream& aOut,
                     Tabs* aTabs ) const;

    /*!
     * \brief Add a child to the land allocator item.
     * \param aChild Child to add.
     * \details Adds a child to the ALandAllocatorItems if this action is
     *          permissable. Not all ALandAllocatorItems support this operation.
     *          The child will not be added if it is not unique.
     */
    virtual void addChild( ALandAllocatorItem* aChild ) = 0;


    /*!
     * \brief Returns the name.
     * \author James Blackwood
     * \return the name of this ALandAllocatorItem
     */
    const std::string& getName() const;

    /*!
     * \brief Complete the initialization of the ALandAllocatorItem.
     * \param aRegionName Region name.
     * \param aInfo Local info object.
     */
    virtual void completeInit( const std::string& aRegionName, 
                               const IInfo* aRegionInfo ) = 0;

    /*!
     * \brief Complete the initialization of the ALandAllocatorItem.
     * \param aRegionName Region name.
     * \param aInfo Local info object.
     */
    virtual void initCalc( const std::string& aRegionName, 
                           const int aPeriod ) {};

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
                               const ILandAllocator::LandUsageType aLandUsageType,
                               const int aPeriod ) = 0;

    /*!
     * \brief Get the land allocated to a given product.
     * \details Returns the land allocated to a given product for harvest in the
     *          given period.
     * \param aLandType Land type.
     * \param aProductName Name of the product.
     * \param aPeriod Harvest period.
     * \return Land allocation.
     */
    virtual double getLandAllocation( const std::string& aLandType,
                                      const std::string& aProductName,
                                      const int aPeriod ) const = 0;

    /*!
     * \brief An enumeration of possible land allocation types.
     */
    enum LandAllocationType {
        // Managed land allocation.
        eManaged,

        //! Unmanaged land allocation.
        eUnmanaged,

        //! Any land allocation.
        eAnyLand
    };

    /*!
     * \brief Returns all land allocated for this land type.
     * \details Returns all land allocated for this land allocator item of a
     *          given type, including all items below it. This land will include
     *          land allocated to future harvests.
     * \param aType The type of land allocation to return: unmanaged, managed,
     *        or either.
     * \param aPeriod Model period.
     * \author Steve Smith
     * \return The total land allocated at or below the item.
     */
    virtual double getTotalLandAllocation( const LandAllocationType aType,
                                           const int aPeriod ) const = 0;

    /*!
     * \brief Determines the base land allocated to all items below this node.
     * \param aPeriod Period
     * \return Total base land allocated.
     * \todo Need a better term for "base land".
     */
    virtual double getBaseLandAllocation( const int aPeriod ) const = 0;

    /*!
     * \brief Sets the initial shares and land allocation.
     * \details
     * \warning Unmanged land allocations are not set properly. 
     * \todo Figure out a way to set the unmanaged land allocation leaves.
     * \param aRegionName Region name.
     * \param aSigmaAbove Sigma parameter of the parent.
     * \param aLandAllocationAbove Land allocation of the node above this item.
     * \param aParentHistoryShare Share of the passed in history object which is
     *        attributed to the parent of this item.
     * \param aParentHistory History object given by the parent. This may be the
     *        history object of a further ancestor.
     * \param aPeriod Period.
     * \author James Blackwood
     */
    virtual void setInitShares( const std::string& aRegionName,
                                const double aSigmaAbove,
                                const double aLandAllocationAbove,
                                const double aParentHistoryShare,
                                const LandUseHistory* aParentHistory,
                                const int aPeriod ) = 0;

    /*!
     * \brief Sets land allocation to read-in calibration value
     * \param aPeriod Period.
     * \author Steve Smith
     */
    virtual void resetToCalLandAllocation( const int aPeriod ) {};

    /*!
     * \brief Sets the intrinsic yield mode for the node and its' children.
     * \details
     * \param aIntrinsicYieldAbove Intrinsic yield of the parent.
     * \param aSigmaAbove Sigma parameter governing the distribution this item
     *        is within.
     * \param aPeriod Period.
     * \note This function must be called in order of increasing periods.
     * \todo find better way of specifying share for the intrinsic yield calc for good with no initial share - perhaps read in a "comparable" share
     * \author James Blackwood
     */
    virtual void setIntrinsicYieldMode( const double aIntrinsicYieldAbove,
                                        const double aSigmaAbove,
                                        const int aPeriod ) = 0;

    /*!
     * \brief Sets the intrinsic rate for a given product.
     * \details Determines the appropriate land leaf and sets the intrinsic rate
     *          for a given period.
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
     * \brief Set the calibrated observed yield for a given product within a
     *        land type.
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
                                    const int aHarvestPeriod, 
                                    const int aCurrentPeriod ) = 0;
    
    /*!
     * \brief This method will calculate the share value for each leaf and node,
     *          and then normalize it.
     * \details This function will be called from the supply side of the sectors
     *          and will be passed a default dummy sigmaAbove. The first loop
     *          cycles through all the children. If a child is a leaf then it
     *          will call the calcLandShares method in LandAllocatorLeaf, where
     *          share is calculated. If a child is a node there will be a
     *          recursive call to this method. The second loop uses the sum of
     *          all the shares of the mChildren vector and normalizes and
     *          overwrites share. Finally, share is calculated for this node
     *          using the calculated intrinsicRate and the sigma from one level
     *          up.
     * \param aRegionName Name of the containing region.
     * \param aSigmaAbove the sigma value from the node above this level.
     * \param aTotalLandAllocated Total base land allocated to the parent node.
     * \param aPeriod Model period.
     * \return The unnormalized share.
     * \author James Blackwood
     * \todo need a better way to check if "UnmanagedLand" to not overwrite
     *       intrinsicRate that was read in through input
     * \todo May need to add a method to deal with case if total allocation is
     *       greater than initial allocation 
     * \todo this will not work if unmanaged land nodes are nested
     */
    virtual double calcLandShares( const std::string& aRegionName,
                                   const double aSigmaAbove,
                                   const double aTotalBaseLand,
                                   const int aPeriod ) = 0;

    /*!
     * \brief Calculates the land allocation for all items in the land
     *        allocation tree.
     * \details Recursively calculates the landAllocation at each leaf and node
     *          using the shares. The land allocation is passed the value of 0
     *          at the root when this method is called, so the value in the land
     *          allocation variable at the root will not be changed and be
     *          passed down recursively.
     * \author Steve Smith, James Blackwood
     */
    virtual void calcLandAllocation( const std::string& aRegionName,
                                     const double aLandAllocationAbove,
                                     const int aPeriod ) = 0;

    //TODO: These next 3 functions need better names
    /*!
     * \brief This pass handles LUC flow from box models to the summer
     */
    virtual void calcLUCCarbonFlowsOut( const std::string& aRegionName,
                                            const int aYear ) {}

    /*!
     * \brief This pass handles LUC flow from the summer to box models.
     */
    virtual void calcLUCCarbonFlowsIn( const std::string& aRegionName,
                                              const int aYear ) {}

    /*!
     * \brief This pass handles box flows within this carbon box model.
     */
    virtual void calcCarbonBoxModel( const std::string& aRegionName,
                                             const int aYear ) {}
    
    /*!
     * \brief Calculates and stores the yield for a product.
     * \details Calculates the yield for a product based on the profit rate
     *          calculated by the Technology and the average intrinsic rate of
     *          the entire land allocation tree.
     * \param aLandType Land type of the product.
     * \param aProductName Name of the product.
     * \param aRegionName Name of the containing region.
     * \param aProfitRate Profit rate calculated by the Technology.
     * \param aAvgIntrinsicRate Average intrinsic rate of all land.
     * \param aHarvestPeriod Period in which the land will be harvested.
     * \param aCurrentPeriod Current period.
     * \author James Blackwood
     */
    virtual void calcYieldInternal( const std::string& aLandType,
                                    const std::string& aProductName,
                                    const std::string& aRegionName,
                                    const double aProfitRate,
                                    const double aAvgIntrinsicRate,
                                    const int aHarvestPeriod,
                                    const int aCurrentPeriod ) = 0;

    /*!
     * \brief Get the yield for a given product using a specified land type.
     * \param aLandType Land type of the product.
     * \param aProductName Name of the product.
     * \param aPeriod Period.
     * \return The yield of the product.
     */
    virtual double getYield( const std::string& aLandType,
                             const std::string& aProductName,
                             const int aPeriod ) const = 0;
    
    /*!
     * \brief Sets land allocation for unmanaged land nodes and leafs.
     * \details Production nodes have their land allocation set by the supply
     *          sectors that create them. The land allocation in unmanaged land
     *          nodes need to be set as the difference between the land
     *          allocated above and the land used in the rest of the children
     *          at this level. Unmanaged land leafs have an allocation read in,
     *          which acts as a relative share of their land -- this needs to be
     *          adjusted to be consistant with the land specified to be used in
     *          the production sectors.
     * \param aRegionName Name of the containing region.
     * \param aNewUnmanaged Total land to be allocated to this node.
     * \param aPeriod Period
     * \author Steve Smith
     */
    virtual void setUnmanagedLandAllocation( const std::string& aRegionName,
                                             const double aNewUnmanaged,
                                             const int aPeriod ) = 0;

    /*!
     * \brief Adjust land values for unmanaged land nodes.
     * \details Unmanaged land nodes must explicitly set their intrinsic rate
     *          because their is no associated supply sector.
     * \param aRegionName Region name.
     * \param aPeriod Model period
     * \author Steve Smith
     * \todo Change name of this function to something more clear (perhaps setUnmanagedLandRates).
     */
    virtual void setUnmanagedLandValues( const std::string& aRegionName,
                                         const int aPeriod ) = 0;

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
     * \brief Set the share of this land item.
     * \param aShare Share of the land allocated to the parent.
     * \param aPeriod Period.
     * \author James Blackwood
     */
    void setShare( const double aShare,
                   const int aPeriod );
    
    double getShare( const int aPeriod ) const;


    /*!
     * \brief Returns whether this node is an unmanaged nest.
     * \details An item is an unmanaged nest if all its children are either
     *          unmanaged nests or unmanaged leaves.
     * \return Whether this item is an unmanaged nest.
     */
    virtual bool isUnmanagedNest() const = 0;

    /*!
     * \brief Returns whether a node is a conceptual root.
     * \details An item is a conceptual root if its parent's sigma is 0 because
     *          that means this item does not share with its children.
     * \return Whether this item is a conceptual root.
     */
    virtual bool isConceptualRoot() const = 0;
 
    const ALandAllocatorItem* getParent() const;

    double getInstrinsicRate( const int aPeriod ) const;

    double getShare( const double aPeriod ) const;

    /*!
     * \brief Returns the sigma for this node.
     * \return double representing the sigma for this node.
     */
    virtual double getSigma() const = 0;

    TreeItemType getType() const;

    /*! 
     * \brief Write output to a CSV file specified by a global variable.
     * \param aRegionName Region name.
     */
    virtual void csvOutput( const std::string& aRegionName ) const = 0;

    /*! 
     * \brief Write output to a DB file specified by a global variable.
     * \param aRegionName Region name.
     */
    virtual void dbOutput( const std::string& aRegionName ) const = 0;

protected:
    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const = 0;

    virtual const std::string& getXMLName() const = 0;

    //! Parent of this node
    const ALandAllocatorItem* mParent;

    /*!
     * \brief Share of parent's total land.
     * \details This is equal to the land allocated to this node divided by land
     *          allocated to node above. This is always the normalized share and
     *          so is always between zero and one inclusive.
     */
    objects::PeriodVector<Value> mShare;
    
    //! Rate in dollars to rent the land
    objects::PeriodVector<Value> mIntrinsicRate;
    
    //! Name of the land allocator item. This is the name of the product for
    //! leafs and name of the type of land for nodes.
    std::string mName;

    /*!
     * \brief Enum that stores the item's type.
     * \note This is stored to avoid a virtual function call.
     */
    TreeItemType mType;

    double ALandAllocatorItem::getDefaultShare( ) const;
    
};

typedef std::unary_function<const ALandAllocatorItem*, bool> SearchPredicate;

/*!
 * \brief SearchPredicate that finds an unmanaged nest.
 * \details This predicate should be passed to TreeItem's findItem method.
 *          It will be called on each item during the search.
 */
struct IsUnmanagedNest : SearchPredicate {
    
    /*!
     * \brief Operator() that returns whether this item is an unmanaged nest.
     * \param aItem The item to check.
     * \return Whether this item is an unmanaged nest.
     */
    bool operator()( const ALandAllocatorItem* aItem ) const {
        return aItem->isUnmanagedNest();
    }
};

/*!
 * \brief SearchPredicate that finds an item with the desired type and name.
 * \details This predicate should be passed to TreeItem's findItem method.
 *          It will be called on each item during the search.
 */
struct MatchesTypeAndName : public SearchPredicate {
    
    /*!
     * \brief Enum that stores the desired type.
     */
    TreeItemType mType;

    /*!
     * \brief String that stores the desired name.
     */
    const std::string& mName;

    /*!
     * \brief Constructor.
     * \param aName The desired name.
     * \param aType The desired type.
     */
    explicit MatchesTypeAndName( const std::string& aName, TreeItemType aType )
    : mType( aType ),
      mName( aName )
    {}

    /*!
     * \brief Operator() that returns whether this item is the desired type and name.
     * \param aItem The item to check.
     * \return Whether this item matches the desired type and name.
     */
    bool operator()( const ALandAllocatorItem* aItem ) const {
        return ( mType == eAny || aItem->getType() == mType ) 
               && ( mName == aItem->getName() );
    }
};

#endif // _ALAND_ALLOCATOR_ITEM_H_


