#include <cassert>
#include <vector>
#include <iostream>
#include "containers/include/imodel_feedback_calc.h"
#include "containers/include/scenario.h"
#include "solution/util/include/solution_info.h"

using namespace std;
using namespace xercesc;

/*!
 * \ingroup Objects
 * \brief Writes out supply & demand curves for user-designated markets
 * \details 
 *
 * \author Rich Plevin
 */
class SupplyDemandCurveSaver : public IModelFeedbackCalc
{
public:
    SupplyDemandCurveSaver();
    virtual ~SupplyDemandCurveSaver();
    
    static const string& getXMLNameStatic();
    
    // INamed methods
    virtual const string& getName() const;
    
    // IParsable methods
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    
    // IRoundTrippable methods
    virtual void toInputXML( ostream& aOut, Tabs* aTabs ) const;
    
    // IModelFeedbackCalc methods
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario,
                                            const IClimateModel* aClimateModel,
                                            const int aPeriod );
    
    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario,
    					                   const IClimateModel* aClimateModel,
                                           const int aPeriod );

    virtual void printCSV( ostream& aOut, Scenario* aScenario, const int aPeriod, bool aPrintHeader );

    virtual int getMarketIndex(const string& aMarketName, vector<SolutionInfo> &aSolvable );

protected:
    //! The name of this feedback
    std::string mName;
    
    bool mIsPricesRelative;
    
    std::vector<double>mPrices;

    static std::ios_base::openmode mOpenMode;
};
