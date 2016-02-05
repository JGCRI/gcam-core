#include <iostream>

#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>

#include <boost/mpl/vector.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/remove_if.hpp>

#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/any.hpp>
#include <boost/fusion/include/accumulate.hpp>

#include "testing_classes_def.hpp"

template<typename SubClassFamilyVector>
class Factory {
    using CreatableVector = typename boost::mpl::remove_if<SubClassFamilyVector, boost::is_abstract<boost::mpl::_> >::type;
    using SubClassFamilyVectorPtr = typename boost::mpl::transform<SubClassFamilyVector, boost::add_pointer<boost::mpl::_> >::type;
    using CreatableVectorPtr = typename boost::mpl::transform<CreatableVector, boost::add_pointer<boost::mpl::_> >::type;
    using FamilyBasePtr = typename boost::mpl::front<SubClassFamilyVectorPtr>::type;

    public:
    bool canCreateType( const std::string& aXMLName ) const {
        // TODO: no early exit option in any/for_each etc
        typename boost::fusion::result_of::as_vector<CreatableVectorPtr>::type asFusionVec;
        return boost::fusion::any( asFusionVec,
            [&aXMLName] ( auto aType ) -> bool {
                return aType->getXMLNameStatic() == aXMLName;
            } );
    }

    FamilyBasePtr createType( const std::string& aXMLName ) const {
        // TODO: no early exit option in any/for_each etc
        FamilyBasePtr nullPtr = 0;
        typename boost::fusion::result_of::as_vector<CreatableVectorPtr>::type asFusionVec;
        FamilyBasePtr ret = boost::fusion::accumulate( asFusionVec, nullPtr,
            [&aXMLName] ( const FamilyBasePtr& aCurrResult, auto aType  ) -> FamilyBasePtr {
                return !aCurrResult && aType->getXMLNameStatic() == aXMLName ? new typename boost::remove_pointer<decltype( aType )>::type : aCurrResult;
            } );
        if( !ret ) {
            std::cout << "Could not create unknown type " << aXMLName << std::endl;
        }
        return ret;
    }
};

void runCanCreateTests( const Factory<BaseFamily>& aBaseFactory, const std::string aXMLName ) {
    std::cout << "Knows " << aXMLName << "? " << aBaseFactory.canCreateType( aXMLName ) << std::endl;
}

void runCreateTests( const Factory<BaseFamily>& aBaseFactory, const std::string aXMLName ) {
    AbstractBase* b = aBaseFactory.createType( aXMLName );
    if( b ) {
        std::cout << "Do doSomethingVirtual for " << aXMLName << " = " << b->doSomethingVirtual() << std::endl;
        delete b;
    }
}

int main() {
    Factory<BaseFamily> baseFactory;
    std::string tests[] = { "Base", "D1", "D2", "D3", "D4" };
    for( auto str : tests ) {
        runCanCreateTests( baseFactory, str );
        runCreateTests( baseFactory, str );
    }
    return 0;
}

