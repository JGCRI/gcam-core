#include <iostream>

#include <boost/type_traits.hpp>
#include <boost/utility/enable_if.hpp>

#include <boost/mpl/not.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/remove_if.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/unpack_args.hpp>

#include <boost/mpl/has_xxx.hpp>

#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/map.hpp>
#include <boost/fusion/include/at_key.hpp>
#include <boost/fusion/include/for_each.hpp>
#include <boost/fusion/include/mpl.hpp>


#include "testing_classes_def.hpp"

BOOST_MPL_HAS_XXX_TRAIT_DEF( ParentClass );

template<typename T, typename Enable=void>
struct get_base_class {
    using type = typename get_base_class<typename T::ParentClass>::type;
};

template<typename T>
struct get_base_class<T, typename boost::enable_if<boost::mpl::not_<has_ParentClass<T> > >::type> {
    using type = T;
};

template<typename T, typename Enable=void>
struct get_full_datavector_type {
    using type = boost::fusion::joint_view<typename get_full_datavector_type<typename T::ParentClass>::type, decltype( T::mDataVector )>;
};

template<typename T>
struct get_full_datavector_type<T, typename boost::enable_if<boost::mpl::not_<has_ParentClass<T> > >::type> {
    using type = boost::fusion::joint_view< decltype( T::mDataVector ), boost::fusion::vector<> >;
};

template<typename SubClassFamilyVector>
struct ExpandDataVector  {
    using SubClassVecPtr = typename boost::mpl::transform<SubClassFamilyVector, boost::add_pointer<boost::mpl::_> >::type;
    using SubClassPtrMap = typename boost::fusion::result_of::as_map<typename boost::fusion::result_of::as_vector<typename boost::mpl::transform_view<boost::mpl::zip_view< boost::mpl::vector<SubClassFamilyVector, SubClassVecPtr> >, boost::mpl::unpack_args<boost::fusion::pair<boost::mpl::_1, boost::mpl::_2> > > >::type>::type;
    SubClassPtrMap mSubClassPtrMap;

    ExpandDataVector() {
        reset();
    }

    void reset() {
        boost::fusion::for_each( mSubClassPtrMap, [] ( auto& aPair ) {
            aPair.second = 0;
        } );
    }

    template<typename SubClass>
    void setSubClass( SubClass* aSubClass ) {
        reset();
        boost::fusion::at_key<SubClass>( mSubClassPtrMap ) = aSubClass;
    }

    template<typename DataVecHandler>
    void getFullDataVector( DataVecHandler& aDataHandler ) const {
        boost::fusion::for_each( mSubClassPtrMap, [this, &aDataHandler] ( auto& aPair ) {
            if( aPair.second ) {
                aDataHandler.processData( gatherDataVector( aPair.second ) );
            }
        } );
    }

    protected:
    template<typename SubClass>
    typename boost::enable_if<has_ParentClass<SubClass>, typename get_full_datavector_type<SubClass>::type>::type gatherDataVector( SubClass* aSubClass ) const {
        auto parentDataVec = gatherDataVector( static_cast<typename SubClass::ParentClass*>( aSubClass ) );
        boost::fusion::joint_view< decltype( parentDataVec ), decltype( aSubClass->mDataVector )> fullDataVector( parentDataVec, aSubClass->mDataVector );
        return fullDataVector;
    }

    template<typename SubClass>
    typename boost::enable_if<boost::mpl::not_<has_ParentClass<SubClass> >, typename get_full_datavector_type<SubClass>::type>::type gatherDataVector( SubClass* aSubClass ) const {
        boost::fusion::vector<> emptyVector;
        boost::fusion::joint_view< decltype( aSubClass->mDataVector ), decltype( emptyVector )> baseDataVector( aSubClass->mDataVector, emptyVector );
        return baseDataVector;
    }
};
//template<typename T> struct PrintError;
//PrintError<boost::mpl::front<ExpandDataVector<BaseFamily>::FusionMap>::type> asdf;

struct PrintDataHandler {
    template<typename DataVectorType>
    void processData( DataVectorType aDataVector ) {
        boost::fusion::for_each( aDataVector, [] ( const auto& aData ) { std::cout << aData.mDataName << " = " << aData.mData << std::endl; } );
    }
};

template<typename T>
void runTest(T obj) {
  /*
    obj.print();
    using BaseType = typename get_base_class<T>::type;
    static_cast<BaseType>(obj ).print();
    */
}

int main() {
    Base b;
    runTest(b);
    D1 d1;
    runTest(d1);
    D2 d2;
    runTest(d2);
    D3 d3;
    runTest(d3);

    PrintDataHandler printer;
    ExpandDataVector<AbstractBase::SubClassFamilyVector> edv;
    edv.setSubClass(&b);
    edv.getFullDataVector(printer);
    edv.setSubClass(&d1);
    edv.getFullDataVector(printer);

    return 0;
}

