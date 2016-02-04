#include <iostream>

#include <boost/utility/enable_if.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/has_xxx.hpp>

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

template<typename T>
void runTest(T obj) {
    obj.print();
    using BaseType = typename get_base_class<T>::type;
    static_cast<BaseType>(obj ).print();
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
    return 0;
}

