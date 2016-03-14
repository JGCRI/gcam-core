#include <iostream>

#include "util/base/include/factory.h"

#include "testing_classes_def.hpp"

typedef AbstractBase::SubClassFamilyVector BaseFamily;

void runCanCreateTests( const std::string aXMLName ) {
    std::cout << "Knows " << aXMLName << "? " << Factory<BaseFamily>::canCreateType( aXMLName ) << std::endl;
}

void runCreateTests( const std::string aXMLName ) {
    AbstractBase* b = Factory<BaseFamily>::createType( aXMLName );
    if( b ) {
        std::cout << "Do doSomethingVirtual for " << aXMLName << " = " << b->doSomethingVirtual() << std::endl;
        delete b;
    }
}

int main() {
    std::string tests[] = { "Base", "D1", "D2", "D3", "D4" };
    for( auto str : tests ) {
        runCanCreateTests( str );
        runCreateTests( str );
    }
    return 0;
}

