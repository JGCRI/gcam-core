#ifndef _AUTO_OUTPUT_FILE_H_
#define _AUTO_OUTPUT_FILE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file auto_output_file.h  
* \ingroup util
* \brief Header file for the AutoOutputFile class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>
#include <fstream>
#include <string>
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"

/*!
* \ingroup util
* \brief A class which wraps a file stream so that it is automatically opened and closed. 
* \author Josh Lurz
*/
class AutoOutputFile {
public:
    AutoOutputFile( const std::string& confVariableName, const std::string& defaultName = "outputFile.txt" ){
        const Configuration* conf = Configuration::getInstance();
        const std::string fileName = conf->getFile( confVariableName, defaultName );
        wrappedFile.open( fileName.c_str(), std::ios::out );
        util::checkIsOpen( wrappedFile, fileName );
    }

    ~AutoOutputFile(){
        wrappedFile.close();
    }
    template<class T>
    std::ostream& operator<<( T& value ){
        return wrappedFile << value;
    }
    std::ofstream& operator*(){
        return wrappedFile;
    }
protected:
    std::ofstream wrappedFile; //<! The wrapped file stream.
};

#endif // _AUTO_OUTPUT_FILE_H_