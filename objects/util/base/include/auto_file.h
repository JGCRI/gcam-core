#ifndef _AUTO_OUTPUT_FILE_H_
#define _AUTO_OUTPUT_FILE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file auto_file.h  
* \ingroup util
* \brief Header file for the AutoOutputFile class.
* \author Josh Lurz
*/

#include <iostream>
#include <fstream>
#include <string>
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"

/*!
* \ingroup util
* \brief A class which wraps a file stream so that it is automatically opened
*        and closed. 
* \author Josh Lurz
*/
class AutoOutputFile {
public:
    /*! \brief Open an output file with a name found from the Configuration.
    * \details Checks the Configuration for a variable with the given name. If
    *          it is not found, the given default name is used. That name is
    *          then used to open an automatically closing output file.
    * \param aConfVariableName Name of the configuration variable that stores
    *        the file name.
    * \param aDefaultName Filename to use if the variable is not found.
    */
    AutoOutputFile( const std::string& aConfVariableName, const std::string& aDefaultName ){
        const Configuration* conf = Configuration::getInstance();
        const std::string fileName = conf->getFile( aConfVariableName, aDefaultName );
        mWrappedFile.open( fileName.c_str(), std::ios::out );
        util::checkIsOpen( mWrappedFile, fileName );
    }
    
    /*! \brief Open an output file with the given name.
    * \details Opens an automatically closing output file with a given filename.
    * \param aFileName Name of the file to open.
    * \todo This interface is dangerous because forgetting an argument calls a
    *       different function.
    */
    explicit AutoOutputFile( const std::string& aFileName ){
        mWrappedFile.open( aFileName.c_str(), std::ios::out );
        util::checkIsOpen( mWrappedFile, aFileName );
    }

    /*! \brief Destructor which closes the internal file stream.*/
    ~AutoOutputFile(){
        mWrappedFile.close();
    }

    /*! \brief Write a value of type T to the output stream.
    * \param aValue Value to write.
    * \return The output stream for chaining.
    */
    template<class T>
    std::ostream& operator<<( T& aValue ){
        return mWrappedFile << aValue;
    }

    /*! \brief Dereference operator which returns the internal file stream.
    * \return The internal file stream.
    */
    std::ofstream& operator*(){
        return mWrappedFile;
    }
protected:
    //! The wrapped file stream.
    std::ofstream mWrappedFile;
};

#endif // _AUTO_OUTPUT_FILE_H_
