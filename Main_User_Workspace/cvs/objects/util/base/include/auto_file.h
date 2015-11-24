#ifndef _AUTO_OUTPUT_FILE_H_
#define _AUTO_OUTPUT_FILE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file auto_file.h  
* \ingroup util
* \brief Header file for the AutoOutputFile class.
* \author Josh Lurz
*/

#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/device/null.hpp>
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
    * \param aShouldWriteOverride If we should not write the file even if the user
    *                             specified they want it, i.e. when target finding.
    */
    AutoOutputFile( const std::string& aConfVariableName,
                    const std::string& aDefaultName,
                    const bool aShouldWriteOverride = true )
        :mShouldWrite( aShouldWriteOverride && Configuration::getInstance()->shouldWriteFile( aConfVariableName ) )
    {
        const Configuration* conf = Configuration::getInstance();
        if( mShouldWrite ) {
            std::string fileName = conf->getFile( aConfVariableName, aDefaultName );
            if( conf->shouldAppendScnToFile( aConfVariableName ) ) {
                fileName = util::appendScenarioToFileName( fileName );
            }
            boost::iostreams::file_sink fileBuffer( fileName );
            mWrappedFile.push( fileBuffer );
            util::checkIsOpen( fileBuffer, fileName );
        }
        else {
            mWrappedFile.push( boost::iostreams::null_sink() );
        }
    }
    
    /*! \brief Open an output file with the given name.
    * \details Opens an automatically closing output file with a given filename.
    * \param aFileName Name of the file to open.
    * \todo This interface is dangerous because forgetting an argument calls a
    *       different function.
    */
    explicit AutoOutputFile( const std::string& aFileName )
        :mShouldWrite( true )
    {
        boost::iostreams::file_sink fileBuffer( aFileName );
        mWrappedFile.push( fileBuffer );
        util::checkIsOpen( fileBuffer, aFileName );
    }

    /*! \brief Destructor which closes the internal file stream.*/
    ~AutoOutputFile(){
        close( mWrappedFile );
    }

    /*!
     * \brief Get the flag if this file should be written.
     * \return True if this file is being written and false if the output is being ignored.
     */
    bool shouldWrite() const {
        return mShouldWrite;
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
    std::ostream& operator*(){
        return mWrappedFile;
    }
protected:
    //! The wrapped file/null stream.
    boost::iostreams::filtering_ostream mWrappedFile;

    //! The flag if this file was not to be written
    const bool mShouldWrite;
};

#endif // _AUTO_OUTPUT_FILE_H_
