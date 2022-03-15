#ifndef _LOGGER_H_
#define _LOGGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy ( DOE ). NEITHER THE GOVERNMENT NOR THE
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
* \file logger.h
* \ingroup Objects
* \brief The Logger class header file.
* \author Josh Lurz
* \date $Date: 2007/01/11 23:52:34 $
* \version $Revision: 1.5.2.3 $
*/

#include <iosfwd>
#include <sstream>
#include "util/logger/include/ilogger.h"
#include "util/base/include/definitions.h"
#include "util/base/include/data_definition_util.h"

#if GCAM_PARALLEL_ENABLED
#include <tbb/spin_mutex.h>
#endif

// Forward definition of the Logger class.
class Logger; 
class Tabs;

/*!
* \ingroup Objects
* \brief This is an overridden streambuffer class used by the Logger class.
* 
* This is a very simple class which contains a pointer to its parent Logger.
* When the streambuf receives a character it passes it to its parent stream for processing.
*
* \author Josh Lurz
* \warning Overriding the iostream class is somewhat difficult so this class may be somewhat esoteric.
* \warning This is a write-only streambuf.
*/

class PassToParentStreamBuf: std::streambuf {
    friend class Logger;

public:
    PassToParentStreamBuf();
    int overflow( int ch );
    int underflow( int ch );
    void setParent( Logger* parentIn );
    void toDebugXML( std::ostream& out ) const;
private:
    //! A pointer to the parent logger which will receive all data. 
    Logger* mParent;
};

// Forward definition of LoggerFactory class.
class LoggerFactory;

/*! 
* \ingroup Objects
* \brief This is an abstract class which defines the interface to a Logger. 
* \details Loggers may come in many different forms, but must use the defined
*          interface. Each error message is given a priority, and the user may
*          set the level of log messages they wish to print. Loggers are
*          singletons and can only be instantiated by the LoggerFactory class.
*
* \author Josh Lurz
* \date $Date: 2007/01/11 23:52:34 $
* \version $Revision: 1.5.2.3 $
* \warning This is an abstract class and cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/

class Logger: public ILogger {

    //! Friend declaration to allow LoggerFactory to create Loggers.
    friend class LoggerFactory;
public:
    virtual ~Logger(); //!< Virtual destructor.
    virtual void open( const char[] = 0 ) = 0; //!< Pure virtual function called to begin logging.
    int receiveCharFromUnderStream( int ch ); //!< Pure virtual function called to complete the log and clean up.
    virtual void close() = 0;
    ILogger::WarningLevel setLevel( const ILogger::WarningLevel newLevel );
    bool wouldPrint(ILogger::WarningLevel aLevel) const;
    void toDebugXML( std::ostream& out, Tabs* tabs ) const;
protected:
    
    DEFINE_DATA(
        // InterpolationRule is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY( Logger ),
                
        //! Logger name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        //! Logger type
        DEFINE_VARIABLE( SIMPLE, "type", mType, std::string ),

        //! File name of the file it uses.
        DEFINE_VARIABLE( SIMPLE, "FileName", mFileName, std::string ),
        
        //! Header message to print at the beginning of the log.
        DEFINE_VARIABLE( SIMPLE, "headerMessage", mHeaderMessage, std::string ),
        
        //! Defines the minimum level of messages which should be printed.
        DEFINE_VARIABLE( SIMPLE, "minLogWarningLevel", mMinLogWarningLevel, ILogger::WarningLevel ),
        
        //! Defines the minimum level of warnings to print to the console.
        DEFINE_VARIABLE( SIMPLE, "minToScreenWarningLevel", mMinToScreenWarningLevel, ILogger::WarningLevel ),
        
        //! Defines the current warning level.
        DEFINE_VARIABLE( SIMPLE, "current-warning-level", mCurrentWarningLevel, ILogger::WarningLevel ),
        
        //! Defines whether to print the warning level.
        DEFINE_VARIABLE( SIMPLE, "printLogWarningLevel", mPrintLogWarningLevel, bool )
    )

    Logger( const std::string& aFileName = "" );
    
	//! Log a message with the given warning level.
    virtual void logCompleteMessage( const std::string& aMessage ) = 0;
    void printToScreenIfConfigured( const std::string& aMessage );
    static void parseHeader( std::string& aHeader );
    static const std::string& convertLevelToString( ILogger::WarningLevel aLevel );
private:
	 //! Buffer which contains characters waiting to be printed.
    std::stringstream mBuf;
#if GCAM_PARALLEL_ENABLED
    tbb::spin_mutex mMutex;  //<! mutex protecting mBuf
#endif

	 //! Underlying ofstream
    PassToParentStreamBuf mUnderStream;

    static const std::string getTimeString();
    static const std::string getDateString();
};

#endif // _LOGGER_H_

