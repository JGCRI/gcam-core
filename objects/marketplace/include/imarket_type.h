#ifndef _IMARKET_TYPE_H_
#define _IMARKET_TYPE_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file imarket_type.h
* \ingroup Objects
* \brief The IMarketType class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

/*!
* \ingroup Objects
* \brief An interface which specifies the various types of markets.
* \note This was seperated from Market.h to reduce dependencies. 
* \author Josh Lurz
*/

class IMarketType
{
public:
    enum Type { //!< Types of new markets which can be instantiated from other parts of the model.
      NORMAL, //!< Normal Market
      CALIBRATION, //!< Calibration Market
      GHG, //!< Greenhouse Gas Market
      DEMAND //!< Demand Market
    };
};

#endif // _IMARKET_TYPE_H_
