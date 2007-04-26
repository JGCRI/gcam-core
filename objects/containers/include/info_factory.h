#ifndef _INFO_FACTORY_H_
#define _INFO_FACTORY_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file info_factory.h
* \ingroup objects
* \brief The InfoFactory class header file.
* \author Josh Lurz
*/

class IInfo;

/*! \brief A factory to create Info objects.
* \details The factory wraps the creation of Info objects so that other classes
*          are not aware of the underlying and complicated Info class. This
*          class should always be used to create Info objects instead of the
*          direct constructor.
* \author Josh Lurz
*/
class InfoFactory {
public:
    static IInfo* constructInfo( const IInfo* aParent, const std::string& aOwnerName );
};

#endif // _INFO_FACTORY_H_
