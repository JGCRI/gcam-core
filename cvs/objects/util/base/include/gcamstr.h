#ifndef _GCAMSTR_H_
#define _GCAMSTR_H_
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
* \file gcamstr.h
* \ingroup Objects
* \brief An alternate string class to reduce memory.
* \details The gcamstr class uses the Pooling/Interning design pattern 
*          (via boost::flyweight) to reduce memory from redundant strings.
* \author Pralit Patel
*/

#include <string>
#include <boost/flyweight.hpp>
#include <boost/flyweight/no_locking.hpp>
#include <boost/flyweight/no_tracking.hpp>

#if DEBUG_STATE
// Note we are going to attempt to maximize performance in terms of runtime and memory
// usage.  To do this we configure gcamstr to use "no_locking" and "no_tracking".  This
// means we are susceptible to bloat in the string pool and concurrency errors.  To mitigate
// this we will add checks when running in DEBUG_STATE and trap errors.

#include <unordered_set>


// forward declare only otherwise we will run into circular #include dependencies
void checkAddStrDuringCalc(const std::string& aStr);

// In the boost::flyweights framework the Factory concept is the best place to
// customize to add our additional debug checks.  It is the object that is responsible
// for maintaining the pool of interned string values. At the moment we are only
// checking to ensure we are not inserting any new values during World.calc, which
// would be problematic due to concurrent write error.  We could add even more checks
// to detect bloat.
// Note: much of this syntax is duplicated from boost::flyweights::hash_factory, we
// have added additional debugging in the insert method
template<typename Entry,typename Key>
class DebugFactoryClass : boost::flyweights::factory_marker {
    // Allow AnalyzeStr to access the size of the contained string pool so it can
    // include it in the diagnstic report of string usage.
    friend class AnalyzeStr;
    public:
        typedef const Entry* handle_type;

        typedef DebugFactoryClass type;
        BOOST_MPL_AUX_LAMBDA_SUPPORT(
                2,DebugFactoryClass,(Entry,Key,))

        handle_type insert(const Entry& x) {
            auto ret = cont.insert(x);
            // Ideally we wouldn't even attempt to during World.calc but presently
            // there are a handful of instances where we do.  This does not seem
            // to show up as a performance bottleneck however.
            if(ret.second) {
                // errors are only issued if we are in World.calc
                // we need to put the check in a function call defined elsewhere to
                // avoid circular #include dependencies
                checkAddStrDuringCalc(x.x);
            }
            return &*ret.first;
        }

        handle_type insert(Entry&& x) {
            auto ret = cont.insert(std::move(x));
            // Ideally we wouldn't even attempt to during World.calc but presently
            // there are a handful of instances where we do.  This does not seem
            // to show up as a performance bottleneck however.
            if(ret.second) {
                // errors are only issued if we are in World.calc
                // we need to put the check in a function call defined elsewhere to
                // avoid circular #include dependencies
                checkAddStrDuringCalc(x.x);
            }
            return &*ret.first;
        }

        void erase(handle_type h) {
            for(auto it = cont.begin(); it != cont.end(); ++it) {
                handle_type curr = &*it;
                if(curr == h) {
                    cont.erase(it);
                    return;
                }
            }
        }

        static const Entry& entry(handle_type h){return *h;}

    private:
        std::unordered_set<Entry, boost::hash<Key>, std::equal_to<Key> > cont;

};

struct DebugFactory : boost::flyweights::factory_marker
{
  template<typename Entry,typename Key>
  struct apply:
    boost::mpl::apply2<
      DebugFactoryClass<
        boost::mpl::_1,boost::mpl::_2
      >,
      Entry,Key
    >
  {};
};

// the base class uses our DebugFactory instead of the default
using GCAMStrBase = boost::flyweight<std::string, DebugFactory, boost::flyweights::no_tracking, boost::flyweights::no_locking>;
#else
// the base class is configured for maximum performance
using GCAMStrBase = boost::flyweight<std::string, boost::flyweights::no_tracking, boost::flyweights::no_locking>;
#endif // DEBUG_STATE


/*!
 * \brief A string class which uses pooling to reduce memory usage and boost performance.
 * \details We rely on boost::flyweight to do all of the heavy lifting.  And in most
 *          cases gcamstr can be used as a drop in replacement for std::string.  However,
 *          we have configured gcamstr for maximum performance.  Therefore users should not
 *          attempt to create "new" strings during World.calc (having created it at least once
 *          ahead of time is fine) and avoid adding a lot of "generated" strings which may bloat
 *          the size of the pool, also slowing performance.
 */
struct gcamstr : public GCAMStrBase {
    //! Inherit constructors from the base class
    using GCAMStrBase::GCAMStrBase;

    //! Constructor to allow directly adding C-style strings
    gcamstr(const char* aStr):GCAMStrBase(std::string(aStr)) { }

    //! Operator to allow inserts via assignment
    inline gcamstr& operator=(const std::string& aStr) {
        gcamstr wrap(aStr);
        std::swap(*this, wrap);
        return *this;
    }

    //! Operator to allow inserts via assignment
    inline gcamstr& operator=(const char* aStr) {
        gcamstr wrap(aStr);
        std::swap(*this, wrap);
        return *this;
    }

    //! replicate std::string::empty() method for convenience
    bool empty() const {
        return get().empty();
    }
};

/*!
 * \brief A comparison operator allowing gcamstr to be sorted or included
 *        in map or set objects.
 * \param aLHS The left hand side gcamstr object to compare.
 * \param aRHS The right hand side gcamstr object to compare.
 * \warning This method is doing the comparison based of memory address
 *          for performance reasons.  If a real lexical ordering is required
 *          users should supply custom comparator which compares gcamstr.get()
 *          instead.
 */
inline bool operator<(const gcamstr& aLHS, const gcamstr& aRHS) {
    return reinterpret_cast<const GCAMStrBase&>(aLHS) <
        reinterpret_cast<const GCAMStrBase&>(aRHS);
}

/*!
 * \brief A custom equality operator of gcamstr for speed.
 * \param aLHS The left hand side gcamstr object to compare.
 * \param aRHS The right hand side gcamstr object to compare.
 * \note This method is doing the comparison based of memory address
 *       for performance reasons.
 */
inline bool operator==(const gcamstr& aLHS, const gcamstr& aRHS) {
    return reinterpret_cast<const GCAMStrBase&>(aLHS) ==
        reinterpret_cast<const GCAMStrBase&>(aRHS);
}

/*!
 * \brief A custom in-equality operator of gcamstr for speed.
 * \param aLHS The left hand side gcamstr object to compare.
 * \param aRHS The right hand side gcamstr object to compare.
 * \note This method is doing the comparison based of memory address
 *       for performance reasons.
 */
inline bool operator!=(const gcamstr& aLHS, const gcamstr& aRHS) {
    return reinterpret_cast<const GCAMStrBase&>(aLHS) !=
        reinterpret_cast<const GCAMStrBase&>(aRHS);
}

#endif // _GCAMSTR_H_
