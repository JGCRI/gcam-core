#ifndef UTIL_HPP_
#define UTIL_HPP_

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

#include <set>
#include <map>

template <class T, class Predicate_t>
void set_filter(std::set<T> &s, const Predicate_t &pred)
{
  typename std::set<T>::iterator it = s.begin();
  while(it != s.end()) {
    typename std::set<T>::iterator elem = it++;
    if(pred(*elem))
      s.erase(elem);
  }
}

template <class T1, class T2>
void getkeys(const std::map<T1,T2> &m, std::vector<T1> &keys)
{
  keys.clear();
  typename std::map<T1,T2>::const_iterator it=m.begin();
  for( ; it != m.end(); ++it)
    keys.push_back(it->first);
}

template <class T1, class T2>
void getkeys(const std::map<T1,T2> &m, std::set<T1> &keys)
{
  keys.clear();
  typename std::map<T1,T2>::const_iterator it=m.begin();
  typename std::set<T1>::iterator insrt = keys.begin();
  for( ; it != m.end(); ++it)
    insrt = keys.insert(insrt, it->first);
}

template <class T1>
bool subsetp(const std::set<T1> &sub, const std::set<T1> &super)
{
  // sub is a subset of super iff every element of sub is in super
  typename std::set<T1>::const_iterator si;
  
  for(si = sub.begin(); si != sub.end(); ++si)
    if(super.find(*si) == super.end())
      return false;

  return true;
}

#endif

      
