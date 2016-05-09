#ifndef UTIL_HH_
#define UTIL_HH_

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

      
