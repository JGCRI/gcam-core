#ifndef _GHG_MAC_H_
#define _GHG_MAC_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_mac.h
* \ingroup objects
* \brief The ghg_mac class header file.
* \author Nick Fernandez
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <memory>

class Curve;
class Tabs;


/*! 
* \ingroup objects
* \brief This class represents a Marginal Abatement Curve for a GHG.
* \author Nick Fernandez
*/
class GhgMAC {
public:
	GhgMAC();
	~GhgMAC();
    GhgMAC( const GhgMAC& other );
    GhgMAC& operator=( const GhgMAC& other );
    GhgMAC* clone() const;
    const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
	void XMLParse( const xercesc::DOMNode* node );
	double findReduction( const std::string& region, const int period );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
private:
    void copy( const GhgMAC& other );
    static const std::string XML_NAME; //!< node name for toXML methods
    std::auto_ptr<Curve> macCurve; //!< The underlying Curve
};

#endif // _GHG_MAC_H_
