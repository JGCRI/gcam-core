#ifndef _STORAGE_TABLE_H_
#define _STORAGE_TABLE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
    This software, which is provided in confidence, was prepared by employees
    of Pacific Northwest National Laboratory operated by Battelle Memorial
    Institute. Battelle has certain unperfected rights in the software
    which should not be copied or otherwise disseminated outside your
    organization without the express written authorization from Battelle. All rights to
    the software are reserved by Battelle.  Battelle makes no warranty,
    express or implied, and assumes no liability or responsibility for the 
    use of this software.
*/

/*! 
* \file storage_table.h
* \ingroup Objects
* \brief StorageTable class header file.
*
*  Detailed description.
*
* \author Josh Lurz
*/

#include <string>
#include <vector>

/*! 
* \ingroup Objects
* \brief A datastructure which stores in a row column format which is referenced by the column and row names.
* \details This is a sparse table as all columns do not have to contain the same rows. SWAP. For this reason the columns
* can be iterated by the rows cannot.
* \author Josh Lurz
* \todo Fix handling of total row so that a consumer of this class must request it.
*/

class StorageTable {
public:
    StorageTable();
    void clear();
    bool isEmpty() const;
    void addColumn( const std::string& aCol );
    void addToType( const int aRow, const std::string& aCol, const double aValue );
    void addToType( const std::string& aRow, const std::string& aCol, const double aValue );
    void setType( const std::string& aRow, const std::string& aCol, const double aValue ); 
    double getValue( const std::string& aRow, const std::string& aCol ) const;
    const std::vector<std::string> getRowLabels() const;
    const std::vector<std::string> getColLabels() const;
private:
    int getRowIndex( const std::string& aRow ) const;
    const static int NO_ITEM_FOUND = -1;
    std::vector<std::string> mColLabels;
    //! Structure for each Item
    struct Item {
        explicit Item( const std::string& aLabel );
        std::string label;
        double value;
    };

    //! Structure for each Column.
    struct Row {
        explicit Row( const std::string& aLabel );
        int getColIndex( const std::string& aCol ) const;
        std::string label;
        std::vector<Item> data;
        double total;
    };
    //! Structure for the internal storage.
    struct InternalTable {
        std::string label;
        std::vector<Row> rows;
    };
    InternalTable mInternalTable; //!< The internal storage.
};

#endif // _STORAGE_TABLE_H_

