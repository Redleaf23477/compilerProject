#ifndef YYSTYPE_H_
#define YYSTYPE_H_

#include <string>
#include <iostream>

#define YYSTYPE Node

struct Node {
    std::string str;
    Node& operator = (std::string_view sv);
};

std::ostream& operator << (std::ostream&, Node&);

#endif // YYSTYPE_H_
