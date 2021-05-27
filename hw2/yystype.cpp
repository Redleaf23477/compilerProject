#include <string>
#include <iostream>
#include "yystype.h"

Node& Node::operator = (std::string_view sv) {
    str = sv;
    return *this;
}

std::ostream& operator << (std::ostream& out, Node& nd) {
    out << nd.str;
    return out;
}
