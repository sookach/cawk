BEGIN {
    print "#pragma once" > "../include/cawk_code.h"
    print "#include <array>" > "../include/cawk_code.h"
    print "constexpr std::array cawk_code{" > "../include/cawk_code.h"
}

NR != 1 { print "," > "../include/cawk_code.h" }
{ print "R\"(" $0 ")\"" > "../include/cawk_code.h" }

END { print "};" > "../include/cawk_code.h" }