# run ~/dev/cawk/build/main -f %s
# output
# 12345
# 1 2 3 4 5
#
# output 
BEGIN {
    print (1) (2) (3) (4) (5)
    print (1), (2), (3), (4), (5)
}