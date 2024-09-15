# run ~/dev/cawk/build/main -f %s
# expected 12345
# expected 1 2 3 4 5
# expected  
BEGIN {
    print (1) (2) (3) (4) (5)
    print (1), (2), (3), (4), (5)
}