# run ~/dev/cawk/build/main -f %s
# expected 0x1.5p+5
# expected 

BEGIN {
	printf("%a\n", 42)
}