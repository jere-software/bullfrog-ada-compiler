# Test 1
Run this against known good source files and ensure no lexical errors are detected.
* This project's own source files might be a good starting point since they compile correctly
* May want both a single file interface and a directory interface

# Test 2
1. Have test create files from token lists generated during Test 1
   * add in line breaks and spaces as indicated by token locations
2. Use `diff` command ignoring case and whitespace to compare results and ensure they match
3. Make it work with a location specified by command line
   * Tested against gnoga
   * Tested against simple_components
   * Tested against mathpaqs