# Test 1
Run this against known good source files and ensure no lexical errors are detected.
* This project's own source files might be a good starting point since they compile correctly
* May want both a single file interface and a directory interface

# Test 2
1. Output results of Test 1 to individual files
2. Use `cat` and `tr` commands to generate new line stripped files in preparation for diff
3. Have test create files from token lists generated during Test 1
4. Use `diff` command ignoring case and whitespace to compare results of #2 and #3 and ensure they match