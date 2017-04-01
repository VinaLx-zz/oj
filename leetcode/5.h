// Given a string s, find the longest palindromic substring in s. You may assume that the maximum length of s is 1000.
//
// Example:
//
// Input: "babad"
// Output: "bab"
//
// Input: "cbbd"
// Output: "bb"

#include <string>

using std::string;

namespace longest_palindrome_substring {

class Solution {
  public:
    string longestPalindrome(string s) {
        if (s.empty() or s.size() == 1) {
            return s;
        }
        string result;
        result += s[0];
        for (int i = 0; i < s.size(); ++i) {
            int j = i - 1, k = i + 1;
            for (; j >= 0 and k < s.size() and s[j] == s[k]; --j, ++k)
                continue;
            ++j;
            --k;
            if (k - j + 1 > result.size()) {
                result = s.substr(j, k - j + 1);
            }
            for (j = i, k = i + 1; j >= 0 and k < s.size() and s[j] == s[k];
                 --j, ++k)
                continue;
            ++j;
            --k;
            if (k - j + 1 > result.size()) {
                result = s.substr(j, k - j + 1);
            }
        }
        return result;
    }
};

}  // namespace longest_palindrome_substring
