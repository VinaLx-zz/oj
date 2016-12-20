//  A message containing letters from A-Z is being encoded to numbers using the
//  following mapping:

// 'A' -> 1
// 'B' -> 2
// ...
// 'Z' -> 26

// Given an encoded message containing digits, determine the total number of
// ways to decode it.

// For example,
// Given encoded message "12", it could be decoded as "AB" (1 2) or "L" (12).

// The number of ways decoding "12" is 2.

#include <string>

using std::string;

#define ISVALID(a, b) (((a) == '1' or (a) == '2' and (b) <= '6') and (b) > '0')

namespace DecodeWays {

// spend time handling edge cases
// except for that, it's a simple 1 dimensional dp
class Solution {
  public:
    int numDecodings(string s) {
        if (s.empty() or s[0] == '0') {
            return 0;
        }
        int x = 1, y = 1;
        for (int i = 1; i < s.size(); ++i) {
            int t;
            if (ISVALID(s[i - 1], s[i])) {
                t = x + y;
            } else if (s[i] == '0') {
                if (s[i - 1] > '2' or s[i - 1] == '0') {
                    return 0;
                }
                t = x;
            } else {
                t = y;
            }
            x = y;
            y = t;
        }
        return y;
    }
};

}