// The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)
//
// P   A   H   N
// A P L S I I G
// Y   I   R
//
// And then read line by line: "PAHNAPLSIIGYIR"
//
// Write the code that will take a string and make this conversion given a number of rows:
//
// string convert(string text, int nRows);
//
// convert("PAYPALISHIRING", 3) should return "PAHNAPLSIIGYIR".

#include <string>
#include <vector>
#include <numeric>

using std::string;
using std::vector;
using std::accumulate;

namespace zigzag_conversion {

class Solution {
public:
    string convert(string s, int numRows) {
        if (numRows == 1) {
            return s;
        }
        vector<string> v(numRows);
        for (int i = 0, j = 0, down = 1; i < s.size(); ++i) {
            v[j].push_back(s[i]);
            if (j == numRows - 1) {
                --j;
                down = -1;
            } else if (j == 0) {
                ++j;
                down = 1;
            } else {
                j += down;
            }
        }
        return accumulate(v.begin(), v.end(), string(""));
    }
};

} // namespace zigzag_conversion
