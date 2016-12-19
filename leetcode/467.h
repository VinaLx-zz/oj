// Consider the string s to be the infinite wraparound string of
// "abcdefghijklmnopqrstuvwxyz", so s will look like this:
// "...zabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcd....".

// Now we have another string p. Your job is to find out how many unique
// non-empty substrings of p are present in s. In particular, your input is the
// string p and you need to output the number of different non-empty substrings
// of p in the string s.

// Note: p consists of only lowercase English letters and the size of p might be
// over 10000.

#include <vector>
#include <string>

using namespace std;

namespace UniqueSubstringsInWraparoundString {


// substring matching and stores the result
class Solution {
  public:
    int findSubstringInWraproundString(string p) {
        vector<int> count(26, 0);
        for (int i = 0; i < p.size();) {
            char start = p[i], current = start + 1;
            int total = 1, j = i + 1;
            for (; j < p.size(); ++j) {
                if (current > 'z') {
                    current = 'a';
                }
                if (p[j] == current) {
                    ++total;
                    ++current;
                } else {
                    break;
                }
            }
            i = j;
            for (current = start; total > 0; --total) {
                if (total > count[current - 'a']) {
                    count[current - 'a'] = total;
                } else {
                    break;
                }
                ++current;
                if (current > 'z') {
                    current = 'a';
                }
                if (current == start) {
                    break;
                }
            }
        }
        int sum = 0;
        for (auto c : count) {
            sum += c;
        }
        return sum;
    }
};

}