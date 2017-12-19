#include <string>
#include <limits>

using std::string;

namespace MinimalWindowSubstring {

class Solution {
  public:
    string minWindow(string s, string t) {
        int table[255] = {0}, rest = 0;
        for (auto c : t) {
            ++table[int(c)];
            ++rest;
        }
        int head = 0, rear = std::numeric_limits<int>::max();
        for (int fast = 0, slow = 0; fast < s.size(); ++fast) {
            if (table[int(s[fast])] > 0) {
                --rest;
            }
            --table[int(s[fast])];
            if (rest == 0) {
                for (; slow < fast and table[int(s[slow])] < 0; ++slow) {
                    ++table[int(s[slow])];
                }
                if (fast - slow < rear - head) {
                    head = slow;
                    rear = fast;
                }
                if (slow < s.size()) {
                    ++table[int(s[slow])];
                    ++rest;
                }
            }
        }
        if (rear == std::numeric_limits<int>::max()) {
            return "";
        }
        return s.substr(head, rear - head + 1);
    }
};

} // namespace MinimalWindowSubstring