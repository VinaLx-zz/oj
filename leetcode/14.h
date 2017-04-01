// Write a function to find the longest common prefix string amongst an array of strings.

#include <string>
#include <vector>

using std::string;
using std::vector;

namespace longest_common_prefix {

class Solution {
  public:
    string longestCommonPrefix(vector<string>& strs) {
        if (strs.empty()) {
            return "";
        }
        if (strs.size() == 1) {
            return strs.front();
        }
        string result;
        result.reserve(strs.front().size());
        for (int i = 0; i < strs[0].size(); ++i) {
            char target = strs[0][i];
            for (auto& s : strs) {
                if (i >= s.size() or s[i] != target) {
                    goto End;
                }
            }
            result += target;
        }
    End:
        return result;
    }
};
}  // namespace longest_common_prefix
