#include <string>
#include <vector>

using std::string;
using std::vector;

namespace DistinctSubsequences {

class Solution {
  public:
    int numDistinct(string s, string t) {
        vector<int> dp(t.size() + 1);
        dp.front() = 1;
        for (int i = 0; i < s.size(); ++i) {
            for (int j = t.size(); j >= 0; --j) {
                if (j > 0 and t[j - 1] == s[i]) {
                    dp[j] += dp[j - 1];
                }
            }
        }
        return dp[t.size()];
    }
};

} // namespace DistinctSubsequences
