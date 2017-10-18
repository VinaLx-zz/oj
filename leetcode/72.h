#include <algorithm>
#include <string>
#include <vector>

using std::min;
using std::string;
using std::vector;

namespace EditDistance {
class Solution {
  public:
    int minDistance(string s1, string s2) {
        vector<vector<int>> dp(s1.size() + 1, vector<int>(s2.size() + 1, 0));
        for (int i = 0; i <= s1.size(); ++i) {
            dp[i][0] = i;
        }
        for (int i = 0; i <= s2.size(); ++i) {
            dp[0][i] = i;
        }
        for (int i = 1; i <= s1.size(); ++i) {
            for (int j = 1; j <= s2.size(); ++j) {
                int add_delete = min(dp[i][j - 1] + 1, dp[i - 1][j] + 1);
                int change = dp[i - 1][j - 1] + (s1[i - 1] == s2[j - 1] ? 0 : 1);
                dp[i][j] = min(add_delete, change);
            }
        }
        return dp[s1.size()][s2.size()];
    }
};
} // namespace EditDistance