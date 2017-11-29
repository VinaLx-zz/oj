#include <algorithm>
#include <string>
#include <vector>

using std::min;
using std::string;
using std::vector;

namespace PalindromePartitioning2 {

class Solution {
  public:
    int minCut(string s) {
        int n = s.size();
        vector<bool> dp(n * n, false);
        vector<int> res(n);
        for (int i = 0; i < n; ++i) {
            res[i] = i;
            for (int j = 0; j <= i; ++j) {
                if (s[i] == s[j] and (i - j < 2 or dp[n * (i - 1) + j + 1])) {
                    dp[n * i + j] = true;
                    if (j == 0) {
                        res[i] = 0;
                    } else {
                        res[i] = min(res[i], res[j - 1] + 1);
                    }
                }
            }
        }
        return res.back();
    }
};

} // namespace PalindromePartitioning2