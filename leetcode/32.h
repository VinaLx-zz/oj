#include <stack>
#include <string>
#include <vector>
#include <utility>
#include <algorithm>

using std::stack;
using std::string;
using std::vector;
using std::pair;
using std::max;

namespace LongestValidParenthesis {

class Solution {
  public:
    int longestValidParentheses(string s) {
        stack<int> stk;
        vector<int> dp(s.size(), 0);
        int result = 0;
        for (int i = 0; i < s.size(); ++i) {
            if (s[i] == '(') {
                stk.push(i);
            } else if (stk.empty()){
                continue;
            } else {
                int match = stk.top();
                dp[i] = dp[i - 1] + 2;
                stk.pop();
                if (match != 0) {
                    dp[i] += dp[match - 1];
                }
                result = max(result, dp[i]);
            }
        }
        return result;
    }
};

} // namespace LongestValidParenthesis