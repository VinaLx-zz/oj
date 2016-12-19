// In the computer world, use restricted resource you have to generate maximum
// benefit is what we always want to pursue.

// For now, suppose you are a dominator of m 0s and n 1s respectively. On the
// other hand, there is an array with strings consisting of only 0s and 1s.

// Now your task is to find the maximum number of strings that you can form with
// given m 0s and n 1s. Each 0 and 1 can be used at most once.

// Note:

//     The given numbers of 0s and 1s will both not exceed 100
//     The size of given string array won't exceed 600.

#include <vector>
#include <string>
#include <utility>

using namespace std;

namespace OnesAndZeros {

// simple dp, the trick is walking the dp matrix backwards to
// avoid duplication
class Solution {
public:
    pair<int, int> count(const string& s) {
        pair<int, int> result{0, 0};
        for (auto c : s) {
            if (c == '0') {
                ++result.first;
            } else {
                ++result.second;
            }
        }
        return result;
    }
    int findMaxForm(vector<string>& strs, int m, int n) {
        int dp[101][101] = {0};
        for (const auto& s : strs) {
            pair<int, int> p = count(s);
            int zeros = p.first, ones = p.second;
            for (int i = m; i >= zeros; --i) {
                for (int j = n; j >= ones; --j) {
                    dp[i][j] = max(dp[i-zeros][j-ones] + 1, dp[i][j]);
                }
            }
        }
        return dp[m][n];
    }
};

}