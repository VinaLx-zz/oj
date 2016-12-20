// Given a triangle, find the minimum path sum from top to bottom. Each step you
// may move to adjacent numbers on the row below.

// For example, given the following triangle

// [
//      [2],
//     [3,4],
//    [6,5,7],
//   [4,1,8,3]
// ]

// The minimum path sum from top to bottom is 11 (i.e., 2 + 3 + 5 + 1 = 11).

#include <vector>

using namespace std;

namespace Triangle {

// simple dp with O(n) extra space
// update the dp array from right to left=
class Solution {
  public:
    int minimumTotal(vector<vector<int>> &triangle) {
        vector<int> dp(triangle.size(), 0);
        dp[0] = triangle[0][0];
        for (int i = 1; i < triangle.size(); ++i) {
            dp[i] = dp[i - 1] + triangle[i][i];
            for (int j = i - 1; j > 0; --j) {
                dp[j] = min(dp[j], dp[j - 1]) + triangle[i][j];
            }
            dp[0] += triangle[i][0];
        }
        int m = dp.front();
        for (int i = 1; i < dp.size(); ++i) {
            m = min(m, dp[i]);
        }
        return m;
    }
};

}