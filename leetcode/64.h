// Given a m x n grid filled with non-negative numbers, find a path from top
// left to bottom right which minimizes the sum of all numbers along its path.

// Note: You can only move either down or right at any point in time.

#include <vector>

using namespace std;

namespace MinimumPathSum {

class Solution {
  public:
    int minPathSum(vector<vector<int>> &grid) {
        for (int i = 1; i < grid.size(); ++i) {
            grid[i][0] += grid[i - 1][0];
        }
        for (int i = 1; i < grid.front().size(); ++i) {
            grid[0][i] += grid[0][i - 1];
        }
        for (int i = 1; i < grid.size(); ++i) {
            for (int j = 1; j < grid.front().size(); ++j) {
                grid[i][j] += min(grid[i - 1][j], grid[i][j - 1]);
            }
        }
        return grid.back().back();
    }
};
}