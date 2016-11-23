// Given numRows, generate the first numRows of Pascal's triangle.

// For example, given numRows = 5,
// Return

// [
//      [1],
//     [1,1],
//    [1,2,1],
//   [1,3,3,1],
//  [1,4,6,4,1]
// ]


#include <vector>

using std::vector;

namespace PascalsTriangle {

class Solution {
public:
    vector<vector<int>> generate(int numRows) {
        vector<vector<int>> result;
        if (not numRows) return result;
        result.push_back({1});
        for (int i = 2; i <= numRows; ++i) {
            vector<int> layer(i);
            layer.front() = layer.back() = 1;
            for (int j = 1; j < i - 1; ++j) {
                layer[j] = result[i-2][j-1] + result[i-2][j];
            }
            result.push_back(layer);
        }
        return result;
    }
};

} // namespace PascalsTriangle