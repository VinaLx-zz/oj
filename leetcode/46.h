// Given a collection of distinct numbers, return all possible permutations.

// For example,
// [1,2,3] have the following permutations:

// [
//   [1,2,3],
//   [1,3,2],
//   [2,1,3],
//   [2,3,1],
//   [3,1,2],
//   [3,2,1]
// ]


#include <vector>

using std::vector;

namespace Permutations {

/**
 * simple dfs
 */
class Solution {
public:
    vector<vector<int>> permute(vector<int>& nums) {
        sz = nums.size();
        result_.clear();
        used.assign(nums.size(), false);
        vector<int> current;
        dfs(nums, current);
        return result_;
    }
    
private:
    void dfs(vector<int>& nums, vector<int>& current) {
        if (current.size() == sz) {
            result_.push_back(current);
        }
        for (int i = 0; i < sz; ++i) {
            if (not used[i]) {
                current.push_back(nums[i]);
                used[i] = true;
                dfs(nums, current);
                current.pop_back();
                used[i] = false;
            }
        }
    }
    
    int sz;
    vector<bool> used;
    vector<vector<int>> result_;
};

} // namespace Permutations