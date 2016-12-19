// Given a set of candidate numbers (C) (without duplicates) and a target number (T),
// find all unique combinations in C where the candidate numbers sums to T.
// The same repeated number may be chosen from C unlimited number of times.
// Note:

//     All numbers (including target) will be positive integers.
//     The solution set must not contain duplicate combinations.
// For example, given candidate set [2, 3, 6, 7] and target 7,
// A solution set is:
// [
//   [7],
//   [2, 2, 3]
// ]
#include <vector>

using std::vector;

namespace combinationSum {

// naive dfs and backtrack
class Solution {
public:
    vector<vector<int>> combinationSum(vector<int>& candidates, int target) {
        vector<vector<int>> result;
        vector<int> current;
        dfs(0, candidates, target, current, 0, result);
        return result;
    }
    
    void dfs(
			size_t idx, const vector<int>& candidates, int target,
			vector<int>& current, int sum, vector<vector<int>>& result) {
        if (sum > target) {
            return;
        }
        if (sum == target) {
            result.push_back(current);
            return;
        }
        for (int i = idx; i < candidates.size(); ++i) {
            int num = candidates[i];
            current.push_back(num);
            dfs(i, candidates, target, current, sum + num, result);
            current.pop_back();
        }
    }
};

// dp solution takes about O(n^3) time, do even worse than the naive dfs

} // namespace CombinationSum
