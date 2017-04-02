// Given an array S of n integers, are there elements a, b, c in S such that a +
// b + c = 0? Find all unique triplets in the array which gives the sum of zero.

#include <vector>

using std::vector;

namespace three_sum {

/**
 * O(n^2) solve, sort first and double pointer
 */
class Solution {
  public:
    vector<vector<int>> threeSum(vector<int>& nums) {
        vector<vector<int>> result;
        sort(begin(nums), end(nums));
        for (int i = 0; i < nums.size();) {
            int a = nums[i];
            for (int j = i + 1, k = nums.size() - 1; j < k;) {
                int b = nums[j], c = nums[k];
                int s = a + b + c;
                if (s < 0) {
                    ++j;
                } else if (s > 0) {
                    --k;
                } else {
                    result.push_back({a, b, c});
                    for (; ++j < k && nums[j] == b;)
                        continue;
                    for (; --k > j && nums[k] == c;)
                        continue;
                }
            }
            for (; nums[++i] == a;)
                continue;
        }
        return result;
    }
};
}  // namespace three_sum
