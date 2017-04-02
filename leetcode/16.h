// Given an array S of n integers, find three integers in S such that the sum is
// closest to a given number, target. Return the sum of the three integers. You
// may assume that each input would have exactly one solution.
//
// For example, given array S = {-1 2 1 -4}, and target = 1.
//
// The sum that is closest to the target is 2. (-1 + 2 + 1 = 2).

#include <cstdlib>
#include <vector>

using std::vector;
using std::abs;

namespace three_sum_closest {

class Solution {
  public:
    int threeSumClosest(vector<int>& nums, int target) {
        sort(nums.begin(), nums.end());
        int result = nums[0] + nums[1] + nums[2], diff = abs(target - result);
        for (int i = 0; i < nums.size(); ++i) {
            int a = nums[i];
            for (int j = i + 1, k = nums.size() - 1; j < k;) {
                int b = nums[j], c = nums[k], s = a + b + c,
                    sdiff = abs(target - s);
                if (sdiff < diff) {
                    diff = sdiff;
                    result = s;
                }
                if (s < target) {
                    ++j;
                } else if (s > target) {
                    --k;
                } else {
                    return result;
                }
            }
        }
        return result;
    }
};

}  // namespace three_sum_closest
