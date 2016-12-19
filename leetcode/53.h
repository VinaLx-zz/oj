//  Find the contiguous subarray within an array (containing at least one
//  number) which has the largest sum.

// For example, given the array [-2,1,-3,4,-1,2,1,-5,4],
// the contiguous subarray [4,-1,2,1] has the largest sum = 6.

#include <algorithm>
#include <vector>

using namespace std;

namespace MaximumSubarray {

// dynamic programming
// Maximum sum of subarray = max(sum[i] - minSum[i])
// where minSum[i] is the minimum sum from 0 to n(n < i)
class Solution {
  public:
    int maxSubArray(vector<int> &nums) {
        int minimum = nums.front() < 0 ? nums.front() : 0,
            maximum = nums.front(), sum = maximum;
        for (int i = 1; i < nums.size(); ++i) {
            sum += nums[i];
            maximum = max(maximum, sum - minimum);
            minimum = min(minimum, sum);
        }
        return maximum;
    }
};
}