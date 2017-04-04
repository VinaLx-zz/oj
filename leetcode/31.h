#include <algorithm>
#include <vector>

using std::reverse;
using std::vector;
using std::swap;

namespace next_permutation {
class Solution {
  public:
    void nextPermutation(vector<int>& nums) {
        if (nums.size() < 2)
            return;
        int i = nums.size() - 1, j = i - 1;
        for (; j >= 0 and nums[j] >= nums[i]; --i, --j)
            continue;
        if (j < 0) {
            reverse(begin(nums), end(nums));
            return;
        }
        int mid = i, needle = nums[j];
        for (int hi = nums.size(), lo = i;;) {
            mid = (hi + lo) / 2;
            if (mid == lo)
                break;
            if (nums[mid] <= needle)
                hi = mid;
            else if (nums[mid] > needle)
                lo = mid;
        }
        swap(nums[j], nums[mid]);
        reverse(nums.begin() + i, nums.end());
    }
};
}  // namespace next_permutation
