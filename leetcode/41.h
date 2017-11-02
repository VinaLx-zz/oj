#include <vector>

using std::swap;
using std::vector;

namespace FirstMissingPositive {

class Solution {
  public:
    int firstMissingPositive(vector<int> &nums) {
        for (int i = 0; i < nums.size();) {
            if (i == nums[i] - 1 || nums[i] <= 0 || nums[i] - 1 >= nums.size() ||
                nums[i] == nums[nums[i] - 1]) {
                ++i;
                continue;
            }
            swap(nums[i], nums[nums[i] - 1]);
        }
        int i = 0;
        for (; i < nums.size(); ++i) {
            if (nums[i] != i + 1) {
                return i + 1;
            }
        }
        return i + 1;
    }
};

} // namespace FirstMissingPositive